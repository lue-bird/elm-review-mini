module ImportExposingIsExplicit exposing (review)

{-|

@docs review

-}

import Dict
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import FastDict
import FastSet
import Review


{-| Enforce that all imports with exposes specify the member names
and don't use the `exposing (..)` syntax to expose everything.

When you import everything from a module without explicitly listing what you actually need,
it becomes harder to know where a reference comes from and which "domain" it belongs to for example.
Try using qualified imports like ModuleName.member or if that becomes too inconvenient, explicitly list what exposes you want to import for use without qualification.


### reported

    import Test exposing (..)

    test =
        test "example"
            (\() -> Expect.pass)


### not reported

    import Test exposing (test)

    test =
        test "example"
            (\() -> Expect.pass)

-}
review : Review.Review
review =
    Review.create
        { inspect =
            [ Review.inspectModule moduleDataToKnowledge
            ]
        , knowledgeMerge = knowledgeMerge
        , report = report
        }


type alias Knowledge =
    { exposesByModuleName :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            { choiceTypesExposingVariants : FastSet.Set String
            , simpleNames : FastSet.Set String
            }
    , importsExposingEverything :
        List
            { containingModulePath : String
            , moduleName : Elm.Syntax.ModuleName.ModuleName
            , dotsRange : Elm.Syntax.Range.Range
            }
    }


knowledgeMerge : Knowledge -> Knowledge -> Knowledge
knowledgeMerge a b =
    { exposesByModuleName = FastDict.union a.exposesByModuleName b.exposesByModuleName
    , importsExposingEverything = a.importsExposingEverything ++ b.importsExposingEverything
    }


moduleDataToKnowledge : { data_ | path : String, syntax : Elm.Syntax.File.File } -> Knowledge
moduleDataToKnowledge moduleData =
    let
        moduleName : Elm.Syntax.ModuleName.ModuleName
        moduleName =
            moduleData.syntax.moduleDefinition |> Elm.Syntax.Node.value |> Elm.Syntax.Module.moduleName
    in
    { exposesByModuleName =
        let
            exposes : { simpleNames : FastSet.Set String, typesExposingVariants : FastSet.Set String }
            exposes =
                case moduleData.syntax.moduleDefinition |> Elm.Syntax.Node.value |> Elm.Syntax.Module.exposingList of
                    Elm.Syntax.Exposing.Explicit topLevelExposeList ->
                        topLevelExposeList |> Review.topLevelExposeListToExposes

                    Elm.Syntax.Exposing.All _ ->
                        moduleData.syntax |> moduleMembersAsExplicitExposing
        in
        FastDict.singleton moduleName
            { choiceTypesExposingVariants = exposes.typesExposingVariants
            , simpleNames = exposes.simpleNames
            }
    , importsExposingEverything =
        moduleData.syntax.imports
            |> List.filterMap
                (\(Elm.Syntax.Node.Node _ import_) ->
                    case import_.exposingList |> Maybe.map Elm.Syntax.Node.value of
                        Nothing ->
                            Nothing

                        Just (Elm.Syntax.Exposing.Explicit _) ->
                            Nothing

                        Just (Elm.Syntax.Exposing.All dotsRange) ->
                            { containingModulePath = moduleData.path
                            , moduleName = import_.moduleName |> Elm.Syntax.Node.value
                            , dotsRange = dotsRange
                            }
                                |> Just
                )
    }


moduleMembersAsExplicitExposing :
    Elm.Syntax.File.File
    ->
        { simpleNames : FastSet.Set String
        , typesExposingVariants : FastSet.Set String
        }
moduleMembersAsExplicitExposing syntaxFile =
    syntaxFile.declarations
        |> List.foldl
            (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                case declaration of
                    Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                        { soFar
                            | typesExposingVariants =
                                soFar.typesExposingVariants |> FastSet.insert (choiceTypeDeclaration.name |> Elm.Syntax.Node.value)
                        }

                    Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                        { soFar
                            | simpleNames =
                                soFar.simpleNames
                                    |> FastSet.insert
                                        (valueOrFunctionDeclaration.declaration
                                            |> Elm.Syntax.Node.value
                                            |> .name
                                            |> Elm.Syntax.Node.value
                                        )
                        }

                    Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                        { soFar
                            | simpleNames =
                                soFar.simpleNames |> FastSet.insert (typeAliasDeclaration.name |> Elm.Syntax.Node.value)
                        }

                    Elm.Syntax.Declaration.PortDeclaration signature ->
                        { soFar
                            | simpleNames =
                                soFar.simpleNames |> FastSet.insert (signature.name |> Elm.Syntax.Node.value)
                        }

                    -- not supported
                    Elm.Syntax.Declaration.InfixDeclaration _ ->
                        soFar

                    Elm.Syntax.Declaration.Destructuring _ _ ->
                        soFar
            )
            { typesExposingVariants = FastSet.empty, simpleNames = FastSet.empty }


report : Knowledge -> List Review.Error
report knowledge =
    knowledge.importsExposingEverything
        |> List.filterMap
            (\exposingEverythingInModule ->
                case knowledge.exposesByModuleName |> FastDict.get exposingEverythingInModule.moduleName of
                    Nothing ->
                        Nothing

                    Just exposesOfModule ->
                        Just
                            { path = exposingEverythingInModule.containingModulePath
                            , message = "import " ++ (exposingEverythingInModule.moduleName |> String.join ".") ++ " exposes everything, not explicit"
                            , details =
                                [ "When you import everything from a module without explicitly listing what you actually need, it becomes harder to know where a reference comes from and which \"domain\" it belongs to for example."
                                , "Try using qualified imports like ModuleName.member or if that becomes too inconvenient, explicitly list what exposes you want to import for use without qualification. To start by explicitly listing all of the current members, accept the automatic fix and clean up from there."
                                ]
                            , range = exposingEverythingInModule.dotsRange
                            , fix =
                                [ { path = exposingEverythingInModule.containingModulePath
                                  , edits =
                                        [ Review.replaceRange exposingEverythingInModule.dotsRange (exposesOfModule |> explicitExposesToString)
                                        ]
                                  }
                                ]
                            }
            )


explicitExposesToString :
    { choiceTypesExposingVariants : FastSet.Set String
    , simpleNames : FastSet.Set String
    }
    -> String
explicitExposesToString explicitExposes =
    FastSet.union
        explicitExposes.simpleNames
        (explicitExposes.choiceTypesExposingVariants |> FastSet.map (\choiceTypeName -> choiceTypeName ++ "(..)"))
        |> FastSet.toList
        |> String.join ", "
