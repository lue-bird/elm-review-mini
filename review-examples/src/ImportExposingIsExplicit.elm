module ImportExposingIsExplicit exposing (review)

{-|

@docs review

-}

import Dict
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing
import Elm.Syntax.File
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import FastDict
import Review exposing (Review)
import Set exposing (Set)


{-| Enforces that all imports with exposes specify the member names
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
review : Review
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
            { choiceTypesExposingVariants : Set String
            , simpleNames : Set String
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
            moduleData.syntax.moduleDefinition |> Review.moduleHeaderNameNode |> Elm.Syntax.Node.value
    in
    { exposesByModuleName =
        let
            exposes : { simpleNames : Set String, choiceTypeNames : Set String }
            exposes =
                moduleData.syntax |> moduleMembersThatCanBeExposed
        in
        FastDict.singleton moduleName
            { choiceTypesExposingVariants = exposes.choiceTypeNames
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


moduleMembersThatCanBeExposed :
    Elm.Syntax.File.File
    ->
        { simpleNames : Set String
        , choiceTypeNames : Set String
        }
moduleMembersThatCanBeExposed syntaxFile =
    syntaxFile.declarations
        |> List.foldl
            (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                case declaration of
                    Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                        { soFar
                            | choiceTypeNames =
                                soFar.choiceTypeNames |> Set.insert (choiceTypeDeclaration.name |> Elm.Syntax.Node.value)
                        }

                    Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                        { soFar
                            | simpleNames =
                                soFar.simpleNames
                                    |> Set.insert
                                        (valueOrFunctionDeclaration.declaration
                                            |> Elm.Syntax.Node.value
                                            |> .name
                                            |> Elm.Syntax.Node.value
                                        )
                        }

                    Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                        { soFar
                            | simpleNames =
                                soFar.simpleNames |> Set.insert (typeAliasDeclaration.name |> Elm.Syntax.Node.value)
                        }

                    Elm.Syntax.Declaration.PortDeclaration signature ->
                        { soFar
                            | simpleNames =
                                soFar.simpleNames |> Set.insert (signature.name |> Elm.Syntax.Node.value)
                        }

                    -- not supported
                    Elm.Syntax.Declaration.InfixDeclaration _ ->
                        soFar

                    Elm.Syntax.Declaration.Destructuring _ _ ->
                        soFar
            )
            { choiceTypeNames = Set.empty, simpleNames = Set.empty }


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
    { choiceTypesExposingVariants : Set String
    , simpleNames : Set String
    }
    -> String
explicitExposesToString explicitExposes =
    Set.union
        explicitExposes.simpleNames
        (explicitExposes.choiceTypesExposingVariants |> Set.map (\choiceTypeName -> choiceTypeName ++ "(..)"))
        |> Set.toList
        |> String.join ", "
