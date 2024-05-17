module ModuleExposingIsExplicit exposing (review)

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


{-| Enforces that all module exposes specify the member names
and don't use the `exposing (..)` syntax to expose everything.

Modules can have helpers for implementation details which the users of this module should not have to know about.
Therefore, the API should be explicitly defined and as small as possible.


### reported

    module A exposing (..)

    a =
        ""


### not reported

    module A exposing (a)

    a =
        ""

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
    { exposingEverythingByModulePath :
        FastDict.Dict
            String
            { choiceTypeNames : Set String
            , simpleNames : Set String
            , moduleName : Elm.Syntax.ModuleName.ModuleName
            , dotsRange : Elm.Syntax.Range.Range
            }
    }


knowledgeMerge : Knowledge -> Knowledge -> Knowledge
knowledgeMerge a b =
    { exposingEverythingByModulePath =
        FastDict.union a.exposingEverythingByModulePath b.exposingEverythingByModulePath
    }


moduleDataToKnowledge : { data_ | path : String, syntax : Elm.Syntax.File.File } -> Knowledge
moduleDataToKnowledge moduleData =
    let
        moduleName : Elm.Syntax.ModuleName.ModuleName
        moduleName =
            moduleData.syntax.moduleDefinition |> Review.moduleHeaderNameNode |> Elm.Syntax.Node.value
    in
    { exposingEverythingByModulePath =
        case moduleData.syntax.moduleDefinition |> Review.moduleHeaderExposingNode |> Elm.Syntax.Node.value of
            Elm.Syntax.Exposing.Explicit _ ->
                FastDict.empty

            Elm.Syntax.Exposing.All dotsRange ->
                let
                    exposes =
                        moduleData.syntax |> moduleMembersThatCanBeExposed
                in
                FastDict.singleton moduleData.path
                    { moduleName = moduleName
                    , dotsRange = dotsRange
                    , choiceTypeNames = exposes.choiceTypeNames
                    , simpleNames = exposes.simpleNames
                    }
    }


report : Knowledge -> List Review.Error
report knowledge =
    knowledge.exposingEverythingByModulePath
        |> FastDict.toList
        |> List.map
            (\( modulePath, exposingEverythingInModule ) ->
                { path = modulePath
                , message = "module " ++ (exposingEverythingInModule.moduleName |> String.join ".") ++ " exposes everything, not explicit"
                , details =
                    [ "Modules can have helpers for implementation details which the users of this module should not have to know about. Therefore, the API should be explicitly defined and as small as possible."
                    , "Try to explicitly pick the members you want to make public and put them where the .. is currently. To start with all the currently exposed members, accept the provided fix and then remove the undesired ones."
                    ]
                , range = exposingEverythingInModule.dotsRange
                , fix =
                    [ { path = modulePath
                      , edits =
                            [ Review.replaceRange exposingEverythingInModule.dotsRange
                                ({ choiceTypeNames = exposingEverythingInModule.choiceTypeNames
                                 , simpleNames = exposingEverythingInModule.simpleNames
                                 }
                                    |> explicitExposesToString
                                )
                            ]
                      }
                    ]
                }
            )


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

                    Elm.Syntax.Declaration.InfixDeclaration symbol ->
                        { soFar
                            | simpleNames =
                                soFar.simpleNames |> Set.insert (symbol.function |> Elm.Syntax.Node.value)
                        }

                    -- invalid elm
                    Elm.Syntax.Declaration.Destructuring _ _ ->
                        soFar
            )
            { choiceTypeNames = Set.empty, simpleNames = Set.empty }


explicitExposesToString :
    { choiceTypeNames : Set String
    , simpleNames : Set String
    }
    -> String
explicitExposesToString explicitExposes =
    Set.union
        explicitExposes.simpleNames
        (explicitExposes.choiceTypeNames
            |> Set.map (\choiceTypeName -> choiceTypeName ++ "(..)")
        )
        |> Set.toList
        |> String.join ", "
