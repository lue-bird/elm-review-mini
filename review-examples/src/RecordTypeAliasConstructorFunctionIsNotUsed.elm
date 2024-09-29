module RecordTypeAliasConstructorFunctionIsNotUsed exposing (review)

{-|

@docs review

-}

import Declaration.LocalExtra
import Elm.Docs
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import Elm.Type
import FastDict
import FastDict.LocalExtra
import FastSet
import FastSet.LocalExtra
import Review


{-| Report using a record type alias constructor function
and suggests a fix that converts it a record.
[Read about why](https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest#why).


## examples

    type alias User =
        { name : String, age : Int }

    User "Balsa" 42

will be reported and automatically fixed:

    { name = "Balsa", age = 42 }

The same goes for cases where only some or no arguments are applied:

    map2 User
        (field "name" string)
        (field "age" int)

fixed

    map2 (\name age -> { name = name, age = age })
        (field "name" string)
        (field "age" int)

-}
review : Review.Review
review =
    Review.create
        { inspect =
            [ Review.inspectDirectDependencies directDependenciesToKnowledge
            , Review.inspectModule moduleDataToKnowledge
            ]
        , knowledgeMerge = knowledgeMerge
        , report = report
        }


type alias Knowledge =
    { moduleExposes :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            { simpleNames : FastSet.Set String
            , typesWithVariantNames : FastDict.Dict String (FastSet.Set String)
            }
    , recordTypeAliasDeclarations :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { recordFields : List String }
    , modulePossibleTypeAliasConstructorUses :
        List
            { modulePath : String
            , moduleName : Elm.Syntax.ModuleName.ModuleName
            , importsExposingAll :
                List
                    { moduleName : Elm.Syntax.ModuleName.ModuleName
                    , alias : Maybe String
                    }
            , importsExposingExplicit :
                List
                    { moduleName : Elm.Syntax.ModuleName.ModuleName
                    , alias : Maybe String
                    , simpleNames : FastSet.Set String
                    , typesExposingVariants : FastSet.Set String
                    }
            , moduleDeclaredNames : FastSet.Set String
            , possibleTypeAliasConstructorUses :
                List
                    { qualification : Elm.Syntax.ModuleName.ModuleName
                    , unqualifiedName : String
                    , referenceRange : Elm.Syntax.Range.Range
                    , bindingsInScope : FastSet.Set String
                    , directCallRange : Elm.Syntax.Range.Range
                    , directCallArguments : List { source : String, startColumn : Int }
                    }
            }
    }


directDependenciesToKnowledge :
    List { dependency_ | modules : List Elm.Docs.Module }
    -> Knowledge
directDependenciesToKnowledge =
    \dependencies ->
        let
            modules : List Elm.Docs.Module
            modules =
                dependencies |> List.concatMap .modules
        in
        { modulePossibleTypeAliasConstructorUses = []
        , moduleExposes =
            modules
                |> List.map
                    (\moduleInterface ->
                        ( moduleInterface.name |> String.split "."
                        , moduleInterface |> Review.moduleInterfaceExposes
                        )
                    )
                |> FastDict.fromList
        , recordTypeAliasDeclarations =
            modules
                |> List.concatMap
                    (\moduleInterface ->
                        let
                            moduleName : Elm.Syntax.ModuleName.ModuleName
                            moduleName =
                                moduleInterface.name |> String.split "."
                        in
                        moduleInterface.aliases
                            |> List.filterMap
                                (\alias ->
                                    case alias.tipe of
                                        Elm.Type.Record fields Nothing ->
                                            ( ( moduleName, alias.name )
                                            , { recordFields =
                                                    fields
                                                        |> List.map (\( fieldName, _ ) -> fieldName)
                                              }
                                            )
                                                |> Just

                                        _ ->
                                            Nothing
                                )
                    )
                |> FastDict.fromList
        }


knowledgeMerge : Knowledge -> Knowledge -> Knowledge
knowledgeMerge a b =
    { moduleExposes = FastDict.union a.moduleExposes b.moduleExposes
    , recordTypeAliasDeclarations =
        FastDict.union a.recordTypeAliasDeclarations
            b.recordTypeAliasDeclarations
    , modulePossibleTypeAliasConstructorUses =
        a.modulePossibleTypeAliasConstructorUses
            ++ b.modulePossibleTypeAliasConstructorUses
    }


moduleDataToKnowledge :
    { source : String, syntax : Elm.Syntax.File.File, path : String }
    -> Knowledge
moduleDataToKnowledge =
    \moduleData ->
        let
            moduleName : Elm.Syntax.ModuleName.ModuleName
            moduleName =
                moduleData.syntax.moduleDefinition
                    |> Elm.Syntax.Node.value
                    |> Elm.Syntax.Module.moduleName
        in
        { recordTypeAliasDeclarations =
            moduleData.syntax.declarations
                |> List.filterMap
                    (\(Elm.Syntax.Node.Node _ declaration) ->
                        case declaration of
                            Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                                case typeAliasDeclaration.typeAnnotation |> Elm.Syntax.Node.value of
                                    Elm.Syntax.TypeAnnotation.Record fields ->
                                        ( ( moduleName
                                          , typeAliasDeclaration.name |> Elm.Syntax.Node.value
                                          )
                                        , { recordFields =
                                                fields
                                                    |> List.map
                                                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ field, _ )) -> field)
                                          }
                                        )
                                            |> Just

                                    _ ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> FastDict.fromList
        , moduleExposes =
            FastDict.singleton moduleName
                (moduleData.syntax |> Review.moduleExposes)
        , modulePossibleTypeAliasConstructorUses =
            [ { modulePath = moduleData.path
              , moduleName = moduleName
              , moduleDeclaredNames =
                    moduleData.syntax.declarations
                        |> FastSet.LocalExtra.unionFromListMap
                            (\(Elm.Syntax.Node.Node _ declaration) ->
                                declaration |> Declaration.LocalExtra.declaredNames
                            )
              , importsExposingAll =
                    moduleData.syntax.imports
                        |> List.filterMap
                            (\(Elm.Syntax.Node.Node _ import_) ->
                                case import_.exposingList |> Maybe.map Elm.Syntax.Node.value of
                                    Nothing ->
                                        Nothing

                                    Just (Elm.Syntax.Exposing.Explicit _) ->
                                        Nothing

                                    Just (Elm.Syntax.Exposing.All _) ->
                                        { moduleName = import_.moduleName |> Elm.Syntax.Node.value
                                        , alias = import_.moduleAlias |> Maybe.map (\(Elm.Syntax.Node.Node _ aliasParts) -> aliasParts |> String.join ".")
                                        }
                                            |> Just
                            )
              , importsExposingExplicit =
                    moduleData.syntax.imports
                        |> List.filterMap
                            (\(Elm.Syntax.Node.Node _ import_) ->
                                (case import_.exposingList |> Maybe.map Elm.Syntax.Node.value of
                                    Just (Elm.Syntax.Exposing.All _) ->
                                        Nothing

                                    Nothing ->
                                        { simpleNames = FastSet.empty, typesExposingVariants = FastSet.empty }
                                            |> Just

                                    Just (Elm.Syntax.Exposing.Explicit topLevelExposeList) ->
                                        topLevelExposeList |> Review.topLevelExposeListToExposes |> Just
                                )
                                    |> Maybe.map
                                        (\exposes ->
                                            { moduleName = import_.moduleName |> Elm.Syntax.Node.value
                                            , alias = import_.moduleAlias |> Maybe.map (\(Elm.Syntax.Node.Node _ aliasParts) -> aliasParts |> String.join ".")
                                            , simpleNames = exposes.simpleNames
                                            , typesExposingVariants = exposes.typesExposingVariants
                                            }
                                        )
                            )
              , possibleTypeAliasConstructorUses =
                    moduleData.syntax.declarations
                        |> List.concatMap
                            (\(Elm.Syntax.Node.Node _ declaration) ->
                                case declaration of
                                    Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                                        valueOrFunctionDeclaration.declaration
                                            |> Elm.Syntax.Node.value
                                            |> .expression
                                            |> expressionPossibleTypeAliasConstructorUsesWithBindingsInScope
                                                (valueOrFunctionDeclaration.declaration
                                                    |> Elm.Syntax.Node.value
                                                    |> .arguments
                                                    |> List.concatMap Review.patternBindings
                                                    |> FastSet.fromList
                                                )
                                            |> List.map
                                                (\possibleTypeAliasConstructorUse ->
                                                    { qualification = possibleTypeAliasConstructorUse.qualification
                                                    , unqualifiedName = possibleTypeAliasConstructorUse.unqualifiedName
                                                    , referenceRange = possibleTypeAliasConstructorUse.referenceRange
                                                    , bindingsInScope = possibleTypeAliasConstructorUse.bindingsInScope
                                                    , directCallRange = possibleTypeAliasConstructorUse.directCallRange
                                                    , directCallArguments =
                                                        possibleTypeAliasConstructorUse.directCallArguments
                                                            |> List.map
                                                                (\directCallArgument ->
                                                                    { startColumn = directCallArgument.range.start.column
                                                                    , source = moduleData.source |> Review.sourceExtractInRange directCallArgument.range
                                                                    }
                                                                )
                                                    }
                                                )

                                    _ ->
                                        []
                            )
              }
            ]
        }


expressionPossibleTypeAliasConstructorUsesWithBindingsInScope :
    FastSet.Set String
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    ->
        List
            { qualification : Elm.Syntax.ModuleName.ModuleName
            , unqualifiedName : String
            , referenceRange : Elm.Syntax.Range.Range
            , bindingsInScope : FastSet.Set String
            , directCallRange : Elm.Syntax.Range.Range
            , directCallArguments : List { range : Elm.Syntax.Range.Range }
            }
expressionPossibleTypeAliasConstructorUsesWithBindingsInScope outerBindingsInScope =
    \expressionNode ->
        case expressionNode of
            Elm.Syntax.Node.Node referenceRange (Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName) ->
                if unqualifiedName |> isPossibleTypeAliasConstructorName then
                    [ { qualification = qualification
                      , unqualifiedName = unqualifiedName
                      , referenceRange = referenceRange
                      , bindingsInScope = outerBindingsInScope
                      , directCallRange = referenceRange
                      , directCallArguments = []
                      }
                    ]

                else
                    []

            Elm.Syntax.Node.Node directCallRange (Elm.Syntax.Expression.Application ((Elm.Syntax.Node.Node referenceRange (Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName)) :: directCallArguments)) ->
                let
                    argumentsPossibleTypeAliasConstructorUse :
                        List
                            { qualification : Elm.Syntax.ModuleName.ModuleName
                            , unqualifiedName : String
                            , referenceRange : Elm.Syntax.Range.Range
                            , bindingsInScope : FastSet.Set String
                            , directCallRange : Elm.Syntax.Range.Range
                            , directCallArguments : List { range : Elm.Syntax.Range.Range }
                            }
                    argumentsPossibleTypeAliasConstructorUse =
                        directCallArguments
                            -- note how we don't recuse into the first element in the application
                            |> List.concatMap
                                (\otherExpressionNode ->
                                    otherExpressionNode
                                        |> Review.expressionSubsWithBindings
                                        |> List.concatMap
                                            (\sub ->
                                                sub.expressionNode
                                                    |> expressionPossibleTypeAliasConstructorUsesWithBindingsInScope
                                                        (FastSet.union (sub.bindings |> FastSet.fromList) outerBindingsInScope)
                                            )
                                )
                in
                if unqualifiedName |> isPossibleTypeAliasConstructorName then
                    { qualification = qualification
                    , unqualifiedName = unqualifiedName
                    , referenceRange = referenceRange
                    , bindingsInScope = outerBindingsInScope
                    , directCallRange = directCallRange
                    , directCallArguments =
                        directCallArguments
                            |> List.map
                                (\(Elm.Syntax.Node.Node directCallArgumentRange _) ->
                                    { range = directCallArgumentRange }
                                )
                    }
                        :: argumentsPossibleTypeAliasConstructorUse

                else
                    argumentsPossibleTypeAliasConstructorUse

            otherExpressionNode ->
                otherExpressionNode
                    |> Review.expressionSubsWithBindings
                    |> List.concatMap
                        (\sub ->
                            sub.expressionNode
                                |> expressionPossibleTypeAliasConstructorUsesWithBindingsInScope
                                    (FastSet.union (sub.bindings |> FastSet.fromList) outerBindingsInScope)
                        )


isPossibleTypeAliasConstructorName : String -> Bool
isPossibleTypeAliasConstructorName =
    \unqualifiedName ->
        case unqualifiedName |> String.uncons of
            Nothing ->
                False

            Just ( unqualifiedNameFirstChar, _ ) ->
                unqualifiedNameFirstChar |> Char.isUpper


report : Knowledge -> List Review.Error
report knowledge =
    knowledge.modulePossibleTypeAliasConstructorUses
        |> List.concatMap
            (\possibleTypeAliasConstructorUsesInModule ->
                let
                    explicitImports :
                        FastDict.Dict
                            Elm.Syntax.ModuleName.ModuleName
                            { alias : Maybe String, exposes : FastSet.Set String }
                    explicitImports =
                        { moduleExposes = knowledge.moduleExposes
                        , importsExposingAll = possibleTypeAliasConstructorUsesInModule.importsExposingAll
                        , importsExposingExplicit = possibleTypeAliasConstructorUsesInModule.importsExposingExplicit
                        }
                            |> Review.importsToExplicit
                in
                possibleTypeAliasConstructorUsesInModule.possibleTypeAliasConstructorUses
                    |> List.filterMap
                        (\possibleTypeAliasConstructorUse ->
                            let
                                moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                                moduleOrigin =
                                    ( possibleTypeAliasConstructorUse.qualification
                                    , possibleTypeAliasConstructorUse.unqualifiedName
                                    )
                                        |> Review.determineModuleOrigin explicitImports
                                        |> Maybe.withDefault possibleTypeAliasConstructorUsesInModule.moduleName
                            in
                            case knowledge.recordTypeAliasDeclarations |> FastDict.get ( moduleOrigin, possibleTypeAliasConstructorUse.unqualifiedName ) of
                                Nothing ->
                                    Nothing

                                Just usedRecordTypeAlias ->
                                    let
                                        reservedInScope : FastSet.Set String
                                        reservedInScope =
                                            possibleTypeAliasConstructorUse.bindingsInScope
                                                |> FastSet.union
                                                    (explicitImports
                                                        |> FastDict.LocalExtra.unionToSetMap
                                                            (\_ imported -> imported.exposes)
                                                    )
                                                |> FastSet.union possibleTypeAliasConstructorUsesInModule.moduleDeclaredNames
                                    in
                                    { path = possibleTypeAliasConstructorUsesInModule.modulePath
                                    , message = "record type alias constructor function is used"
                                    , details =
                                        [ "Constructing this record by specifying the fields and values instead will make your code easier to understand and less prone to positional errors."
                                        , "Read about more of the reasons in https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest#why"
                                        ]
                                    , range = possibleTypeAliasConstructorUse.referenceRange
                                    , fix =
                                        [ { path = possibleTypeAliasConstructorUsesInModule.modulePath
                                          , edits =
                                                [ let
                                                    curriedFieldValueNames : List String
                                                    curriedFieldValueNames =
                                                        usedRecordTypeAlias.recordFields
                                                            |> List.drop (possibleTypeAliasConstructorUse.directCallArguments |> List.length)
                                                            |> List.map
                                                                (\argumentName ->
                                                                    argumentName
                                                                        |> disambiguateFromSet
                                                                            (FastSet.union reservedInScope possibleTypeAliasConstructorUsesInModule.moduleDeclaredNames)
                                                                )

                                                    baseIndentation : String
                                                    baseIndentation =
                                                        String.repeat (possibleTypeAliasConstructorUse.directCallRange.start.column - 1) " "

                                                    record : String
                                                    record =
                                                        [ "{ "
                                                        , List.map2
                                                            (\field value -> field ++ " =" ++ value)
                                                            usedRecordTypeAlias.recordFields
                                                            ((possibleTypeAliasConstructorUse.directCallArguments
                                                                |> List.map
                                                                    (\argument ->
                                                                        "\n"
                                                                            ++ String.repeat (argument.startColumn - 1) " "
                                                                            ++ argument.source
                                                                    )
                                                             )
                                                                ++ (curriedFieldValueNames
                                                                        |> List.map (\curriedFieldValueName -> " " ++ curriedFieldValueName)
                                                                   )
                                                            )
                                                            |> String.join ("\n" ++ baseIndentation ++ ", ")
                                                        , "\n"
                                                        , baseIndentation
                                                        , "}"
                                                        ]
                                                            |> String.concat
                                                  in
                                                  Review.replaceRange possibleTypeAliasConstructorUse.directCallRange
                                                    (case curriedFieldValueNames of
                                                        [] ->
                                                            record

                                                        _ ->
                                                            [ "(\\"
                                                            , curriedFieldValueNames |> String.join " "
                                                            , " ->\n"
                                                            , baseIndentation
                                                            , record
                                                            , "\n"
                                                            , baseIndentation
                                                            , ")"
                                                            ]
                                                                |> String.concat
                                                    )
                                                ]
                                          }
                                        ]
                                    }
                                        |> Just
                        )
            )


disambiguateFromSet : FastSet.Set String -> (String -> String)
disambiguateFromSet forbiddenNamesSet =
    \name ->
        if forbiddenNamesSet |> FastSet.member name then
            (name ++ "_") |> disambiguateFromSet forbiddenNamesSet

        else
            name
