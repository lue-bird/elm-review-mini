module ExposesAreUsed exposing (review)

import Codec
import Declaration.LocalExtra
import Elm.Project
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import FastDict
import FastDict.LocalExtra
import Origin
import Review exposing (Review)
import Set exposing (Set)


review : Review
review =
    Review.create
        { inspect =
            [ Review.inspectElmJson
                (\elmJson ->
                    { referenceUseCounts = FastDict.empty
                    , moduleExposes = FastDict.empty
                    , modulesAllowingUnusedExposes =
                        case elmJson of
                            Elm.Project.Application _ ->
                                Set.empty

                            Elm.Project.Package packageElmJson ->
                                packageElmJson.exposed |> Review.packageElmJsonExposedModules
                    }
                )
            , Review.inspectModule
                (\moduleData ->
                    let
                        moduleName : Elm.Syntax.ModuleName.ModuleName
                        moduleName =
                            moduleData.syntax.moduleDefinition |> Review.moduleHeaderNameNode |> Elm.Syntax.Node.value
                    in
                    { referenceUseCounts =
                        { referenceUseCounts =
                            moduleData.syntax.declarations |> Declaration.LocalExtra.listReferenceCountUses
                        , imports = moduleData.syntax.imports |> List.map Elm.Syntax.Node.value
                        }
                            |> FastDict.singleton moduleName
                    , moduleExposes =
                        let
                            exposes :
                                { range : Elm.Syntax.Range.Range
                                , typeWithoutVariantsAndTypeAliasAndValueAndFunctionNames : FastDict.Dict String Elm.Syntax.Range.Range
                                , typesWithVariantNames : FastDict.Dict String { range : Elm.Syntax.Range.Range, variants : Set String }
                                }
                            exposes =
                                moduleData.syntax |> moduleToExposes
                        in
                        FastDict.singleton
                            moduleName
                            { path = moduleData.path
                            , exposingRange = exposes.range
                            , exposedValueAndFunctionAndTypeAliasAndTypeWithoutVariantsNames = exposes.typeWithoutVariantsAndTypeAliasAndValueAndFunctionNames
                            , exposedTypesWithVariantNames = exposes.typesWithVariantNames
                            }
                    , modulesAllowingUnusedExposes = Set.empty
                    }
                )
            ]
        , knowledgeMerge = knowledgeMerge
        , report = report
        , knowledgeCodec = knowledgeCodec
        }


report : Knowledge -> List { path : String, range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix }
report knowledge =
    let
        allFullyQualifiedReferenceUseCounts : FastDict.Dict Elm.Syntax.ModuleName.ModuleName (FastDict.Dict ( List String, String ) Int)
        allFullyQualifiedReferenceUseCounts =
            knowledge.referenceUseCounts
                |> FastDict.map
                    (\_ referenceUseCountsForModule ->
                        let
                            resourcesForEasyOriginLookup : Origin.ResourcesForEasyLookup
                            resourcesForEasyOriginLookup =
                                Origin.bundleResourcesForEasyLookup
                                    (knowledge.moduleExposes
                                        |> FastDict.map
                                            (\_ moduleExposes ->
                                                { exposedValueAndFunctionAndTypeAliasAndTypeWithoutVariantsNames =
                                                    moduleExposes.exposedValueAndFunctionAndTypeAliasAndTypeWithoutVariantsNames
                                                        |> FastDict.LocalExtra.keys
                                                , exposedTypesWithVariantNames =
                                                    moduleExposes.exposedTypesWithVariantNames
                                                        |> FastDict.map (\_ rangeAndVariantNames -> rangeAndVariantNames.variants)
                                                }
                                            )
                                    )
                                    referenceUseCountsForModule.imports
                        in
                        referenceUseCountsForModule.referenceUseCounts
                            |> FastDict.foldl
                                (\( qualification, unqualified ) referenceCount soFar ->
                                    soFar
                                        |> FastDict.update
                                            ( ( qualification, unqualified ) |> Origin.determine resourcesForEasyOriginLookup |> Maybe.withDefault []
                                            , unqualified
                                            )
                                            (\countSoFar ->
                                                (countSoFar |> Maybe.withDefault 0)
                                                    + referenceCount
                                                    |> Just
                                            )
                                )
                                FastDict.empty
                    )
    in
    knowledge.moduleExposes
        |> FastDict.LocalExtra.justsToListMap
            (\moduleName moduleExposes ->
                if knowledge.modulesAllowingUnusedExposes |> Set.member moduleName then
                    Nothing

                else
                    Just { name = moduleName, exposes = moduleExposes }
            )
        |> List.concatMap
            (\moduleKnowledge ->
                let
                    usedReferences : Set ( Elm.Syntax.ModuleName.ModuleName, String )
                    usedReferences =
                        allFullyQualifiedReferenceUseCounts
                            |> FastDict.remove moduleKnowledge.name
                            |> FastDict.foldl
                                (\_ referenceUseCounts soFar ->
                                    soFar |> referenceUseCountsMerge referenceUseCounts
                                )
                                FastDict.empty
                            |> FastDict.LocalExtra.justsToSetMap
                                (\reference _ ->
                                    reference |> Just
                                )
                            |> Set.insert ( moduleKnowledge.name, "main" )
                            |> Debug.log "used references"
                in
                [ moduleKnowledge.exposes.exposedValueAndFunctionAndTypeAliasAndTypeWithoutVariantsNames
                    |> Debug.log "exposedValueAndFunctionAndTypeAliasAndTypeWithoutVariantsNames"
                    |> FastDict.LocalExtra.justsToListMap
                        (\exposeUnqualified exposeRange ->
                            if usedReferences |> Set.member ( moduleKnowledge.name, exposeUnqualified ) then
                                Nothing

                            else
                                { path = moduleKnowledge.exposes.path
                                , message = [ "expose ", ( moduleKnowledge.name, exposeUnqualified ) |> referenceToString, " isn't used outside of this module" ] |> String.concat
                                , details =
                                    [ "Either use it or remove it from the exposing part of the module header which might reveal its declaration as unused." ]
                                , range = exposeRange
                                , fix =
                                    [ Review.fixReplaceRange moduleKnowledge.exposes.exposingRange
                                        (Set.union
                                            (moduleKnowledge.exposes.exposedValueAndFunctionAndTypeAliasAndTypeWithoutVariantsNames
                                                |> FastDict.LocalExtra.keys
                                                |> Set.remove exposeUnqualified
                                            )
                                            (moduleKnowledge.exposes.exposedTypesWithVariantNames
                                                |> FastDict.LocalExtra.keys
                                            )
                                            |> exposingToString
                                        )
                                    ]
                                }
                                    |> Just
                        )
                , moduleKnowledge.exposes.exposedTypesWithVariantNames
                    |> FastDict.LocalExtra.justsToListMap
                        (\typeExposeUnqualified typeExpose ->
                            let
                                typeExposeVariantReferences : () -> Set ( Elm.Syntax.ModuleName.ModuleName, String )
                                typeExposeVariantReferences () =
                                    typeExpose.variants |> Set.map (\unqualified -> ( moduleKnowledge.name, unqualified ))
                            in
                            if
                                (usedReferences |> Set.member ( moduleKnowledge.name, typeExposeUnqualified ))
                                    && (Set.diff (typeExposeVariantReferences ()) usedReferences |> Set.isEmpty)
                            then
                                { path = moduleKnowledge.exposes.path
                                , message = [ "expose ", ( moduleKnowledge.name, typeExposeUnqualified ) |> referenceToString, " isn't used outside of this module" ] |> String.concat
                                , details =
                                    [ "Either use it or remove it from the exposing part of the module header which might reveal its declaration as unused." ]
                                , range = typeExpose.range
                                , fix =
                                    [ Review.fixReplaceRange moduleKnowledge.exposes.exposingRange
                                        (Set.union
                                            (moduleKnowledge.exposes.exposedValueAndFunctionAndTypeAliasAndTypeWithoutVariantsNames
                                                |> FastDict.LocalExtra.keys
                                            )
                                            (moduleKnowledge.exposes.exposedTypesWithVariantNames
                                                |> FastDict.LocalExtra.keys
                                                |> Set.remove typeExposeUnqualified
                                            )
                                            |> exposingToString
                                        )
                                    ]
                                }
                                    |> Just

                            else
                                Nothing
                        )
                ]
                    |> List.concat
            )


exposingToString : Set String -> String
exposingToString =
    \exposes ->
        [ "exposing (", exposes |> Set.toList |> String.join ", ", ")" ] |> String.concat


referenceToString : ( Elm.Syntax.ModuleName.ModuleName, String ) -> String
referenceToString =
    \( moduleName, name ) ->
        [ moduleName |> String.join ".", ".", name ] |> String.concat


referenceUseCountsMerge :
    FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
referenceUseCountsMerge a b =
    FastDict.LocalExtra.unionWith (\aCount bCount -> aCount + bCount) a b


moduleToExposes :
    Elm.Syntax.File.File
    ->
        { range : Elm.Syntax.Range.Range
        , typeWithoutVariantsAndTypeAliasAndValueAndFunctionNames : FastDict.Dict String Elm.Syntax.Range.Range
        , typesWithVariantNames : FastDict.Dict String { range : Elm.Syntax.Range.Range, variants : Set String }
        }
moduleToExposes syntaxFile =
    let
        moduleTypesWithVariantNames : FastDict.Dict String (Set String)
        moduleTypesWithVariantNames =
            syntaxFile.declarations
                |> List.filterMap
                    (\(Elm.Syntax.Node.Node _ declaration) ->
                        case declaration of
                            Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                                ( choiceTypeDeclaration.name |> Elm.Syntax.Node.value
                                , choiceTypeDeclaration.constructors
                                    |> List.map
                                        (\(Elm.Syntax.Node.Node _ constructor) ->
                                            constructor.name |> Elm.Syntax.Node.value
                                        )
                                    |> Set.fromList
                                )
                                    |> Just

                            _ ->
                                Nothing
                    )
                |> FastDict.fromList
    in
    case syntaxFile.moduleDefinition |> Review.moduleHeaderExposing of
        Elm.Syntax.Node.Node range (Elm.Syntax.Exposing.Explicit exposeSet) ->
            { range = range
            , typeWithoutVariantsAndTypeAliasAndValueAndFunctionNames =
                exposeSet
                    |> List.filterMap
                        (\(Elm.Syntax.Node.Node exposeRange expose) ->
                            case expose of
                                Elm.Syntax.Exposing.TypeExpose choiceTypeExpose ->
                                    case choiceTypeExpose.open of
                                        Nothing ->
                                            ( choiceTypeExpose.name, exposeRange ) |> Just

                                        Just _ ->
                                            Nothing

                                Elm.Syntax.Exposing.FunctionExpose valueOrFunctionName ->
                                    ( valueOrFunctionName, exposeRange ) |> Just

                                Elm.Syntax.Exposing.TypeOrAliasExpose exposeName ->
                                    ( exposeName, exposeRange ) |> Just

                                Elm.Syntax.Exposing.InfixExpose _ ->
                                    Nothing
                        )
                    |> FastDict.fromList
            , typesWithVariantNames =
                exposeSet
                    |> List.filterMap
                        (\(Elm.Syntax.Node.Node exposeRange expose) ->
                            case expose of
                                Elm.Syntax.Exposing.TypeExpose choiceTypeExpose ->
                                    case choiceTypeExpose.open of
                                        Nothing ->
                                            Nothing

                                        Just _ ->
                                            moduleTypesWithVariantNames
                                                |> FastDict.get choiceTypeExpose.name
                                                |> Maybe.map
                                                    (\variantNames ->
                                                        ( choiceTypeExpose.name
                                                        , { range = exposeRange
                                                          , variants = variantNames
                                                          }
                                                        )
                                                    )

                                Elm.Syntax.Exposing.FunctionExpose _ ->
                                    Nothing

                                Elm.Syntax.Exposing.TypeOrAliasExpose _ ->
                                    Nothing

                                Elm.Syntax.Exposing.InfixExpose _ ->
                                    Nothing
                        )
                    |> FastDict.fromList
            }

        Elm.Syntax.Node.Node range (Elm.Syntax.Exposing.All allRange) ->
            { range = range
            , typeWithoutVariantsAndTypeAliasAndValueAndFunctionNames =
                Debug.todo ""
                    |> FastDict.fromList
            , typesWithVariantNames =
                moduleTypesWithVariantNames
                    |> FastDict.map (\_ variants -> { variants = variants, range = allRange })
            }



--


knowledgeMerge : Knowledge -> Knowledge -> Knowledge
knowledgeMerge a b =
    { moduleExposes = FastDict.union a.moduleExposes b.moduleExposes
    , referenceUseCounts =
        FastDict.union a.referenceUseCounts b.referenceUseCounts
    , modulesAllowingUnusedExposes =
        Set.union a.modulesAllowingUnusedExposes b.modulesAllowingUnusedExposes
    }


knowledgeCodec : Codec.Codec Knowledge
knowledgeCodec =
    Codec.object
        (\moduleExposes referenceUseCounts modulesAllowingUnusedExposes ->
            { moduleExposes = moduleExposes
            , referenceUseCounts = referenceUseCounts
            , modulesAllowingUnusedExposes = modulesAllowingUnusedExposes
            }
        )
        |> Codec.field "moduleExposes"
            .moduleExposes
            (fastDictKeyValueCodec Review.moduleNameCodec
                (Codec.object
                    (\path exposingRange exposedValueAndFunctionAndTypeAliasAndTypeWithoutVariantsNames exposedTypesWithVariantNames ->
                        { path = path
                        , exposingRange = exposingRange
                        , exposedValueAndFunctionAndTypeAliasAndTypeWithoutVariantsNames = exposedValueAndFunctionAndTypeAliasAndTypeWithoutVariantsNames
                        , exposedTypesWithVariantNames = exposedTypesWithVariantNames
                        }
                    )
                    |> Codec.field "path" .path Codec.string
                    |> Codec.field "exposingRange" .exposingRange Review.rangeCodec
                    |> Codec.field "exposedValueAndFunctionAndTypeAliasAndTypeWithoutVariantsNames"
                        .exposedValueAndFunctionAndTypeAliasAndTypeWithoutVariantsNames
                        (fastDictKeyValueCodec Codec.string Review.rangeCodec)
                    |> Codec.field "exposedTypesWithVariantNames"
                        .exposedTypesWithVariantNames
                        (fastDictKeyValueCodec Codec.string
                            (Codec.object (\variants range -> { variants = variants, range = range })
                                |> Codec.field "variants" .variants (Codec.set Codec.string)
                                |> Codec.field "range" .range Review.rangeCodec
                                |> Codec.buildObject
                            )
                        )
                    |> Codec.buildObject
                )
            )
        |> Codec.field "referenceUseCounts"
            .referenceUseCounts
            (fastDictKeyValueCodec Review.moduleNameCodec
                (Codec.object (\referenceUseCounts imports -> { referenceUseCounts = referenceUseCounts, imports = imports })
                    |> Codec.field "referenceUseCounts"
                        .referenceUseCounts
                        (fastDictKeyValueCodec
                            (Codec.tuple Review.moduleNameCodec Codec.string)
                            Codec.int
                        )
                    |> Codec.field "imports" .imports (Codec.list importCodec)
                    |> Codec.buildObject
                )
            )
        |> Codec.field "modulesAllowingUnusedExposes"
            .modulesAllowingUnusedExposes
            (Codec.set Review.moduleNameCodec)
        |> Codec.buildObject


importCodec : Codec.Codec Elm.Syntax.Import.Import
importCodec =
    Codec.build Elm.Syntax.Import.encode Elm.Syntax.Import.decoder



--


fastDictKeyValueCodec :
    Codec.Codec comparableKey
    -> Codec.Codec value
    -> Codec.Codec (FastDict.Dict comparableKey value)
fastDictKeyValueCodec keyCodec valueCodec =
    Codec.map FastDict.fromList
        FastDict.toList
        (Codec.list
            (Codec.tuple
                keyCodec
                valueCodec
            )
        )


type alias Knowledge =
    { referenceUseCounts :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            { imports : List Elm.Syntax.Import.Import
            , referenceUseCounts : FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
            }
    , modulesAllowingUnusedExposes : Set Elm.Syntax.ModuleName.ModuleName
    , moduleExposes :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            { path : String
            , exposingRange : Elm.Syntax.Range.Range
            , exposedValueAndFunctionAndTypeAliasAndTypeWithoutVariantsNames : FastDict.Dict String Elm.Syntax.Range.Range
            , exposedTypesWithVariantNames : FastDict.Dict String { range : Elm.Syntax.Range.Range, variants : Set String }
            }
    }
