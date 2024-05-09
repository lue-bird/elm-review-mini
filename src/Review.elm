module Review exposing
    ( ignoreErrorsForFilesWhere
    , create
    , Inspect, inspectElmJson, inspectModule, inspectDirectDependencies, inspectExtraFile
    , inspectBatch, inspectMap
    , Error, FileTarget(..)
    , Fix(..), fixInsertAt, fixRemoveRange, fixReplaceRangeBy
    , expressionSubs, expressionFold
    , moduleHeaderDocumentation, moduleHeaderNameNode, moduleHeaderExposing
    , sourceExtractInRange
    , packageElmJsonExposedModules
    , locationCodec, rangeCodec, moduleNameCodec
    , run, Cache, cacheEmpty, fixFile, FixError(..)
    , KnowledgeGeneric(..)
    , Review, InspectSingleTargetToKnowledge(..)
    )

{-| `elm-review-mini` scans the modules, `elm.json`, dependencies and extra files like `README.md` from your project.
All this data is then fed into your reviews, which in turn inspect it to report problems.

@docs ignoreErrorsForFilesWhere

@docs create


## inspecting

Look at the global picture of an Elm project to collect knowledge.

@docs Inspect, inspectElmJson, inspectModule, inspectDirectDependencies, inspectExtraFile
@docs inspectBatch, inspectMap


## reporting

@docs Error, FileTarget
@docs Fix, fixInsertAt, fixRemoveRange, fixReplaceRangeBy


## convenience helpers

@docs expressionSubs, expressionFold
@docs moduleHeaderDocumentation, moduleHeaderNameNode, moduleHeaderExposing
@docs sourceExtractInRange
@docs packageElmJsonExposedModules
@docs locationCodec, rangeCodec, moduleNameCodec


# safe internals

@docs run, Cache, cacheEmpty, fixFile, FixError
@docs KnowledgeGeneric
@docs Review, InspectSingleTargetToKnowledge

-}

import Codec
import Elm.Docs
import Elm.Module
import Elm.Parser
import Elm.Project
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Infix
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import ElmCoreDependency
import ElmJson.LocalExtra
import FastDict
import FastDictExtra
import Json.Decode
import Json.Encode
import ListExtra
import Rope exposing (Rope)
import Set exposing (Set)
import Unicode


{-| A construct that can inspect your project files and report errors.

You can create one with [`Review.create`](Review#create)

-}
type alias Review =
    { name : String
    , ignoreErrorsForFiles : String -> Bool
    , run :
        { elmJson : { source : String, project : Elm.Project.Project }
        , directDependencies : List { elmJson : String, docsJson : String }
        , addedOrChangedFiles : List { path : String, source : String }
        , removedFilePaths : List String
        , cache : Cache
        }
        ->
            { errorsByPath : FastDict.Dict String (List { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Fix })
            , cache : Cache
            }
    }


{-| The collected info from scanning a project in a generic format that can be used for caching.
See [`Inspect`](#Inspect)
-}
type KnowledgeGeneric
    = KnowledgeJson Json.Encode.Value


{-| How to collect info from scanning a project.
-}
type alias Inspect knowledge =
    Rope (InspectSingleTargetToKnowledge knowledge)


{-| Allow multiple reviews to feed off the same collected knowledge,
for example published in a package.

Examples:

  - "data to determine all bindings in scope"
  - "data to determine a given reference's full origin"
  - "data to determine the reference's minimum qualification"
  - "type information"

The given name should describe what's in the knowledge,
like "module exposes including variants from project, direct and indirect dependencies"

-}
inspectBatch : List (Inspect knowledge) -> Inspect knowledge
inspectBatch inspects =
    inspects |> Rope.fromList |> Rope.concat


{-| The smallest possible thing you can inspect and create knowledge from
-}
type InspectSingleTargetToKnowledge knowledge
    = InspectDirectDependenciesToKnowledge (List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> knowledge)
    | InspectElmJsonToKnowledge (Elm.Project.Project -> knowledge)
    | InspectExtraFileToKnowledge ({ path : String, source : String } -> knowledge)
    | InspectProjectModuleToKnowledge ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> knowledge)


{-| Reference to a project file.
For modules and extra files, use `FileTarget` with the path from the inspect.
To refer to the elm.json, use `FileTargetElmJson`
-}
type FileTarget
    = FileTargetElmJson
    | FileTarget { path : String }


{-| Usually, when using an external [`inspectBatch`](#inspectBatch)
your knowledge type and its knowledge type don't match up.

Use [`inspectMap`](#inspectMap) to use all that information to create your own knowledge from it

-}
inspectMap : (knowledge -> knowledgeMapped) -> (Inspect knowledge -> Inspect knowledgeMapped)
inspectMap knowledgeChange =
    \inspect ->
        inspect
            |> Rope.map
                (\inspectSingle ->
                    inspectSingle |> inspectSingleTargetToKnowledgeMap knowledgeChange
                )


inspectSingleTargetToKnowledgeMap : (knowledge -> knowledgeMapped) -> (InspectSingleTargetToKnowledge knowledge -> InspectSingleTargetToKnowledge knowledgeMapped)
inspectSingleTargetToKnowledgeMap knowledgeChange =
    \inspectSingle ->
        case inspectSingle of
            InspectDirectDependenciesToKnowledge toKnowledge ->
                (\data -> data |> toKnowledge |> knowledgeChange) |> InspectDirectDependenciesToKnowledge

            InspectElmJsonToKnowledge toKnowledge ->
                (\data -> data |> toKnowledge |> knowledgeChange) |> InspectElmJsonToKnowledge

            InspectExtraFileToKnowledge toKnowledge ->
                (\data -> data |> toKnowledge |> knowledgeChange) |> InspectExtraFileToKnowledge

            InspectProjectModuleToKnowledge toKnowledge ->
                (\data -> data |> toKnowledge |> knowledgeChange) |> InspectProjectModuleToKnowledge


{-| All the info you can get from a module.


## syntax

A tree-like structure which represents your source code, using the
[`elm-syntax` package](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/).
All the unknown imports you'll see will be coming from there. You are likely to
need to have the documentation for that package open when writing a review.


### module header

the module's
[module definition](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Module) (`module SomeModuleName exposing (a, b)`).

Through a couple of helpers, you can get to the specific information you need faster:

  - [`moduleHeaderNameNode`](#moduleHeaderNameNode)
  - [`moduleHeaderDocumentation`](#moduleHeaderDocumentation)


### imports

the module's
[import statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Import)
(`import Html as H exposing (div)`) in order of their definition

The following example forbids importing both `Element` (`elm-ui`) and
`Html.Styled` (`elm-css`).

    import Elm.Syntax.Import exposing (Import)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Rule)

    type alias Knowledge =
        { elmUiWasImported : Bool
        , elmCssWasImported : Bool
        }

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoUsingBothHtmlAndHtmlStyled" initialKnowledge
            |> Rule.withImportVisitor importVisitor
            |> Rule.fromModuleRuleSchema

    initialKnowledge : Knowledge
    initialKnowledge =
        { elmUiWasImported = False
        , elmCssWasImported = False
        }

    error : Node Import -> Error
    error node =
        Rule.error
            { message = "Do not use both `elm-ui` and `elm-css`"
            , details = [ "At fruits.com, we use `elm-ui` in the dashboard application, and `elm-css` in the rest of the code. We want to use `elm-ui` in our new projects, but in projects using `elm-css`, we don't want to use both libraries to keep things simple." ]
            }
            (Node.range node)

    importVisitor : Node Import -> Knowledge -> ( List Rule.Error, Knowledge )
    importVisitor node knowledge =
        case Node.value node |> .moduleName |> Node.value of
            [ "Element" ] ->
                if knowledge.elmCssWasImported then
                    ( [ error node ]
                    , { knowledge | elmUiWasImported = True }
                    )

                else
                    ( [ error node ]
                    , { knowledge | elmUiWasImported = True }
                    )

            [ "Html", "Styled" ] ->
                if knowledge.elmUiWasImported then
                    ( [ error node ]
                    , { knowledge | elmCssWasImported = True }
                    )

                else
                    ( [ error node ]
                    , { knowledge | elmCssWasImported = True }
                    )

            _ ->
                ( [], knowledge )


### comments

the module's comments in source order not parsed as attached documentation by
[`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/):
included (✅) / excluded (❌)

  - ✅ Module documentation (`{-| -}`)
  - ✅ Port documentation comments (`{-| -}`)
  - ✅ Top-level comments not internal to a function/type/etc.
  - ✅ Comments internal to a function/type/etc.
  - ❌ Function/type/type alias documentation comments (`{-| -}`)

If you only need to access the module documentation, you can use
[`moduleHeaderDocumentation`](#moduleHeaderDocumentation).


### declarations

the module's
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`),

The following example forbids exposing a function or a value without it having a
type annotation.

    import Elm.Syntax.Declaration as Declaration exposing (Declaration)
    import Elm.Syntax.Exposing as Exposing
    import Elm.Syntax.Module as Module exposing (Module)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Rule)

    type ExposedFunctions
        = All
        | OnlySome (List String)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoMissingDocumentationForExposedFunctions" (OnlySome [])
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.withDeclarationEnterVisitor declarationVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> ExposedFunctions -> ( List Rule.Error, ExposedFunctions )
    moduleDefinitionVisitor node knowledge =
        case Node.value node |> Module.exposingList of
            Exposing.All _ ->
                ( [], All )

            Exposing.Explicit exposedValues ->
                ( [], OnlySome (List.filterMap exposedFunctionName exposedValues) )

    exposedFunctionName : Node Exposing.TopLevelExpose -> Maybe String
    exposedFunctionName value =
        case Node.value value of
            Exposing.FunctionExpose functionName ->
                Just functionName

            _ ->
                Nothing

    declarationVisitor : Node Declaration -> ExposedFunctions -> ( List Rule.Error, ExposedFunctions )
    declarationVisitor node direction knowledge =
        case Node.value node of
            Declaration.FunctionDeclaration { documentation, declaration } ->
                let
                    functionName : String
                    functionName =
                        Node.value declaration |> .name |> Node.value
                in
                if documentation == Nothing && isExposed knowledge functionName then
                    ( [ Rule.error
                            { message = "Exposed function " ++ functionName ++ " is missing a type annotation"
                            , details =
                                [ "Type annotations are very helpful for people who use the module. It can give a lot of information without having to read the contents of the function."
                                , "To add a type annotation, add a line like `" functionName ++ " : ()`, and replace the `()` by the type of the function. If you don't replace `()`, the compiler should give you a suggestion of what the type should be."
                                ]
                            }
                            (Node.range node)
                      ]
                    , knowledge
                    )

                else
                    ( [], knowledge )

            _ ->
                ( [], knowledge )

    isExposed : ExposedFunctions -> String -> Bool
    isExposed exposedFunctions name =
        case exposedFunctions of
            All ->
                True

            OnlySome exposedList ->
                List.member name exposedList


### path

The file path, relative to the project's `elm.json`

-}
inspectModule : ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> knowledge) -> Inspect knowledge
inspectModule moduleDataToKnowledge =
    InspectProjectModuleToKnowledge moduleDataToKnowledge |> Rope.singleton


{-| Collect knowledge from the [elm.json project config](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Project#Project)

A commonly useful helper is [`packageElmJsonExposedModules`](#packageElmJsonExposedModules)

-}
inspectElmJson : (Elm.Project.Project -> knowledge) -> Inspect knowledge
inspectElmJson moduleDataToKnowledge =
    InspectElmJsonToKnowledge moduleDataToKnowledge |> Rope.singleton


{-| Collect knowledge from a provided non-elm or elm.json file like README.md or CHANGELOG.md.

The provided file path is relative to the project's `elm.json`

-}
inspectExtraFile : ({ path : String, source : String } -> knowledge) -> Inspect knowledge
inspectExtraFile moduleDataToKnowledge =
    InspectExtraFileToKnowledge moduleDataToKnowledge |> Rope.singleton


{-| Collect knowledge from all [project config and docs](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/) you directly depend upon
-}
inspectDirectDependencies : (List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> knowledge) -> Inspect knowledge
inspectDirectDependencies moduleDataToKnowledge =
    InspectDirectDependenciesToKnowledge moduleDataToKnowledge |> Rope.singleton


{-| Write a new [`Review`](#Review)

  - read ["when to write or enable a review"](./#when-to-write-or-enable-a-review)
  - `name`: (`VariablesAreUsed`, `DebugForbid`, ...)
    the name of the module this review is defined in including all the `.`s.
    It should really quickly convey what kind of pattern we're dealing with. A user who
    encounters this pattern for the first time could guess the problem just from the
    name. And a user who encountered it several times should know how to fix the
    problem just from the name, too
  - `inspect`: the parts you want to analyze and collect knowledge from. See [`Inspect`](#Inspect)
  - `knowledgeMerge`: assemble knowledge from multiple you've collected into one
  - `report`: the final evaluation, turning your collected and merged knowledge into a list of errors.
    Take the elm compiler errors as inspiration in terms of helpfulness:
  - error `message`: half-sentence on what _is_ undesired here. A user
    that has encountered this error multiple times should know exactly what to do.
    Example: "`foo` is never used" → a user who
    knows the rule knows that a function can be removed and which one
  - error `details`: additional information such as the rationale and suggestions
    for a solution or alternative
  - error report `range`: where the squiggly lines appear under. Make this section as small as
    possible. For instance, in a rule that would forbid
    `Debug.log`, you would the error to appear under `Debug.log`, not on the whole
    function call
  - `knowledgeCodec`: For big files especially, re-running all the parsing and inspection every time we change a different file
    is costly, so we serialize it for caching using [miniBill/elm-codec](https://dark.elm.dmy.fr/packages/miniBill/elm-codec/latest/)
  - documentation: explain when (not) to enable the review. For instance, for a review that
    makes sure that a package is publishable by ensuring that all docs are valid,
    it should say something like "If you are writing an application, you shouldn't enable it".
    Add a few examples of patterns that will (not) be reported


### use Test-Driven Development!

[`Review.Test`](Review-Test) works with [`elm-test`](https://github.com/elm-explorations/test).
Read ["strategies for effective testing"](Review-Test) before
starting writing a review.
If you like putting `Debug.log`s in your code, you'll have a pleasant time
running your tests with [`elm-test-rs`](https://github.com/mpizenberg/elm-test-rs).

-}
create :
    { name : String
    , inspect : List (Inspect knowledge)
    , knowledgeMerge : knowledge -> knowledge -> knowledge
    , report : knowledge -> List Error
    , knowledgeCodec : Codec.Codec knowledge
    }
    -> Review
create review =
    let
        knowledgeToCache : knowledge -> KnowledgeGeneric
        knowledgeToCache =
            \knowledge ->
                knowledge
                    |> Codec.encodeToValue review.knowledgeCodec
                    |> KnowledgeJson

        knowledgeFromCache : KnowledgeGeneric -> Result Json.Decode.Error knowledge
        knowledgeFromCache =
            \(KnowledgeJson serializedBase64) ->
                serializedBase64 |> Codec.decodeValue review.knowledgeCodec

        toKnowledges :
            { directDependenciesToKnowledge : List (List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> knowledge)
            , elmJsonToKnowledge : List (Elm.Project.Project -> knowledge)
            , extraFileToKnowledge : List ({ path : String, source : String } -> knowledge)
            , moduleToKnowledge : List ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> knowledge)
            }
        toKnowledges =
            review.inspect
                |> Rope.fromList
                |> Rope.concat
                |> inspectToToKnowledges
    in
    { name = review.name
    , ignoreErrorsForFiles = \_ -> False
    , run =
        \project ->
            let
                knowledgesFoldToMaybe : List knowledge -> Maybe knowledge
                knowledgesFoldToMaybe =
                    \knowledges ->
                        case knowledges of
                            [] ->
                                Nothing

                            one :: others ->
                                others |> List.foldl review.knowledgeMerge one |> Just

                elmJsonKnowledgeAndCache : Maybe { knowledge : knowledge, cache : KnowledgeGeneric }
                elmJsonKnowledgeAndCache =
                    case project.cache.elmJsonKnowledge of
                        Just cache ->
                            case cache |> knowledgeFromCache of
                                Err _ ->
                                    -- corrupt cache
                                    Nothing

                                Ok knowledge ->
                                    { knowledge = knowledge, cache = cache } |> Just

                        Nothing ->
                            toKnowledges.elmJsonToKnowledge
                                |> List.map (\f -> f project.elmJson.project)
                                |> knowledgesFoldToMaybe
                                |> Maybe.map (\knowledge -> { knowledge = knowledge, cache = knowledge |> knowledgeToCache })

                directDependenciesKnowledgeAndCache : Maybe { knowledge : knowledge, cache : KnowledgeGeneric }
                directDependenciesKnowledgeAndCache =
                    case project.cache.directDependenciesKnowledge of
                        Just cache ->
                            case cache |> knowledgeFromCache of
                                Err _ ->
                                    -- corrupt cache
                                    Nothing

                                Ok knowledge ->
                                    { knowledge = knowledge, cache = cache } |> Just

                        Nothing ->
                            let
                                directDependenciesIncluding : List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
                                directDependenciesIncluding =
                                    project.directDependencies
                                        |> List.filterMap
                                            (\dependency ->
                                                Result.map2 (\elmJson modules -> { elmJson = elmJson, modules = modules })
                                                    (dependency.elmJson |> Json.Decode.decodeString Elm.Project.decoder)
                                                    (dependency.elmJson |> Json.Decode.decodeString (Json.Decode.list Elm.Docs.decoder))
                                                    |> Result.toMaybe
                                            )
                                        |> ListExtra.consJust ElmCoreDependency.parsed
                            in
                            toKnowledges.directDependenciesToKnowledge
                                |> List.map (\f -> f directDependenciesIncluding)
                                |> knowledgesFoldToMaybe
                                |> Maybe.map (\knowledge -> { knowledge = knowledge, cache = knowledge |> knowledgeToCache })

                moduleToMaybeKnowledge :
                    { path : String, source : String }
                    -> Maybe { path : String, knowledge : knowledge, cache : KnowledgeGeneric }
                moduleToMaybeKnowledge moduleFile =
                    case moduleFile.source |> Elm.Parser.parseToFile of
                        Err _ ->
                            Nothing

                        Ok syntax ->
                            let
                                moduleData : { path : String, source : String, syntax : Elm.Syntax.File.File }
                                moduleData =
                                    { path = moduleFile.path
                                    , source = moduleFile.source
                                    , syntax = syntax |> syntaxFileSanitize
                                    }
                            in
                            toKnowledges.moduleToKnowledge
                                |> List.map (\f -> f moduleData)
                                |> knowledgesFoldToMaybe
                                |> Maybe.map
                                    (\foldedKnowledge ->
                                        { path = moduleFile.path
                                        , knowledge = foldedKnowledge
                                        , cache = foldedKnowledge |> knowledgeToCache
                                        }
                                    )

                sourceDirectories : List String
                sourceDirectories =
                    project.elmJson.project |> ElmJson.LocalExtra.sourceDirectories

                pathIsModule : String -> Bool
                pathIsModule =
                    \path ->
                        (path |> String.endsWith ".elm")
                            && (sourceDirectories |> List.any (\dir -> path |> String.startsWith dir))

                ( addedOrChangedModuleFiles, addedOrChangedExtraFiles ) =
                    project.addedOrChangedFiles
                        |> List.partition
                            (\file -> file.path |> pathIsModule)

                ( removedModuleFilePaths, removedExtraFilePaths ) =
                    project.removedFilePaths
                        |> List.partition
                            (\path -> path |> pathIsModule)

                moduleKnowledges : List { path : String, knowledge : knowledge, cache : KnowledgeGeneric }
                moduleKnowledges =
                    FastDict.merge
                        (\_ new soFar -> soFar |> ListExtra.consJust new)
                        (\_ new _ soFar -> soFar |> ListExtra.consJust new)
                        (\path knowledgeCache soFar ->
                            case knowledgeCache |> knowledgeFromCache of
                                Err _ ->
                                    -- corrupt cache
                                    soFar

                                Ok knowledge ->
                                    soFar |> (::) { path = path, knowledge = knowledge, cache = knowledgeCache }
                        )
                        (addedOrChangedModuleFiles
                            |> FastDictExtra.fromListMap
                                (\file ->
                                    { key = file.path
                                    , value = file |> moduleToMaybeKnowledge
                                    }
                                )
                        )
                        (project.cache.moduleKnowledgesByPath
                            |> dictRemoveKeys removedModuleFilePaths
                        )
                        []

                extraFileToMaybeKnowledge :
                    { path : String, source : String }
                    -> Maybe { path : String, knowledge : knowledge, cache : KnowledgeGeneric }
                extraFileToMaybeKnowledge fileInfo =
                    toKnowledges.extraFileToKnowledge
                        |> List.map (\f -> f fileInfo)
                        |> knowledgesFoldToMaybe
                        |> Maybe.map
                            (\foldedKnowledge ->
                                { path = fileInfo.path
                                , knowledge = foldedKnowledge
                                , cache = foldedKnowledge |> knowledgeToCache
                                }
                            )

                extraFilesKnowledges : List { path : String, knowledge : knowledge, cache : KnowledgeGeneric }
                extraFilesKnowledges =
                    FastDict.merge
                        (\_ new soFar -> soFar |> ListExtra.consJust new)
                        (\_ new _ soFar -> soFar |> ListExtra.consJust new)
                        (\path knowledgeCache soFar ->
                            case knowledgeCache |> knowledgeFromCache of
                                Err _ ->
                                    -- corrupt cache
                                    soFar

                                Ok knowledge ->
                                    soFar |> (::) { path = path, knowledge = knowledge, cache = knowledgeCache }
                        )
                        (addedOrChangedExtraFiles
                            |> FastDictExtra.fromListMap
                                (\file ->
                                    { key = file.path
                                    , value = file |> extraFileToMaybeKnowledge
                                    }
                                )
                        )
                        (project.cache.extraFileKnowledgesByPath
                            |> dictRemoveKeys removedExtraFilePaths
                        )
                        []

                allKnowledges : List knowledge
                allKnowledges =
                    ((moduleKnowledges |> List.map .knowledge)
                        ++ (extraFilesKnowledges |> List.map .knowledge)
                    )
                        |> ListExtra.consJust (directDependenciesKnowledgeAndCache |> Maybe.map .knowledge)
                        |> ListExtra.consJust (elmJsonKnowledgeAndCache |> Maybe.map .knowledge)
            in
            case allKnowledges |> knowledgesFoldToMaybe of
                Nothing ->
                    { errorsByPath = FastDict.empty, cache = project.cache }

                Just completeKnowledge ->
                    { errorsByPath =
                        completeKnowledge
                            |> review.report
                            |> List.foldl
                                (\error soFar ->
                                    soFar
                                        |> FastDict.update
                                            (error.target |> fileTargetToPath)
                                            (\errorsForPathSoFar ->
                                                errorsForPathSoFar
                                                    |> Maybe.withDefault []
                                                    |> (::)
                                                        { message = error.message
                                                        , details = error.details
                                                        , range = error.range
                                                        , fix = error.fix
                                                        }
                                                    |> Just
                                            )
                                )
                                FastDict.empty
                            |> FastDict.map
                                (\_ errors ->
                                    errors |> List.sortWith (\a b -> rangeCompare a.range b.range)
                                )
                    , cache =
                        { elmJsonKnowledge = elmJsonKnowledgeAndCache |> Maybe.map .cache
                        , directDependenciesKnowledge = directDependenciesKnowledgeAndCache |> Maybe.map .cache
                        , moduleKnowledgesByPath =
                            moduleKnowledges
                                |> FastDictExtra.fromListMap
                                    (\moduleKnowledge ->
                                        { key = moduleKnowledge.path, value = moduleKnowledge.cache }
                                    )
                        , extraFileKnowledgesByPath =
                            moduleKnowledges
                                |> FastDictExtra.fromListMap
                                    (\extraFileKnowledge ->
                                        { key = extraFileKnowledge.path, value = extraFileKnowledge.cache }
                                    )
                        }
                    }
    }


fileTargetToPath : FileTarget -> String
fileTargetToPath =
    \fileTarget ->
        case fileTarget of
            FileTargetElmJson ->
                "elm.json"

            FileTarget filePath ->
                filePath.path


inspectToToKnowledges :
    Inspect knowledge
    ->
        { directDependenciesToKnowledge : List (List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> knowledge)
        , elmJsonToKnowledge : List (Elm.Project.Project -> knowledge)
        , extraFileToKnowledge : List ({ path : String, source : String } -> knowledge)
        , moduleToKnowledge : List ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> knowledge)
        }
inspectToToKnowledges =
    \inspect ->
        inspect
            |> Rope.foldl
                (\inspectSingle soFar ->
                    case inspectSingle of
                        InspectDirectDependenciesToKnowledge toKnowledge ->
                            { soFar
                                | directDependenciesToKnowledge =
                                    soFar.directDependenciesToKnowledge |> (::) toKnowledge
                            }

                        InspectElmJsonToKnowledge toKnowledge ->
                            { soFar
                                | elmJsonToKnowledge =
                                    soFar.elmJsonToKnowledge |> (::) toKnowledge
                            }

                        InspectExtraFileToKnowledge toKnowledge ->
                            { soFar
                                | extraFileToKnowledge =
                                    soFar.extraFileToKnowledge |> (::) toKnowledge
                            }

                        InspectProjectModuleToKnowledge toKnowledge ->
                            { soFar
                                | moduleToKnowledge =
                                    soFar.moduleToKnowledge |> (::) toKnowledge
                            }
                )
                { directDependenciesToKnowledge = []
                , elmJsonToKnowledge = []
                , extraFileToKnowledge = []
                , moduleToKnowledge = []
                }


dictRemoveKeys : List comparableKey -> (FastDict.Dict comparableKey v -> FastDict.Dict comparableKey v)
dictRemoveKeys listOfKeysToRemove =
    \dict ->
        listOfKeysToRemove
            |> List.foldl (\key soFar -> soFar |> FastDict.remove key) dict


rangeCompare : Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range -> Order
rangeCompare a b =
    if a.start.row < b.start.row then
        LT

    else if a.start.row > b.start.row then
        GT

    else
    -- Start row is the same from here on
    if
        a.start.column < b.start.column
    then
        LT

    else if a.start.column > b.start.column then
        GT

    else
    -- Start row and column are the same from here on
    if
        a.end.row < b.end.row
    then
        LT

    else if a.end.row > b.end.row then
        GT

    else
    -- Start row and column, and end row are the same from here on
    if
        a.end.column < b.end.column
    then
        LT

    else if a.end.column > b.end.column then
        GT

    else
        EQ


syntaxFileSanitize : Elm.Syntax.File.File -> Elm.Syntax.File.File
syntaxFileSanitize =
    \syntaxFile ->
        { syntaxFile
            | comments =
                syntaxFile.comments
                    |> List.sortBy (\(Elm.Syntax.Node.Node range _) -> ( range.start.row, range.start.column ))
        }


{-| A thing to report in a given [file](#FileTarget) and range,
suggesting [fixes](#Fix) and giving details with hints
-}
type alias Error =
    { target : FileTarget
    , range : Elm.Syntax.Range.Range
    , message : String
    , details : List String
    , fix : List Fix
    }


{-| A fresh [`Cache`](#Cache) for a fresh session (or single [`run`](#run))
-}
cacheEmpty : Cache
cacheEmpty =
    { elmJsonKnowledge = Nothing
    , directDependenciesKnowledge = Nothing
    , moduleKnowledgesByPath = FastDict.empty
    , extraFileKnowledgesByPath = FastDict.empty
    }


{-| There are situations where you don't want review rules to report errors:

  - You copied and updated over an external library because one of your needs wasn't met, and you don't want to modify it more than necessary
  - Your project contains generated source code which isn't supposed to be read and or over which you have less control
  - You wrote a review that is very specific and should only be applied for a portion of your codebase

Note that `tests/` directory is never inspected

-}
ignoreErrorsForFilesWhere : (String -> Bool) -> (Review -> Review)
ignoreErrorsForFilesWhere filterOut =
    \review ->
        { review
            | ignoreErrorsForFiles =
                \path ->
                    (path |> review.ignoreErrorsForFiles) || (path |> filterOut)
        }


{-| Review a project and return the errors reported by the given rules.

Note that you won't need to use this function when writing a rule. You should
only need it if you try to make `elm-review` run in a new environment.

    import Review exposing (Review)

    config : List Review
    config =
        [ Some.review
        , Some.Other.review
        ]

    project =
        { modules =
            [ { path = "src/A.elm", source = "module A exposing (a)\na = 1" }
            , { path = "src/B.elm", source = "module B exposing (b)\nb = 1" }
            ]
        , ...
        }

    doReview =
        let
            { errorsByPath, cache } =
                Rule.review config project
        in
        doSomethingWithTheseValues

(elm/core is automatically part of every project)

updated internal cache to make it faster to re-run the rules on the same project.
If you plan on re-reviewing with the same rules and project, for instance to
review the project after a file has changed, you may want to store the rules in
your `Model`.

-}
run :
    Review
    ->
        ({ elmJson : { source : String, project : Elm.Project.Project }
         , directDependencies : List { elmJson : String, docsJson : String }
         , addedOrChangedFiles : List { path : String, source : String }
         , removedFilePaths : List String
         , cache : Cache
         }
         ->
            { errorsByPath : FastDict.Dict String (List { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Fix })
            , cache : Cache
            }
        )
run review =
    \project ->
        let
            runResult :
                { errorsByPath : FastDict.Dict String (List { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Fix })
                , cache : Cache
                }
            runResult =
                project |> review.run
        in
        { cache = runResult.cache
        , errorsByPath =
            runResult.errorsByPath
                |> FastDict.foldl
                    (\path errors soFar ->
                        if path |> review.ignoreErrorsForFiles then
                            soFar

                        else
                            soFar |> FastDict.insert path errors
                    )
                    FastDict.empty
        }


{-| Cache for the result of the analysis of an inspected project part (modules, elm.json, extra files, direct and indirect dependencies).
-}
type alias Cache =
    { elmJsonKnowledge : Maybe KnowledgeGeneric
    , directDependenciesKnowledge : Maybe KnowledgeGeneric
    , moduleKnowledgesByPath : FastDict.Dict String KnowledgeGeneric
    , extraFileKnowledgesByPath : FastDict.Dict String KnowledgeGeneric
    }


{-| Go through parent, then children and their children, depth-first and collect info along the way
-}
expressionFold :
    (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> (folded -> folded))
    -> folded
    ->
        (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
         -> folded
        )
expressionFold reduce initialFolded =
    \expressionNode ->
        (expressionNode |> expressionSubs)
            |> List.foldl
                (\sub soFar ->
                    sub |> expressionFold reduce soFar
                )
                (initialFolded |> reduce expressionNode)


{-| In case you need the read the source in a range verbatim.
This can be nice to keep the user's formatting if you just move code around
-}
sourceExtractInRange : Elm.Syntax.Range.Range -> (String -> String)
sourceExtractInRange range =
    \string ->
        string
            |> String.lines
            |> List.drop (range.start.row - 1)
            |> List.take (range.end.row - range.start.row + 1)
            |> ListExtra.lastMap (Unicode.left (range.end.column - 1))
            |> String.join "\n"
            |> Unicode.dropLeft (range.start.column - 1)


{-| All surface-level child expressions
-}
expressionSubs : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
expressionSubs node =
    case Elm.Syntax.Node.value node of
        Elm.Syntax.Expression.Application expressions ->
            expressions

        Elm.Syntax.Expression.ListExpr elements ->
            elements

        Elm.Syntax.Expression.RecordExpr fields ->
            List.map (\(Elm.Syntax.Node.Node _ ( _, expr )) -> expr) fields

        Elm.Syntax.Expression.RecordUpdateExpression _ setters ->
            List.map (\(Elm.Syntax.Node.Node _ ( _, expr )) -> expr) setters

        Elm.Syntax.Expression.ParenthesizedExpression expr ->
            [ expr ]

        Elm.Syntax.Expression.OperatorApplication _ direction left right ->
            case direction of
                Elm.Syntax.Infix.Left ->
                    [ left, right ]

                Elm.Syntax.Infix.Right ->
                    [ right, left ]

                Elm.Syntax.Infix.Non ->
                    [ left, right ]

        Elm.Syntax.Expression.IfBlock cond then_ else_ ->
            [ cond, then_, else_ ]

        Elm.Syntax.Expression.LetExpression letIn ->
            List.foldr
                (\declaration soFar ->
                    case Elm.Syntax.Node.value declaration of
                        Elm.Syntax.Expression.LetFunction function ->
                            (function.declaration
                                |> Elm.Syntax.Node.value
                                |> .expression
                            )
                                :: soFar

                        Elm.Syntax.Expression.LetDestructuring _ expr ->
                            expr :: soFar
                )
                [ letIn.expression ]
                letIn.declarations

        Elm.Syntax.Expression.CaseExpression caseOf ->
            caseOf.expression
                :: List.map (\( _, caseExpression ) -> caseExpression) caseOf.cases

        Elm.Syntax.Expression.LambdaExpression lambda ->
            [ lambda.expression ]

        Elm.Syntax.Expression.TupledExpression expressions ->
            expressions

        Elm.Syntax.Expression.Negation expr ->
            [ expr ]

        Elm.Syntax.Expression.RecordAccess expr _ ->
            [ expr ]

        Elm.Syntax.Expression.PrefixOperator _ ->
            []

        Elm.Syntax.Expression.Operator _ ->
            []

        Elm.Syntax.Expression.Integer _ ->
            []

        Elm.Syntax.Expression.Hex _ ->
            []

        Elm.Syntax.Expression.Floatable _ ->
            []

        Elm.Syntax.Expression.Literal _ ->
            []

        Elm.Syntax.Expression.CharLiteral _ ->
            []

        Elm.Syntax.Expression.UnitExpr ->
            []

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            []

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            []

        Elm.Syntax.Expression.GLSLExpression _ ->
            []


{-| The set of modules in [`Elm-Project.Exposed`](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Project#Exposed)
(the `exposed-modules` field in a package elm.json)
-}
packageElmJsonExposedModules : Elm.Project.Exposed -> Set Elm.Syntax.ModuleName.ModuleName
packageElmJsonExposedModules =
    \exposed ->
        case exposed of
            Elm.Project.ExposedList list ->
                list |> List.map elmJsonModuleNameToSyntax |> Set.fromList

            Elm.Project.ExposedDict dict ->
                dict
                    |> List.concatMap (\( _, moduleNames ) -> moduleNames)
                    |> List.map elmJsonModuleNameToSyntax
                    |> Set.fromList


elmJsonModuleNameToSyntax : Elm.Module.Name -> Elm.Syntax.ModuleName.ModuleName
elmJsonModuleNameToSyntax =
    \elmJsonModuleName ->
        elmJsonModuleName |> Elm.Module.toString |> String.split "."


{-| The module header name + range
-}
moduleHeaderNameNode : Elm.Syntax.Node.Node Elm.Syntax.Module.Module -> Elm.Syntax.Node.Node Elm.Syntax.ModuleName.ModuleName
moduleHeaderNameNode =
    \(Elm.Syntax.Node.Node _ moduleHeader) ->
        case moduleHeader of
            Elm.Syntax.Module.NormalModule data ->
                data.moduleName

            Elm.Syntax.Module.PortModule data ->
                data.moduleName

            Elm.Syntax.Module.EffectModule data ->
                data.moduleName


{-| The module header [exposing part](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Exposing#Exposing) + range
-}
moduleHeaderExposing : Elm.Syntax.Node.Node Elm.Syntax.Module.Module -> Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing
moduleHeaderExposing =
    \(Elm.Syntax.Node.Node _ moduleHeader) ->
        case moduleHeader of
            Elm.Syntax.Module.NormalModule moduleHeaderData ->
                moduleHeaderData.exposingList

            Elm.Syntax.Module.PortModule moduleHeaderData ->
                moduleHeaderData.exposingList

            Elm.Syntax.Module.EffectModule moduleHeaderData ->
                moduleHeaderData.exposingList


{-| The module's documentation comment at the top of the file, which can be `Nothing`.
`elm-syntax` has a weird way of giving you the comments of a file, this helper should make it easier
-}
moduleHeaderDocumentation : Elm.Syntax.File.File -> Maybe (Elm.Syntax.Node.Node String)
moduleHeaderDocumentation =
    \syntaxFile ->
        let
            cutOffLine : Int
            cutOffLine =
                case syntaxFile.imports of
                    [] ->
                        case syntaxFile.declarations of
                            [] ->
                                -- Should not happen, as every module should have at least one declaration
                                0

                            firstDeclaration :: _ ->
                                (Elm.Syntax.Node.range firstDeclaration).start.row

                    firstImport :: _ ->
                        (Elm.Syntax.Node.range firstImport).start.row
        in
        findModuleDocumentationBeforeCutOffLine cutOffLine syntaxFile.comments


findModuleDocumentationBeforeCutOffLine : Int -> List (Elm.Syntax.Node.Node String) -> Maybe (Elm.Syntax.Node.Node String)
findModuleDocumentationBeforeCutOffLine cutOffLine comments =
    case comments of
        [] ->
            Nothing

        comment :: restOfComments ->
            let
                (Elm.Syntax.Node.Node range content) =
                    comment
            in
            if range.start.row > cutOffLine then
                Nothing

            else if String.startsWith "{-|" content then
                Just comment

            else
                findModuleDocumentationBeforeCutOffLine cutOffLine restOfComments


{-| Represents (part of a) fix that will be applied to a file's source code in order to
automatically fix a review error.
-}
type Fix
    = FixRangeReplacement { range : Elm.Syntax.Range.Range, replacement : String }


{-| Remove the code in between a range
-}
fixRemoveRange : Elm.Syntax.Range.Range -> Fix
fixRemoveRange rangeToRemove =
    fixReplaceRangeBy rangeToRemove ""


{-| Replace the code in between a range by some other code
-}
fixReplaceRangeBy : Elm.Syntax.Range.Range -> String -> Fix
fixReplaceRangeBy range replacement =
    FixRangeReplacement { range = range, replacement = replacement }


{-| Insert some code at the given position
-}
fixInsertAt : Elm.Syntax.Range.Location -> String -> Fix
fixInsertAt location toInsert =
    fixReplaceRangeBy { start = location, end = location } toInsert


{-| Unexpected cased when trying to apply [`Fix`](#Fix)es
-}
type FixError
    = AfterFixIsUnchanged
    | FixHasCollisionsInRanges


fixRange : Fix -> Elm.Syntax.Range.Range
fixRange fix =
    case fix of
        FixRangeReplacement replace ->
            replace.range


comparePosition : Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location -> Order
comparePosition a b =
    case compare a.row b.row of
        EQ ->
            compare a.column b.column

        LT ->
            LT

        GT ->
            GT


{-| Try to apply a set of [`Fix`](#Fix)es to the relevant file source,
potentially failing with a [`FixError`](#FixError)
-}
fixFile : List Fix -> String -> Result FixError String
fixFile fixes sourceCode =
    if containRangeCollisions fixes then
        FixHasCollisionsInRanges |> Err

    else
        let
            resultAfterFix : String
            resultAfterFix =
                fixes
                    |> List.sortWith
                        (\a b ->
                            -- flipped order
                            comparePosition (b |> fixRange |> .start) (a |> fixRange |> .start)
                        )
                    |> List.foldl applyFix (sourceCode |> String.lines)
                    |> String.join "\n"
        in
        if sourceCode == resultAfterFix then
            AfterFixIsUnchanged |> Err

        else
            resultAfterFix |> Ok


applyFix : Fix -> (List String -> List String)
applyFix fixToApply =
    \lines ->
        case fixToApply of
            FixRangeReplacement replace ->
                let
                    linesBefore : List String
                    linesBefore =
                        List.take (replace.range.start.row - 1) lines

                    linesAfter : List String
                    linesAfter =
                        List.drop replace.range.end.row lines

                    startLine : String
                    startLine =
                        ListExtra.elementAtIndex (replace.range.start.row - 1) lines
                            |> Maybe.withDefault ""
                            |> Unicode.left (replace.range.start.column - 1)

                    endLine : String
                    endLine =
                        ListExtra.elementAtIndex (replace.range.end.row - 1) lines
                            |> Maybe.withDefault ""
                            |> Unicode.dropLeft (replace.range.end.column - 1)
                in
                [ linesBefore
                , replace.replacement
                    |> String.lines
                    |> ListExtra.headMap (\replacementFirstLine -> startLine ++ replacementFirstLine)
                    |> ListExtra.lastMap (\replacementLastLine -> replacementLastLine ++ endLine)
                , linesAfter
                ]
                    |> List.concat


containRangeCollisions : List Fix -> Bool
containRangeCollisions fixes =
    fixes |> List.map fixRange |> ListExtra.anyPair rangesCollide


rangesCollide : Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range -> Bool
rangesCollide a b =
    case comparePosition a.end b.start of
        LT ->
            False

        EQ ->
            False

        GT ->
            case comparePosition b.end a.start of
                LT ->
                    False

                EQ ->
                    False

                GT ->
                    True


{-| Convenience [`Codec`](https://dark.elm.dmy.fr/packages/miniBill/elm-codec/latest/)
for an [`Elm.Syntax.ModuleName.ModuleName`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-ModuleName#ModuleName)
-}
moduleNameCodec : Codec.Codec Elm.Syntax.ModuleName.ModuleName
moduleNameCodec =
    Codec.list Codec.string


{-| Convenience [`Codec`](https://dark.elm.dmy.fr/packages/miniBill/elm-codec/latest/)
for an [`Elm.Syntax.Range.Range`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Range#Range)
-}
rangeCodec : Codec.Codec Elm.Syntax.Range.Range
rangeCodec =
    Codec.object (\start end -> { start = start, end = end })
        |> Codec.field "start" .start locationCodec
        |> Codec.field "end" .end locationCodec
        |> Codec.buildObject


{-| Convenience [`Codec`](https://dark.elm.dmy.fr/packages/miniBill/elm-codec/latest/)
for an [`Elm.Syntax.Range.Location`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Range#Location)
-}
locationCodec : Codec.Codec Elm.Syntax.Range.Location
locationCodec =
    Codec.object (\row column -> { row = row, column = column })
        |> Codec.field "row" .row Codec.int
        |> Codec.field "colum" .column Codec.int
        |> Codec.buildObject
