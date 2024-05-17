module Review exposing
    ( ignoreErrorsForPathsWhere
    , create
    , inspectElmJson, inspectDirectDependencies, inspectExtraFile, inspectModule
    , Error, elmJsonPath
    , SourceEdit(..), insertAt, removeRange, replaceRange
    , expressionSubs, expressionFold
    , moduleHeaderDocumentation, moduleHeaderNameNode, moduleHeaderExposingNode
    , sourceExtractInRange
    , packageElmJsonExposedModules
    , Inspect(..), Review, run, sourceApplyEdits, SourceEditError(..), Run(..)
    )

{-| A review inspects modules, `elm.json`, dependencies and extra files like `README.md` from your project
and uses the combined knowledge to report problems.

@docs ignoreErrorsForPathsWhere

@docs create


# inspecting

Collect knowledge from parts of the project.

@docs inspectElmJson, inspectDirectDependencies, inspectExtraFile, inspectModule


# reporting

@docs Error, elmJsonPath
@docs SourceEdit, insertAt, removeRange, replaceRange


# convenience helpers

@docs expressionSubs, expressionFold
@docs moduleHeaderDocumentation, moduleHeaderNameNode, moduleHeaderExposingNode
@docs sourceExtractInRange
@docs packageElmJsonExposedModules


# running

Make `elm-review-mini` run in a new environment

@docs Inspect, Review, run, sourceApplyEdits, SourceEditError, Run

-}

import Elm.Docs
import Elm.Module
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
import FastDict
import FastDictLocalExtra
import ListLocalExtra
import Set exposing (Set)
import Unicode


{-| A construct that can inspect your project files and report errors, see [`Review.create`](Review#create)
-}
type alias Review =
    { ignoreErrorsForPathsWhere : String -> Bool
    , run : Run
    }


{-| A function that inspects, folds knowledge and reports errors in one go, and provides
a recursively-defined future run function that already has the calculated knowledges cached.

Does it need to be so complicated?

Different reviews can have different knowledge types,
so how can we put all of them in a single list?
No problem, hide this parameter by only storing the combined run function from project files to errors.

But how do we implement caching this way
(only re-run inspections for files that changed, otherwise take the already computed knowledge parts)?
I know about roughly three options:

  - require users to provide encode/decode pairs to some generic structure.
    The simplest and fastest seems to be [miniBill/elm-codec](https://dark.elm.dmy.fr/packages/miniBill/elm-codec/latest/)
      - the ability to use the encoded cache at any time enables write to disk
        (not a goal of `elm-review-mini` but having the option is nice)
      - the ability to show the encoded cache in the elm debugger
        (undeniably cool, though that only works in the browser. `Debug.log` is likely a nice enough alternative)
      - the ability to export the produced json for external use
        (not a goal of `elm-review-mini` but undeniably cool, similar to `elm-review` insights/extract although less explicit)
      - quite a burden users should preferably not bear

  - use js shenanigans to effectively "cast specific context type to any" and "cast any to specific context type"
    similar to [linsyking/elm-anytype](https://dark.elm.dmy.fr/packages/linsyking/elm-anytype/latest/)
      - faster than any possible alternative
      - simpler elm internals than any possible alternative
      - the highest possible degree of danger, as incorrect casting is always possible
        (+ no way to control that `Review.run` users wire them correctly)
      - custom embeds of running reviews (e.g. if you wanted to create a browser playground)
        always need additional js hacking which to me is deal breaking

  - provide a future function as a result which knows about the
    previously calculated knowledges.
    I was surprised to find that it's almost trivial to implement and even cuts down
    internal complexity compared to something like the codec one.
    If you're intrigued and have a week,
    some smart folks have written [an entertaining dialog-blog-ish series about these "Jeremy's interfaces"](https://discourse.elm-lang.org/t/demystifying-jeremys-interfaces/8834)
      - still very fast (much faster than the codec one)
      - concise internals
      - possibly less flexible for implementing stuff like shared knowledge
        (I much prefer defunctionalization wherever possible)
      - slightly less obvious than the codec one

I didn't expect to ever need to resort to such a complex feature but here we are.
Though I'm always excited to try and experiment with other options (except the codec one which I already tried), issues welcome!

-}
type Run
    = Run
        ({ elmJson : { source : String, project : Elm.Project.Project }
         , directDependencies : List { elmJson : Elm.Project.Project, docsJson : List Elm.Docs.Module }
         , addedOrChangedExtraFiles : List { path : String, source : String }
         , addedOrChangedModules : List { path : String, source : String, syntax : Elm.Syntax.File.File }
         , removedExtraFilePaths : List String
         , removedModulePaths : List String
         }
         ->
            { errorsByPath :
                FastDict.Dict
                    String
                    (List
                        { range : Elm.Syntax.Range.Range
                        , message : String
                        , details : List String
                        , fix : List { path : String, edits : List SourceEdit }
                        }
                    )
            , nextRun : Run
            }
        )


{-| How to collect knowledge from scanning a single part of the project
-}
type Inspect knowledge
    = InspectDirectDependencies
        (List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
         -> knowledge
        )
    | InspectElmJson (Elm.Project.Project -> knowledge)
    | InspectExtraFile ({ path : String, source : String } -> knowledge)
    | InspectModule
        ({ syntax : Elm.Syntax.File.File, source : String, path : String }
         -> knowledge
        )


{-| Relative path to the project elm.json to be used in an [`Error`](#Error) or [expected test error](Review-Test#run).
You could also just use `"elm.json"` which feels a bit brittle.
-}
elmJsonPath : String
elmJsonPath =
    "elm.json"


{-| Collect knowledge from a module file.

The file path, relative to the project's `elm.json`
can also be used as an [`Error`](#Error) target file path.

The raw elm file source can be used
to preserve existing formatting in a range
in case you want to move and copy things around,
see [`sourceExtractInRange`](#sourceExtractInRange).

The `syntax` field contains a tree-like structure which represents your source code:
[`elm-syntax` `File`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/File).
I recommend having the documentation for that package open while writing a review.


### .moduleDefinition

The [module header](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Module) (`module SomeModuleName exposing (a, b)`).
A few helpers: [`moduleHeaderNameNode`](#moduleHeaderNameNode), [`moduleHeaderDocumentation`](#moduleHeaderDocumentation)


### .declarations

The module's
[declaration statements](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`).


### .comments

Function/type/type alias declarations have their possible documentation comment (`{-| -}`) included directly in the syntax data.

All other comments are provided in source order

  - Module documentation (`{-| -}`)
  - Port documentation comments (`{-| -}`)
  - comments (`{- -}` and `--`)

If you only need to access the module documentation, you can use
[`moduleHeaderDocumentation`](#moduleHeaderDocumentation).


### .imports

The module's
[import statements](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Import)
(`import Html as H exposing (div)`) in order of their definition

An example review that forbids importing both `Element` (`elm-ui`) and
`Html.Styled` (`elm-css`) in the same module:

    import Elm.Syntax.Import
    import Elm.Syntax.Node
    import Elm.Syntax.Range
    import Review
    import Dict

    review : Review.Review
    review =
        Review.create
            { inspect =
                [ Review.inspectModule
                    (\moduleData ->
                        let
                            importedModuleNames : Dict Elm.Syntax.ModuleName.ModuleName Elm.Syntax.Range.Range
                            importedModuleNames =
                                moduleData.syntax.imports
                                    |> List.map
                                        (\(Elm.Syntax.Node.Node _ import_) ->
                                            ( import.moduleName |> Elm.Syntax.Node.value, import.moduleName |> Elm.Syntax.Node.range )
                                        )
                                    |> Dict.fromList
                        in
                        case
                            ( importedModuleNames |> Dict.get [ "Element" ]
                            , importedModuleNames |> Dict.get [ "Html", "Styled" ]
                            )
                        of
                            ( Just elementImportRange, Just _ ) ->
                                Dict.singleton moduleData.path elementImportRange

                        else
                            Dict.empty
                    )
                ]
            , knowledgeMerge = \a b -> Dict.union a b
            , report =
                \invalidImportsByPath ->
                    invalidImportsByPath
                        |> Dict.toList
                        |> List.map
                            (\( path, elementImportRange ) ->
                                { path = moduleData.path
                                , message = "both `elm-ui` and `elm-css` used"
                                , details = [ "At fruits.com, we use `elm-ui` in the dashboard application, and `elm-css` in the rest of the code. We want to use `elm-ui` in our new projects, but in projects using `elm-css`, we don't want to use both libraries to keep things simple." ]
                                , range = elementImportRange
                                , fix = []
                                }
                            )
            }

-}
inspectModule : ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> knowledge) -> Inspect knowledge
inspectModule moduleDataToKnowledge =
    InspectModule moduleDataToKnowledge


{-| Collect knowledge from the [elm.json project config](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Project#Project)
-}
inspectElmJson : (Elm.Project.Project -> knowledge) -> Inspect knowledge
inspectElmJson moduleDataToKnowledge =
    InspectElmJson moduleDataToKnowledge


{-| Collect knowledge from a provided non-elm or elm.json file like README.md or CHANGELOG.md.

The provided file path is relative to the project's `elm.json`
and can also be used as an [`Error`](#Error) target file path

-}
inspectExtraFile : ({ path : String, source : String } -> knowledge) -> Inspect knowledge
inspectExtraFile moduleDataToKnowledge =
    InspectExtraFile moduleDataToKnowledge


{-| Collect knowledge from all [project config and docs](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/) the project directly depends on
-}
inspectDirectDependencies : (List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> knowledge) -> Inspect knowledge
inspectDirectDependencies moduleDataToKnowledge =
    InspectDirectDependencies moduleDataToKnowledge


{-| First, read ["when to write or enable a review"](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-mini#when-to-write-or-enable-a-review).
To write a new [`Review`](#Review) you need to

  - `inspect`: the parts you want to scan to collect knowledge from. See [section inspecting](#inspecting)
  - `knowledgeMerge`: assemble knowledge from multiple you've collected into one
  - `report`: the final evaluation, turning your combined knowledge into a list of [errors](#Error)

Write tests with [`Review.Test`](Review-Test) alongside your implementation
and document your review,
explaining when (not) to enable the review
and adding a few examples of patterns that will (not) be reported.

-}
create :
    { inspect : List (Inspect knowledge)
    , knowledgeMerge : knowledge -> knowledge -> knowledge
    , report : knowledge -> List Error
    }
    -> Review
create review =
    let
        toKnowledges :
            { directDependenciesToKnowledge : List (List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> knowledge)
            , elmJsonToKnowledge : List (Elm.Project.Project -> knowledge)
            , extraFileToKnowledge : List ({ path : String, source : String } -> knowledge)
            , moduleToKnowledge : List ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> knowledge)
            }
        toKnowledges =
            review.inspect |> inspectToToKnowledges

        knowledgesFoldToMaybe : List knowledge -> Maybe knowledge
        knowledgesFoldToMaybe =
            \knowledges ->
                case knowledges of
                    [] ->
                        Nothing

                    one :: others ->
                        others |> List.foldl review.knowledgeMerge one |> Just

        moduleToMaybeKnowledge :
            { path : String, source : String, syntax : Elm.Syntax.File.File }
            -> Maybe { path : String, knowledge : knowledge }
        moduleToMaybeKnowledge moduleFile =
            let
                moduleData : { path : String, source : String, syntax : Elm.Syntax.File.File }
                moduleData =
                    { path = moduleFile.path
                    , source = moduleFile.source
                    , syntax = moduleFile.syntax |> syntaxFileSanitize
                    }
            in
            toKnowledges.moduleToKnowledge
                |> List.map (\f -> f moduleData)
                |> knowledgesFoldToMaybe
                |> Maybe.map
                    (\foldedKnowledge ->
                        { path = moduleFile.path
                        , knowledge = foldedKnowledge
                        }
                    )

        runWithCache : Cache knowledge -> Run
        runWithCache cache =
            -- IGNORE TCO
            Run
                (\project ->
                    let
                        elmJsonKnowledgeAndCache : Maybe knowledge
                        elmJsonKnowledgeAndCache =
                            case cache.elmJsonKnowledge of
                                Just elmJsonKnowledgeCache ->
                                    elmJsonKnowledgeCache |> Just

                                Nothing ->
                                    toKnowledges.elmJsonToKnowledge
                                        |> List.map (\f -> f project.elmJson.project)
                                        |> knowledgesFoldToMaybe

                        directDependenciesKnowledgeAndCache : Maybe knowledge
                        directDependenciesKnowledgeAndCache =
                            case cache.directDependenciesKnowledge of
                                Just knowledge ->
                                    knowledge |> Just

                                Nothing ->
                                    let
                                        directDependenciesData : List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
                                        directDependenciesData =
                                            project.directDependencies
                                                |> List.map (\directDep -> { elmJson = directDep.elmJson, modules = directDep.docsJson })
                                                |> ListLocalExtra.consJust ElmCoreDependency.parsed
                                    in
                                    toKnowledges.directDependenciesToKnowledge
                                        |> List.map (\f -> f directDependenciesData)
                                        |> knowledgesFoldToMaybe

                        moduleKnowledges : List { path : String, knowledge : knowledge }
                        moduleKnowledges =
                            FastDict.merge
                                (\_ new soFar -> soFar |> ListLocalExtra.consJust new)
                                (\_ new _ soFar -> soFar |> ListLocalExtra.consJust new)
                                (\path knowledgeCache soFar ->
                                    soFar |> (::) { path = path, knowledge = knowledgeCache }
                                )
                                (project.addedOrChangedModules
                                    |> FastDictLocalExtra.fromListMap
                                        (\file ->
                                            { key = file.path
                                            , value = file |> moduleToMaybeKnowledge
                                            }
                                        )
                                )
                                (cache.moduleKnowledgeByPath
                                    |> dictRemoveKeys project.removedModulePaths
                                )
                                []

                        extraFileToMaybeKnowledge :
                            { path : String, source : String }
                            -> Maybe { path : String, knowledge : knowledge }
                        extraFileToMaybeKnowledge fileInfo =
                            toKnowledges.extraFileToKnowledge
                                |> List.map (\f -> f fileInfo)
                                |> knowledgesFoldToMaybe
                                |> Maybe.map
                                    (\foldedKnowledge ->
                                        { path = fileInfo.path
                                        , knowledge = foldedKnowledge
                                        }
                                    )

                        extraFilesKnowledges : List { path : String, knowledge : knowledge }
                        extraFilesKnowledges =
                            FastDict.merge
                                (\_ new soFar -> soFar |> ListLocalExtra.consJust new)
                                (\_ new _ soFar -> soFar |> ListLocalExtra.consJust new)
                                (\path knowledgeCache soFar ->
                                    soFar |> (::) { path = path, knowledge = knowledgeCache }
                                )
                                (project.addedOrChangedExtraFiles
                                    |> FastDictLocalExtra.fromListMap
                                        (\file ->
                                            { key = file.path
                                            , value = file |> extraFileToMaybeKnowledge
                                            }
                                        )
                                )
                                (cache.extraFileKnowledgeByPath
                                    |> dictRemoveKeys project.removedExtraFilePaths
                                )
                                []

                        allKnowledges : List knowledge
                        allKnowledges =
                            ((moduleKnowledges |> List.map .knowledge)
                                ++ (extraFilesKnowledges |> List.map .knowledge)
                            )
                                |> ListLocalExtra.consJust directDependenciesKnowledgeAndCache
                                |> ListLocalExtra.consJust elmJsonKnowledgeAndCache
                    in
                    case allKnowledges |> knowledgesFoldToMaybe of
                        Nothing ->
                            { errorsByPath = FastDict.empty, nextRun = runWithCache knowledgeCacheEmpty }

                        Just completeKnowledge ->
                            { errorsByPath =
                                completeKnowledge
                                    |> review.report
                                    |> List.foldl
                                        (\error soFar ->
                                            soFar
                                                |> FastDict.update
                                                    error.path
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
                            , nextRun =
                                runWithCache
                                    { elmJsonKnowledge = elmJsonKnowledgeAndCache
                                    , directDependenciesKnowledge = directDependenciesKnowledgeAndCache
                                    , moduleKnowledgeByPath =
                                        moduleKnowledges
                                            |> FastDictLocalExtra.fromListMap
                                                (\moduleKnowledge ->
                                                    { key = moduleKnowledge.path, value = moduleKnowledge.knowledge }
                                                )
                                    , extraFileKnowledgeByPath =
                                        extraFilesKnowledges
                                            |> FastDictLocalExtra.fromListMap
                                                (\extraFileKnowledge ->
                                                    { key = extraFileKnowledge.path, value = extraFileKnowledge.knowledge }
                                                )
                                    }
                            }
                )
    in
    { ignoreErrorsForPathsWhere = \_ -> False
    , run = runWithCache knowledgeCacheEmpty
    }


knowledgeCacheEmpty : Cache knowledge_
knowledgeCacheEmpty =
    { elmJsonKnowledge = Nothing
    , directDependenciesKnowledge = Nothing
    , moduleKnowledgeByPath = FastDict.empty
    , extraFileKnowledgeByPath = FastDict.empty
    }


inspectToToKnowledges :
    List (Inspect knowledge)
    ->
        { directDependenciesToKnowledge : List (List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> knowledge)
        , elmJsonToKnowledge : List (Elm.Project.Project -> knowledge)
        , extraFileToKnowledge : List ({ path : String, source : String } -> knowledge)
        , moduleToKnowledge : List ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> knowledge)
        }
inspectToToKnowledges =
    \inspect ->
        inspect
            |> List.foldl
                (\inspectSingle soFar ->
                    case inspectSingle of
                        InspectDirectDependencies toKnowledge ->
                            { soFar
                                | directDependenciesToKnowledge =
                                    soFar.directDependenciesToKnowledge |> (::) toKnowledge
                            }

                        InspectElmJson toKnowledge ->
                            { soFar
                                | elmJsonToKnowledge =
                                    soFar.elmJsonToKnowledge |> (::) toKnowledge
                            }

                        InspectExtraFile toKnowledge ->
                            { soFar
                                | extraFileToKnowledge =
                                    soFar.extraFileToKnowledge |> (::) toKnowledge
                            }

                        InspectModule toKnowledge ->
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
                    |> List.sortBy
                        (\(Elm.Syntax.Node.Node range _) ->
                            ( range.start.row, range.start.column )
                        )
        }


{-| A problem to report in a given file and range, possibly suggesting a fix with [`SourceEdit`](#SourceEdit)s

Take the elm compiler errors as inspiration in terms of helpfulness:

  - error `message`: half-sentence on what _is_ undesired here. A user
    that has encountered this error multiple times should know exactly what to do.
    Example: "\`\` is never used" → a user who
    knows the rule knows that a function can be removed and which one
  - error `details`: additional information such as the rationale and suggestions
    for a solution or alternative
  - error report `range`: The region marked as problematic. Make this section as small as
    possible. For instance, in a rule that would forbid
    `Debug.log`, you would want the error to appear at `Debug.log`, not on the whole
    function call

-}
type alias Error =
    { path : String
    , range : Elm.Syntax.Range.Range
    , message : String
    , details : List String
    , fix : List { path : String, edits : List SourceEdit }
    }


{-| Specify where you don't want errors to be reported in:

  - an external library you copied over and don't want to modify more than necessary
  - generated source code which isn't supposed to be read and or over which you have little control
  - you wrote a review that is very specific and should only be applied for a portion of your codebase

`tests/` is never inspected

-}
ignoreErrorsForPathsWhere : (String -> Bool) -> (Review -> Review)
ignoreErrorsForPathsWhere filterOut =
    \review ->
        { review
            | ignoreErrorsForPathsWhere =
                \path ->
                    (path |> review.ignoreErrorsForPathsWhere) || (path |> filterOut)
        }


{-| Review a given project and return the errors reported by the given [`Review`](#Review).

    import Review
    import SomeConvention

    doReview =
        let
            project =
                { addedOrChangedModules =
                    [ { path = "src/A.elm", source = "module A exposing (a)\na = 1", syntax = ... }
                    , { path = "src/B.elm", source = "module B exposing (b)\nb = 1", syntax = ... }
                    ]
                , ...
                }

            runResult =
                Review.run SomeConvention.review project
        in
        doSomethingWith runResult.errorsByPath

(elm/core is automatically part of every project)

The result also contains a [`Run`](#Run) which
internally keeps a cache to make it faster to re-run the review when only some files have changed.
You can store this resulting `nextRun` in your application state type, e.g.

    Review.run
        { run = yourApplicationState.nextRunProvidedByTheLastReviewRun
        , ignoreErrorsForPathsWhere = SomeConvention.review.ignoreErrorsForPathsWhere
        }
        { addedOrChangedModules =
            [ { path = "src/C.elm", source = "module C exposing (c)\nc = 1", syntax = ... }
            ]
        , removedModulePaths = [ "src/B.elm" ]
        , ...
        }

-}
run :
    Review
    ->
        ({ elmJson : { source : String, project : Elm.Project.Project }
         , directDependencies : List { elmJson : Elm.Project.Project, docsJson : List Elm.Docs.Module }
         , addedOrChangedExtraFiles : List { path : String, source : String }
         , addedOrChangedModules : List { path : String, source : String, syntax : Elm.Syntax.File.File }
         , removedExtraFilePaths : List String
         , removedModulePaths : List String
         }
         ->
            { errorsByPath :
                FastDict.Dict
                    String
                    (List
                        { range : Elm.Syntax.Range.Range
                        , message : String
                        , details : List String
                        , fix : List { path : String, edits : List SourceEdit }
                        }
                    )
            , nextRun : Run
            }
        )
run review =
    \project ->
        let
            runResult :
                { errorsByPath :
                    FastDict.Dict
                        String
                        (List
                            { range : Elm.Syntax.Range.Range
                            , message : String
                            , details : List String
                            , fix : List { path : String, edits : List SourceEdit }
                            }
                        )
                , nextRun : Run
                }
            runResult =
                project |> (review.run |> (\(Run r) -> r))
        in
        { nextRun = runResult.nextRun
        , errorsByPath =
            runResult.errorsByPath
                |> FastDict.map
                    (\_ fileReviewErrors ->
                        fileReviewErrors
                            |> List.map
                                (\fileReviewError ->
                                    { range = fileReviewError.range
                                    , message = fileReviewError.message
                                    , details = fileReviewError.details
                                    , fix =
                                        fileReviewError.fix
                                            |> List.filter (\fileEdit -> not (fileEdit.edits |> List.isEmpty))
                                    }
                                )
                    )
        }


{-| Cache for the result of the analysis of an inspected project part (modules, elm.json, extra files, direct dependencies).
-}
type alias Cache knowledge =
    { elmJsonKnowledge : Maybe knowledge
    , directDependenciesKnowledge : Maybe knowledge
    , moduleKnowledgeByPath : FastDict.Dict String knowledge
    , extraFileKnowledgeByPath : FastDict.Dict String knowledge
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
            |> ListLocalExtra.lastMap (Unicode.left (range.end.column - 1))
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
moduleHeaderExposingNode : Elm.Syntax.Node.Node Elm.Syntax.Module.Module -> Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing
moduleHeaderExposingNode =
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


{-| A single edit to be applied to a file's source
in order to fix a [review error](#Error)
-}
type SourceEdit
    = SourceRangeReplacement { range : Elm.Syntax.Range.Range, replacement : String }


{-| Remove the section in between a given range
-}
removeRange : Elm.Syntax.Range.Range -> SourceEdit
removeRange rangeToRemove =
    replaceRange rangeToRemove ""


{-| Replace the section in between a given range by something
-}
replaceRange : Elm.Syntax.Range.Range -> String -> SourceEdit
replaceRange range replacement =
    SourceRangeReplacement { range = range, replacement = replacement }


{-| Insert something at the given position
-}
insertAt : Elm.Syntax.Range.Location -> String -> SourceEdit
insertAt location toInsert =
    replaceRange { start = location, end = location } toInsert


{-| An undesired situation when trying to apply [`SourceEdit`](#SourceEdit)s
-}
type SourceEditError
    = AfterFixIsUnchanged
    | FixHasCollisionsInRanges


fixRange : SourceEdit -> Elm.Syntax.Range.Range
fixRange fix =
    case fix of
        SourceRangeReplacement replace ->
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


{-| Try to apply a set of [`SourceEdit`](#SourceEdit)es to the relevant file source,
potentially failing with a [`SourceEditError`](#SourceEditError)
-}
sourceApplyEdits : List SourceEdit -> String -> Result SourceEditError String
sourceApplyEdits fixes sourceCode =
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
                    |> List.foldl applyFixSingle (sourceCode |> String.lines)
                    |> String.join "\n"
        in
        if sourceCode == resultAfterFix then
            AfterFixIsUnchanged |> Err

        else
            resultAfterFix |> Ok


applyFixSingle : SourceEdit -> (List String -> List String)
applyFixSingle fixToApply =
    \lines ->
        case fixToApply of
            SourceRangeReplacement replace ->
                let
                    linesBefore : List String
                    linesBefore =
                        List.take (replace.range.start.row - 1) lines

                    linesAfter : List String
                    linesAfter =
                        List.drop replace.range.end.row lines

                    startLine : String
                    startLine =
                        ListLocalExtra.elementAtIndex (replace.range.start.row - 1) lines
                            |> Maybe.withDefault ""
                            |> Unicode.left (replace.range.start.column - 1)

                    endLine : String
                    endLine =
                        ListLocalExtra.elementAtIndex (replace.range.end.row - 1) lines
                            |> Maybe.withDefault ""
                            |> Unicode.dropLeft (replace.range.end.column - 1)
                in
                [ linesBefore
                , replace.replacement
                    |> String.lines
                    |> ListLocalExtra.headMap (\replacementFirstLine -> startLine ++ replacementFirstLine)
                    |> ListLocalExtra.lastMap (\replacementLastLine -> replacementLastLine ++ endLine)
                , linesAfter
                ]
                    |> List.concat


containRangeCollisions : List SourceEdit -> Bool
containRangeCollisions fixes =
    fixes |> List.map fixRange |> ListLocalExtra.anyPair rangesCollide


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
