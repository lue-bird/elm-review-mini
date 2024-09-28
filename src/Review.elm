module Review exposing
    ( ignoreErrorsForPathsWhere
    , create
    , inspectElmJson, inspectDirectDependencies, inspectExtraFile, inspectModule
    , Error, elmJsonPath
    , SourceEdit(..), insertAt, removeRange, replaceRange
    , expressionSubs, expressionSubsWithBindings, patternBindings, patternSubs, typeSubs
    , moduleHeaderDocumentation
    , determineModuleOrigin, importsToExplicit
    , moduleExposes, topLevelExposeListToExposes, moduleInterfaceExposes
    , sourceExtractInRange, sourceRangesOf
    , packageElmJsonExposedModules
    , test, applicationConfigurationMinimal, ExpectedErrorRange(..)
    , Inspect(..), Review, run, sourceApplyEdits, SourceEditError(..), Run(..)
    )

{-| A review inspects modules, `elm.json`, dependencies and extra files like `README.md` from your project
and uses the combined knowledge to report problems.

@docs ignoreErrorsForPathsWhere

Everything below is intended for writing a new review

@docs create


# inspecting

Collect knowledge from parts of the project.

@docs inspectElmJson, inspectDirectDependencies, inspectExtraFile, inspectModule


# reporting

@docs Error, elmJsonPath
@docs SourceEdit, insertAt, removeRange, replaceRange

If you want to insert
bigger pieces of dynamic elm code,
I recommend either using [`the-sett/elm-syntax-dsl`](https://dark.elm.dmy.fr/packages/the-sett/elm-syntax-dsl/latest/)
or [`mdgriffith/elm-codegen`](https://dark.elm.dmy.fr/packages/mdgriffith/elm-codegen/latest/)


# convenience helpers

Various helpers for [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/)
and other project data that is often useful when writing a review.

For example,
by only listing all immediate sub-parts of a piece of syntax you can choose to traverse it however you need to.
E.g. to find all `as` pattern ranges

    findAllAsPatternRanges :
        Elm.Syntax.Node.Node ELm.Syntax.Pattern.Pattern
        -> List ELm.Syntax.Range.Range
    findAllAsPatternRanges patternNode =
        (case patternNode of
            Elm.Syntax.Node.Node asRange (Elm.Syntax.Pattern.AsPattern _ _) ->
                [ asRange ]

            _ ->
                []
        )
            ++ (patternNode |> Review.patternSubs |> List.concatMap findAllAsPatternRanges)

This fine control allows you to e.g. skip visiting certain parts that you already accounted for.

@docs expressionSubs, expressionSubsWithBindings, patternBindings, patternSubs, typeSubs

@docs moduleHeaderDocumentation
@docs determineModuleOrigin, importsToExplicit
@docs moduleExposes, topLevelExposeListToExposes, moduleInterfaceExposes
@docs sourceExtractInRange, sourceRangesOf
@docs packageElmJsonExposedModules

Suggestions to add, remove or change helpers welcome!


# testing

Using [`elm-explorations/test`](https://dark.elm.dmy.fr/packages/elm-explorations/test/latest/).

Implementing a review works really well in a Test-Driven loop:

  - write a failing test before writing any code. Run it to make sure that it is failing.
    Also write tests to ensure cases _similar_ to what you want to report are not reported.
    For instance, if you wish to report uses of variables named `foo`, write a test
    that ensures that the use of variables named differently does not get reported
  - write simple (almost stupid) review code to make the test pass,
    using `Debug.todo` etc to cut corner cases
  - run the tests again and make sure that both new and previous tests are passing

Then repeat for every case that needs to be handled.
Ideally, by only reading through the test titles, someone else should be able to
recreate the rule you are testing.

Tests are cheap and it is better to have too many tests rather than too few,
since the behavior of a review rarely changes drastically.

If you like putting `Debug.log`s in your code as much as I do,
run your tests with [`elm-test-rs`](https://github.com/mpizenberg/elm-test-rs).

@docs test, applicationConfigurationMinimal, ExpectedErrorRange


# running

Make `elm-review-mini` run in a new environment

@docs Inspect, Review, run, sourceApplyEdits, SourceEditError, Run

-}

import Ansi
import Diff
import Elm.Docs
import Elm.Module
import Elm.Parser
import Elm.Project
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Infix
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import ElmCoreDependency
import ElmJson.LocalExtra
import Expect
import FastDict
import FastDictLocalExtra
import FastSet
import FastSetLocalExtra
import Json.Decode
import ListLocalExtra
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
                        , fixEditsByPath : FastDict.Dict String (List SourceEdit)
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
    | InspectElmJson ({ source : String, project : Elm.Project.Project } -> knowledge)
    | InspectExtraFile ({ path : String, source : String } -> knowledge)
    | InspectModule
        ({ syntax : Elm.Syntax.File.File, source : String, path : String }
         -> knowledge
        )


{-| Relative path to the project elm.json to be used in an [`Error`](#Error) or [expected test error](Review#test).
You could also just use `"elm.json"` which feels a bit brittle.
-}
elmJsonPath : String
elmJsonPath =
    "elm.json"


{-| Collect knowledge from a module file.

The path is relative to the project's `elm.json`
and can be used as an [`Error`](#Error) target file path.

The raw elm file source can be used
to preserve existing formatting in a range
in case you want to move and copy things around,
see [`sourceExtractInRange`](#sourceExtractInRange).

The `syntax` field contains a tree-like structure which represents your source code:
[`elm-syntax` `File`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/File).
I recommend having the documentation for that package open while writing a review.


### syntax.moduleDefinition

The [module header](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Module) (`module SomeModuleName exposing (a, b)`).


### syntax.declarations

The module's
[declaration statements](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`).


### syntax.comments

  - Module documentation (`{-| -}`)
  - Port documentation comments (`{-| -}`)
  - comments (`{- -}` and `--`)

provided in source order.
Documentation comments (`{-| -}`) of value/function/choice type/type alias declarations
can be found directly in their syntax data.

If you only need the module documentation, use
[`moduleHeaderDocumentation`](#moduleHeaderDocumentation).


### syntax.imports

The module's
[import statements](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Import)
(`import Html as H exposing (div)`) in order of their definition.
A nice helper to get the exposes: [`topLevelExposeListToExposes`](#topLevelExposeListToExposes)

An example review that forbids importing both `Element` (`elm-ui`) and
`Html.Styled` (`elm-css`) in the same module:

    import Elm.Syntax.Import
    import Elm.Syntax.Node
    import Elm.Syntax.Range
    import Review
    import FastDict

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
                                    |> FastDict.fromList
                        in
                        case
                            ( importedModuleNames |> Dict.get [ "Element" ]
                            , importedModuleNames |> Dict.get [ "Html", "Styled" ]
                            )
                        of
                            ( Just elementImportRange, Just _ ) ->
                                FastDict.singleton moduleData.path elementImportRange

                        else
                            FastDict.empty
                    )
                ]
            , knowledgeMerge = \a b -> FastDict.union a b
            , report =
                \invalidImportsByPath ->
                    invalidImportsByPath
                        |> FastDict.toList
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

I strongly recommend [`FastDict`](https://dark.elm.dmy.fr/packages/miniBill/elm-fast-dict/latest/)
for your writing your review instead of `Dict`
because `Dict.union` and friends have performance footguns you shouldn't need to worry about

-}
inspectModule :
    ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> knowledge)
    -> Inspect knowledge
inspectModule moduleDataToKnowledge =
    InspectModule moduleDataToKnowledge


{-| Collect knowledge from the [elm.json project config](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Project#Project).
You can use the raw source to [search for a section](#sourceRangesOf) you want to highlight in an [`Error`](#Error)
-}
inspectElmJson :
    ({ source : String, project : Elm.Project.Project } -> knowledge)
    -> Inspect knowledge
inspectElmJson moduleDataToKnowledge =
    InspectElmJson moduleDataToKnowledge


{-| Collect knowledge from an available files like README.md or CHANGELOG.md that aren't source modules or the elm.json.
The provided path is relative to the project's `elm.json`
and can be used as an [`Error`](#Error) target file path
-}
inspectExtraFile : ({ path : String, source : String } -> knowledge) -> Inspect knowledge
inspectExtraFile moduleDataToKnowledge =
    InspectExtraFile moduleDataToKnowledge


{-| Collect knowledge from all [project configs](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Project#Project)
and [module docs](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Docs#Module)
the project directly depends on
-}
inspectDirectDependencies :
    (List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> knowledge)
    -> Inspect knowledge
inspectDirectDependencies moduleDataToKnowledge =
    InspectDirectDependencies moduleDataToKnowledge


{-| First, read ["when to write or enable a review"](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-mini#when-to-write-or-enable-a-review).
To write a new [`Review`](#Review) you need to

  - `inspect`: the parts you want to scan to collect knowledge from. See [section inspecting](#inspecting)
  - `knowledgeMerge`: assemble knowledge from multiple you've collected into one
  - `report`: the final evaluation, turning your combined knowledge into a list of [errors](#Error)

Write [tests](#testing) alongside your implementation
and document your review,
explaining when (not) to enable the review
and adding examples of patterns that will (not) be reported

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
            , elmJsonToKnowledge : List ({ source : String, project : Elm.Project.Project } -> knowledge)
            , extraFileToKnowledge : List ({ path : String, source : String } -> knowledge)
            , moduleToKnowledge : List ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> knowledge)
            }
        toKnowledges =
            review.inspect |> inspectToToKnowledges

        knowledgesFoldMapToMaybe : (element -> knowledge) -> List element -> Maybe knowledge
        knowledgesFoldMapToMaybe elementToKnowledge knowledges =
            case knowledges of
                [] ->
                    Nothing

                one :: others ->
                    others
                        |> List.foldl
                            (\element soFar ->
                                soFar |> review.knowledgeMerge (element |> elementToKnowledge)
                            )
                            (one |> elementToKnowledge)
                        |> Just

        moduleToMaybeKnowledge :
            { path : String, source : String, syntax : Elm.Syntax.File.File }
            -> Maybe { path : String, knowledge : knowledge }
        moduleToMaybeKnowledge moduleFile =
            toKnowledges.moduleToKnowledge
                |> knowledgesFoldMapToMaybe (\f -> f moduleFile)
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
                                        |> knowledgesFoldMapToMaybe (\f -> f project.elmJson)

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
                                        |> knowledgesFoldMapToMaybe (\f -> f directDependenciesData)

                        moduleKnowledges : List { path : String, knowledge : knowledge }
                        moduleKnowledges =
                            FastDict.merge
                                (\_ new soFar -> soFar |> ListLocalExtra.consJust new)
                                (\_ new _ soFar -> soFar |> ListLocalExtra.consJust new)
                                (\path knowledgeCache soFar ->
                                    { path = path, knowledge = knowledgeCache } :: soFar
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
                                |> knowledgesFoldMapToMaybe (\f -> f fileInfo)
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
                                    { path = path, knowledge = knowledgeCache } :: soFar
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
                    case allKnowledges |> knowledgesFoldMapToMaybe Basics.identity of
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
                                                        { message = error.message
                                                        , details = error.details
                                                        , range = error.range
                                                        , fixEditsByPath = error.fix |> fixToFileEditsByPath
                                                        }
                                                            :: (errorsForPathSoFar |> Maybe.withDefault [])
                                                            |> Just
                                                    )
                                        )
                                        FastDict.empty
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


fixToFileEditsByPath :
    List { path : String, edits : List SourceEdit }
    -> FastDict.Dict String (List SourceEdit)
fixToFileEditsByPath fix =
    fix
        |> List.foldl
            (\fileFix soFar ->
                soFar
                    |> FastDict.update fileFix.path
                        (\editsSoFar ->
                            case fileFix.edits ++ (editsSoFar |> Maybe.withDefault []) of
                                [] ->
                                    Nothing

                                edit0 :: edit1Up ->
                                    (edit0 :: edit1Up) |> Just
                        )
            )
            FastDict.empty


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
        , elmJsonToKnowledge : List ({ source : String, project : Elm.Project.Project } -> knowledge)
        , extraFileToKnowledge : List ({ path : String, source : String } -> knowledge)
        , moduleToKnowledge : List ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> knowledge)
        }
inspectToToKnowledges inspect =
    inspect
        |> List.foldl
            (\inspectSingle soFar ->
                case inspectSingle of
                    InspectDirectDependencies toKnowledge ->
                        { directDependenciesToKnowledge =
                            toKnowledge :: soFar.directDependenciesToKnowledge
                        , elmJsonToKnowledge = soFar.elmJsonToKnowledge
                        , extraFileToKnowledge = soFar.extraFileToKnowledge
                        , moduleToKnowledge = soFar.moduleToKnowledge
                        }

                    InspectElmJson toKnowledge ->
                        { elmJsonToKnowledge =
                            toKnowledge :: soFar.elmJsonToKnowledge
                        , directDependenciesToKnowledge = soFar.directDependenciesToKnowledge
                        , extraFileToKnowledge = soFar.extraFileToKnowledge
                        , moduleToKnowledge = soFar.moduleToKnowledge
                        }

                    InspectExtraFile toKnowledge ->
                        { extraFileToKnowledge =
                            toKnowledge :: soFar.extraFileToKnowledge
                        , directDependenciesToKnowledge = soFar.directDependenciesToKnowledge
                        , elmJsonToKnowledge = soFar.elmJsonToKnowledge
                        , moduleToKnowledge = soFar.moduleToKnowledge
                        }

                    InspectModule toKnowledge ->
                        { moduleToKnowledge =
                            toKnowledge :: soFar.moduleToKnowledge
                        , directDependenciesToKnowledge = soFar.directDependenciesToKnowledge
                        , elmJsonToKnowledge = soFar.elmJsonToKnowledge
                        , extraFileToKnowledge = soFar.extraFileToKnowledge
                        }
            )
            { directDependenciesToKnowledge = []
            , elmJsonToKnowledge = []
            , extraFileToKnowledge = []
            , moduleToKnowledge = []
            }


dictRemoveKeys : List comparableKey -> (FastDict.Dict comparableKey v -> FastDict.Dict comparableKey v)
dictRemoveKeys listOfKeysToRemove dict =
    listOfKeysToRemove
        |> List.foldl FastDict.remove dict


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
ignoreErrorsForPathsWhere filterOut review =
    { run = review.run
    , ignoreErrorsForPathsWhere =
        \path ->
            (path |> review.ignoreErrorsForPathsWhere) || (path |> filterOut)
    }


{-| Review a given project and return the errors reported by the given [`Review`](#Review)
as a [`FastDict`](https://dark.elm.dmy.fr/packages/miniBill/elm-fast-dict/latest/)
along with a next [`Run`](#Run) that uses cached knowledge wherever it can.

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

The resulting next [`Run`](#Run) internally keeps a cache to make it faster to re-run the review when only some files have changed.
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
                        , fixEditsByPath : FastDict.Dict String (List SourceEdit)
                        }
                    )
            , nextRun : Run
            }
        )
run review project =
    project |> (review.run |> (\(Run r) -> r))


{-| Cache for the result of the analysis of an inspected project part (modules, elm.json, extra files, direct dependencies).
-}
type alias Cache knowledge =
    { elmJsonKnowledge : Maybe knowledge
    , directDependenciesKnowledge : Maybe knowledge
    , moduleKnowledgeByPath : FastDict.Dict String knowledge
    , extraFileKnowledgeByPath : FastDict.Dict String knowledge
    }


{-| In case you need the read the source in a range as formatted by the user.
This can be nice to keep the user's formatting if you just move code around
-}
sourceExtractInRange : Elm.Syntax.Range.Range -> (String -> String)
sourceExtractInRange range string =
    string
        |> String.lines
        |> List.drop (range.start.row - 1)
        |> List.take (range.end.row - range.start.row + 1)
        |> ListLocalExtra.lastMap (Unicode.left (range.end.column - 1))
        |> String.join "\n"
        |> Unicode.dropLeft (range.start.column - 1)


{-| Find all occurrences of a given section in the source in the case you
can't access or easily calculate the [range](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Range)
-}
sourceRangesOf : String -> (String -> List Elm.Syntax.Range.Range)
sourceRangesOf sectionToFind source =
    let
        sectionToFindLength : Int
        sectionToFindLength =
            sectionToFind |> Unicode.length
    in
    source
        |> String.indexes sectionToFind
        |> List.map
            (\startOffset ->
                { start = startOffset |> sourceOffsetToLocationIn source
                , end = (startOffset + sectionToFindLength) |> sourceOffsetToLocationIn source
                }
            )


sourceOffsetToLocationIn : String -> (Int -> Elm.Syntax.Range.Location)
sourceOffsetToLocationIn source sourceOffset =
    let
        lines : List String
        lines =
            source |> Unicode.left sourceOffset |> String.lines
    in
    { row = lines |> List.length
    , column =
        case lines |> ListLocalExtra.last of
            Nothing ->
                1

            Just lastLine ->
                (lastLine |> Unicode.length) + 1
    }


{-| All surface-level child [expression](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression)s.

If you also want to record the newly available variables by child expression,
use [`expressionSubsWithBindings`](#expressionSubsWithBindings)

-}
expressionSubs :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
expressionSubs (Elm.Syntax.Node.Node _ expression) =
    case expression of
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


withoutBindings :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    ->
        { expressionNode : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , bindings : List String
        }
withoutBindings expressionNode =
    { expressionNode = expressionNode, bindings = [] }


{-| Like [`expressionSubs`](#expressionSubs)
with additional info about newly available variables from patterns and let declared value/function names
which can be used to for example disambiguate variables you plan to introduce yourself through a fix
-}
expressionSubsWithBindings :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    ->
        List
            { expressionNode : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
            , bindings : List String
            }
expressionSubsWithBindings (Elm.Syntax.Node.Node _ expression) =
    case expression of
        Elm.Syntax.Expression.Application expressions ->
            expressions |> List.map withoutBindings

        Elm.Syntax.Expression.ListExpr elements ->
            elements |> List.map withoutBindings

        Elm.Syntax.Expression.RecordExpr fields ->
            fields
                |> List.map (\(Elm.Syntax.Node.Node _ ( _, expr )) -> expr |> withoutBindings)

        Elm.Syntax.Expression.RecordUpdateExpression _ setters ->
            setters
                |> List.map (\(Elm.Syntax.Node.Node _ ( _, expr )) -> expr |> withoutBindings)

        Elm.Syntax.Expression.ParenthesizedExpression expr ->
            [ expr |> withoutBindings ]

        Elm.Syntax.Expression.OperatorApplication _ direction left right ->
            case direction of
                Elm.Syntax.Infix.Left ->
                    [ left |> withoutBindings, right |> withoutBindings ]

                Elm.Syntax.Infix.Right ->
                    [ right |> withoutBindings, left |> withoutBindings ]

                Elm.Syntax.Infix.Non ->
                    [ left |> withoutBindings, right |> withoutBindings ]

        Elm.Syntax.Expression.IfBlock cond then_ else_ ->
            [ cond |> withoutBindings
            , then_ |> withoutBindings
            , else_ |> withoutBindings
            ]

        Elm.Syntax.Expression.TupledExpression expressions ->
            expressions |> List.map withoutBindings

        Elm.Syntax.Expression.Negation expr ->
            [ expr |> withoutBindings ]

        Elm.Syntax.Expression.RecordAccess expr _ ->
            [ expr |> withoutBindings ]

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

        Elm.Syntax.Expression.LambdaExpression lambda ->
            [ { expressionNode = lambda.expression
              , bindings =
                    lambda.args |> List.concatMap patternBindings
              }
            ]

        Elm.Syntax.Expression.CaseExpression caseOf ->
            (caseOf.expression |> withoutBindings)
                :: (caseOf.cases
                        |> List.map
                            (\( patternNode, caseExpressionNode ) ->
                                { expressionNode = caseExpressionNode
                                , bindings = patternNode |> patternBindings
                                }
                            )
                   )

        Elm.Syntax.Expression.LetExpression letIn ->
            let
                variablesForWholeLetIn : List String
                variablesForWholeLetIn =
                    letIn.declarations
                        |> List.concatMap
                            (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                case letDeclaration of
                                    Elm.Syntax.Expression.LetFunction letFunction ->
                                        [ letFunction.declaration
                                            |> Elm.Syntax.Node.value
                                            |> .name
                                            |> Elm.Syntax.Node.value
                                        ]

                                    Elm.Syntax.Expression.LetDestructuring patternNode _ ->
                                        patternNode |> patternBindings
                            )
            in
            { expressionNode = letIn.expression
            , bindings = variablesForWholeLetIn
            }
                :: (letIn.declarations
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                letDeclaration |> letDeclarationExpressionSubsWithBindings
                            )
                   )


letDeclarationExpressionSubsWithBindings :
    Elm.Syntax.Expression.LetDeclaration
    ->
        { expressionNode : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
        , bindings : List String
        }
letDeclarationExpressionSubsWithBindings letDeclaration =
    case letDeclaration of
        Elm.Syntax.Expression.LetDestructuring _ destructuredExpressionNode ->
            destructuredExpressionNode |> withoutBindings

        Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
            { expressionNode = letValueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value |> .expression
            , bindings =
                letValueOrFunctionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .arguments
                    |> List.concatMap patternBindings
            }


{-| Recursively find all introduced variables
in the [pattern](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern)
(like `a` and `b` in `( Just a, { b } )`)
-}
patternBindings : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern -> List String
patternBindings (Elm.Syntax.Node.Node _ pattern) =
    -- IGNORE TCO
    case pattern of
        Elm.Syntax.Pattern.VarPattern name ->
            [ name ]

        Elm.Syntax.Pattern.AsPattern afterAsPattern (Elm.Syntax.Node.Node _ name) ->
            name :: (afterAsPattern |> patternBindings)

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            inParens |> patternBindings

        Elm.Syntax.Pattern.ListPattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.TuplePattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.RecordPattern fields ->
            fields |> List.map Elm.Syntax.Node.value

        Elm.Syntax.Pattern.NamedPattern _ patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            (tailPattern |> patternBindings) ++ (headPattern |> patternBindings)

        Elm.Syntax.Pattern.AllPattern ->
            []

        Elm.Syntax.Pattern.UnitPattern ->
            []

        Elm.Syntax.Pattern.CharPattern _ ->
            []

        Elm.Syntax.Pattern.StringPattern _ ->
            []

        Elm.Syntax.Pattern.IntPattern _ ->
            []

        Elm.Syntax.Pattern.HexPattern _ ->
            []

        Elm.Syntax.Pattern.FloatPattern _ ->
            []


{-| All surface-level child [pattern](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern)s
-}
patternSubs :
    Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
patternSubs (Elm.Syntax.Node.Node _ pattern) =
    case pattern of
        Elm.Syntax.Pattern.VarPattern _ ->
            []

        Elm.Syntax.Pattern.AsPattern afterAsPattern _ ->
            [ afterAsPattern ]

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            [ inParens ]

        Elm.Syntax.Pattern.ListPattern patterns ->
            patterns

        Elm.Syntax.Pattern.TuplePattern partPatterns ->
            partPatterns

        Elm.Syntax.Pattern.RecordPattern _ ->
            []

        Elm.Syntax.Pattern.NamedPattern _ patterns ->
            patterns

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            [ headPattern, tailPattern ]

        Elm.Syntax.Pattern.AllPattern ->
            []

        Elm.Syntax.Pattern.UnitPattern ->
            []

        Elm.Syntax.Pattern.CharPattern _ ->
            []

        Elm.Syntax.Pattern.StringPattern _ ->
            []

        Elm.Syntax.Pattern.IntPattern _ ->
            []

        Elm.Syntax.Pattern.HexPattern _ ->
            []

        Elm.Syntax.Pattern.FloatPattern _ ->
            []


{-| All surface-level child [type](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-TypeAnnotation)s
-}
typeSubs :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
typeSubs (Elm.Syntax.Node.Node _ type_) =
    case type_ of
        Elm.Syntax.TypeAnnotation.GenericType _ ->
            []

        Elm.Syntax.TypeAnnotation.Typed _ variantValues ->
            variantValues

        Elm.Syntax.TypeAnnotation.Unit ->
            []

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            parts

        Elm.Syntax.TypeAnnotation.Record fields ->
            fields |> List.map (\(Elm.Syntax.Node.Node _ ( _, value )) -> value)

        Elm.Syntax.TypeAnnotation.GenericRecord _ (Elm.Syntax.Node.Node _ fields) ->
            fields |> List.map (\(Elm.Syntax.Node.Node _ ( _, value )) -> value)

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation from to ->
            [ from, to ]


{-| The set of modules in [`Elm-Project.Exposed`](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Project#Exposed)
(the `exposed-modules` field in a package elm.json)
-}
packageElmJsonExposedModules : Elm.Project.Exposed -> FastSet.Set Elm.Syntax.ModuleName.ModuleName
packageElmJsonExposedModules exposed =
    case exposed of
        Elm.Project.ExposedList list ->
            list |> FastSetLocalExtra.fromListMap elmJsonModuleNameToSyntax

        Elm.Project.ExposedDict dict ->
            dict
                |> List.concatMap (\( _, moduleNames ) -> moduleNames)
                |> FastSetLocalExtra.fromListMap elmJsonModuleNameToSyntax


elmJsonModuleNameToSyntax : Elm.Module.Name -> Elm.Syntax.ModuleName.ModuleName
elmJsonModuleNameToSyntax elmJsonModuleName =
    elmJsonModuleName |> Elm.Module.toString |> String.split "."


{-| The module's documentation comment at the top of the file, which can be `Nothing`.
`elm-syntax` has a weird way of giving you the comments of a file, this helper should make it easier
-}
moduleHeaderDocumentation : Elm.Syntax.File.File -> Maybe (Elm.Syntax.Node.Node String)
moduleHeaderDocumentation syntaxFile =
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


{-| Collect all exposed members from the file

  - `simpleNames`: values, functions, type alias, opaque types (those without `(..)`), infix operators (parenthesized)

  - `typesWithVariantNames`: names of types that expose their variants with `(..)`
    paired with what variant names are actually meant by `..`.

The `Dict` and `Set` are from [`miniBill/elm-fast-dict`](https://dark.elm.dmy.fr/packages/miniBill/elm-fast-dict/latest/).
I strongly recommend using it too for your writing your review instead of `Dict`
because `Dict.union` and friends have performance footguns you shouldn't need to worry about

-}
moduleExposes :
    Elm.Syntax.File.File
    ->
        { simpleNames : FastSet.Set String
        , typesWithVariantNames : FastDict.Dict String (FastSet.Set String)
        }
moduleExposes syntaxFile =
    let
        moduleTypesWithVariantNames : FastDict.Dict String (FastSet.Set String)
        moduleTypesWithVariantNames =
            syntaxFile.declarations
                |> List.foldl
                    (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                        case declaration of
                            Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                                soFar
                                    |> FastDict.insert (choiceTypeDeclaration.name |> Elm.Syntax.Node.value)
                                        (choiceTypeDeclaration.constructors
                                            |> FastSetLocalExtra.fromListMap
                                                (\(Elm.Syntax.Node.Node _ constructor) ->
                                                    constructor.name |> Elm.Syntax.Node.value
                                                )
                                        )

                            _ ->
                                soFar
                    )
                    FastDict.empty
    in
    case syntaxFile.moduleDefinition |> Elm.Syntax.Node.value |> Elm.Syntax.Module.exposingList of
        Elm.Syntax.Exposing.Explicit topLevelExposeList ->
            let
                visibleExposes : { simpleNames : FastSet.Set String, typesExposingVariants : FastSet.Set String }
                visibleExposes =
                    topLevelExposeList |> topLevelExposeListToExposes
            in
            { simpleNames = visibleExposes.simpleNames
            , typesWithVariantNames =
                visibleExposes.typesExposingVariants
                    |> FastSet.foldl
                        (\choiceTypeName soFar ->
                            case moduleTypesWithVariantNames |> FastDict.get choiceTypeName of
                                Nothing ->
                                    soFar

                                Just variantNames ->
                                    soFar |> FastDict.insert choiceTypeName variantNames
                        )
                        FastDict.empty
            }

        Elm.Syntax.Exposing.All _ ->
            { simpleNames =
                syntaxFile.declarations
                    |> List.foldl
                        (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                            case declaration of
                                Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
                                    soFar

                                Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                                    soFar
                                        |> FastSet.insert
                                            (valueOrFunctionDeclaration.declaration
                                                |> Elm.Syntax.Node.value
                                                |> .name
                                                |> Elm.Syntax.Node.value
                                            )

                                Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                                    soFar |> FastSet.insert (typeAliasDeclaration.name |> Elm.Syntax.Node.value)

                                Elm.Syntax.Declaration.PortDeclaration signature ->
                                    soFar |> FastSet.insert (signature.name |> Elm.Syntax.Node.value)

                                Elm.Syntax.Declaration.InfixDeclaration symbol ->
                                    soFar |> FastSet.insert (symbol.function |> Elm.Syntax.Node.value)

                                -- invalid elm
                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                    soFar
                        )
                        FastSet.empty
            , typesWithVariantNames = moduleTypesWithVariantNames
            }


{-| Same as [`moduleExposes`](#moduleExposes) but for [docs module](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Docs#Module)s
from [`Review.inspectDirectDependencies`](#inspectDirectDependencies)
-}
moduleInterfaceExposes :
    Elm.Docs.Module
    -> { simpleNames : FastSet.Set String, typesWithVariantNames : FastDict.Dict String (FastSet.Set String) }
moduleInterfaceExposes moduleInterface =
    { simpleNames =
        FastSet.union (moduleInterface.aliases |> FastSetLocalExtra.fromListMap .name)
            (FastSet.union
                (moduleInterface.binops
                    |> FastSetLocalExtra.fromListMap
                        (\infixOperator -> "(" ++ infixOperator.name ++ ")")
                )
                (moduleInterface.unions
                    |> List.foldl
                        (\choiceTypeInterface soFar ->
                            case choiceTypeInterface.tags of
                                [] ->
                                    soFar |> FastSet.insert choiceTypeInterface.name

                                _ :: _ ->
                                    soFar
                        )
                        FastSet.empty
                )
            )
    , typesWithVariantNames =
        moduleInterface.unions
            |> List.foldl
                (\choiceTypeInterface soFar ->
                    case choiceTypeInterface.tags of
                        [] ->
                            soFar

                        variantInterface0 :: variantInterface1Up ->
                            soFar
                                |> FastDict.insert choiceTypeInterface.name
                                    ((variantInterface0 :: variantInterface1Up)
                                        |> FastSetLocalExtra.fromListMap (\( variantName, _ ) -> variantName)
                                    )
                )
                FastDict.empty
    }


{-| Collect all exposed members from an explicit exposing list of [visible expose](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Exposing#TopLevelExpose)s

  - `simpleNames`: values, functions, type alias, opaque types (those without `(..)`), infix operators (parenthesized)
  - `typesWithVariantNames`: names of types that expose their variants with `(..)`

-}
topLevelExposeListToExposes :
    List (Elm.Syntax.Node.Node Elm.Syntax.Exposing.TopLevelExpose)
    -> { simpleNames : FastSet.Set String, typesExposingVariants : FastSet.Set String }
topLevelExposeListToExposes topLevelExposeSet =
    topLevelExposeSet
        |> List.foldl
            (\(Elm.Syntax.Node.Node _ expose) soFar ->
                case expose of
                    Elm.Syntax.Exposing.TypeExpose choiceTypeExpose ->
                        case choiceTypeExpose.open of
                            Nothing ->
                                { simpleNames = soFar.simpleNames |> FastSet.insert choiceTypeExpose.name
                                , typesExposingVariants = soFar.typesExposingVariants
                                }

                            Just _ ->
                                { typesExposingVariants =
                                    soFar.typesExposingVariants
                                        |> FastSet.insert choiceTypeExpose.name
                                , simpleNames = soFar.simpleNames
                                }

                    Elm.Syntax.Exposing.FunctionExpose valueOrFunctionName ->
                        { simpleNames =
                            soFar.simpleNames |> FastSet.insert valueOrFunctionName
                        , typesExposingVariants = soFar.typesExposingVariants
                        }

                    Elm.Syntax.Exposing.TypeOrAliasExpose exposeName ->
                        { simpleNames =
                            soFar.simpleNames |> FastSet.insert exposeName
                        , typesExposingVariants = soFar.typesExposingVariants
                        }

                    Elm.Syntax.Exposing.InfixExpose symbol ->
                        { simpleNames =
                            soFar.simpleNames |> FastSet.insert ("(" ++ symbol ++ ")")
                        , typesExposingVariants = soFar.typesExposingVariants
                        }
            )
            { simpleNames = FastSet.empty
            , typesExposingVariants = FastSet.empty
            }


{-| Either the full qualification of a given variant/value/function/type identifier
or `Nothing` if defined locally.

Requires a list of local explicit imports, see [`importsToExplicit`](#importsToExplicit)

-}
determineModuleOrigin :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        { alias : Maybe String
        , exposes : FastSet.Set String -- includes names of variants
        }
    -> (( Elm.Syntax.ModuleName.ModuleName, String ) -> Maybe Elm.Syntax.ModuleName.ModuleName)
determineModuleOrigin imports ( qualification, unqualifiedName ) =
    case imports |> FastDict.get qualification of
        Just _ ->
            qualification |> Just

        Nothing ->
            let
                maybeOriginByAlias : Maybe Elm.Syntax.ModuleName.ModuleName
                maybeOriginByAlias =
                    imports
                        |> FastDictLocalExtra.firstJustMap
                            (\importModuleName import_ ->
                                case import_.alias of
                                    Nothing ->
                                        Nothing

                                    Just alias ->
                                        if qualification == [ alias ] then
                                            importModuleName |> Just

                                        else
                                            Nothing
                            )
            in
            case maybeOriginByAlias of
                Just aliasOriginModuleName ->
                    aliasOriginModuleName |> Just

                Nothing ->
                    case qualification of
                        [] ->
                            imports
                                |> FastDictLocalExtra.firstJustMap
                                    (\importModuleName import_ ->
                                        if import_.exposes |> FastSet.member unqualifiedName then
                                            importModuleName |> Just

                                        else
                                            Nothing
                                    )

                        _ :: _ ->
                            Nothing


{-| Regular elm imports don't tell the whole truth of what names can be used in the module declarations.
Elm for example [uses some imports implicitly](https://dark.elm.dmy.fr/packages/elm/core/latest#Default-Imports),
allows `exposing (..)` and `ChoiceTypeWithVariants(..)`
and merges imports in a specific manner.

[`importsToExplicit`](#importsToExplicit) now takes the module exposes
of each module (see [`moduleExposes`](#moduleExposes))
and a list of imports `exposing (..)` and imports exposing explicit (see [`topLevelExposeListToExposes`](#topLevelExposeListToExposes))
from the current module to give you a
[`FastDict`](https://dark.elm.dmy.fr/packages/miniBill/elm-fast-dict/latest/) with all imports and actual exposes and aliases listed.

Can be used to [`determineModuleOrigin`](#determineModuleOrigin).

Edge case note: The [elm compiler has a bug where the List module does not expose the type `List`](https://github.com/elm/core/issues/1037).
To make it easier to differentiate between local and imported `List` type
we still say that the `List` type originates from `List`.
It's then the responsibility of review authors to cope with this edge case
when trying to edit the qualification or adding an import.

-}
importsToExplicit :
    { moduleExposes :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            { typesWithVariantNames : FastDict.Dict String (FastSet.Set String)
            , simpleNames : FastSet.Set String
            }
    , importsExposingAll : List { moduleName : Elm.Syntax.ModuleName.ModuleName, alias : Maybe String }
    , importsExposingExplicit :
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , typesExposingVariants : FastSet.Set String
            , simpleNames : FastSet.Set String
            }
    }
    ->
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            { alias : Maybe String
            , exposes : FastSet.Set String -- includes names of variants
            }
importsToExplicit info =
    let
        explicitImportsExposingAllAndDefaultOnes :
            FastDict.Dict
                Elm.Syntax.ModuleName.ModuleName
                { alias : Maybe String
                , exposes : FastSet.Set String -- includes names of variants
                }
        explicitImportsExposingAllAndDefaultOnes =
            info.importsExposingAll
                |> List.foldl
                    (\import_ importsSoFar ->
                        importsSoFar
                            |> mergeInImport
                                { moduleName = import_.moduleName
                                , alias = import_.alias
                                , exposes =
                                    case info |> .moduleExposes |> FastDict.get import_.moduleName of
                                        Just actualExposes ->
                                            FastSet.union actualExposes.simpleNames
                                                (actualExposes.typesWithVariantNames
                                                    |> FastDict.foldl
                                                        (\typeName variantNames soFar ->
                                                            FastSet.union variantNames
                                                                (soFar |> FastSet.insert typeName)
                                                        )
                                                        FastSet.empty
                                                )

                                        Nothing ->
                                            FastSet.empty
                                }
                    )
                    implicitImports
    in
    info.importsExposingExplicit
        |> List.foldl
            (\import_ importsSoFar ->
                importsSoFar
                    |> mergeInImport
                        { moduleName = import_.moduleName
                        , alias = import_.alias
                        , exposes =
                            case info |> .moduleExposes |> FastDict.get import_.moduleName of
                                Just moduleExposeInfo ->
                                    FastSet.union
                                        import_.simpleNames
                                        (import_.typesExposingVariants
                                            |> FastSet.foldl
                                                (\typeExpose soFar ->
                                                    FastSet.union
                                                        (moduleExposeInfo
                                                            |> .typesWithVariantNames
                                                            |> FastDict.get typeExpose
                                                            |> Maybe.withDefault FastSet.empty
                                                        )
                                                        soFar
                                                        |> FastSet.insert typeExpose
                                                )
                                                FastSet.empty
                                        )

                                Nothing ->
                                    FastSet.empty
                        }
            )
            explicitImportsExposingAllAndDefaultOnes


{-| From the `elm/core` readme:

>
> ### Default Imports

> The modules in this package are so common, that some of them are imported by default in all Elm files. So it is as if every Elm file starts with these imports:
>
>     import Basics exposing (..)
>     import List exposing (List, (::))
>     import Maybe exposing (Maybe(..))
>     import Result exposing (Result(..))
>     import String exposing (String)
>     import Char exposing (Char)
>     import Tuple
>     import Debug
>     import Platform exposing (Program)
>     import Platform.Cmd as Cmd exposing (Cmd)
>     import Platform.Sub as Sub exposing (Sub)

-}
implicitImports :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        { alias : Maybe String
        , exposes : FastSet.Set String -- includes names of variants
        }
implicitImports =
    [ ( [ "Basics" ]
      , { alias = Nothing
        , exposes =
            [ "Int"
            , "Float"
            , "(+)"
            , "(-)"
            , "(*)"
            , "(/)"
            , "(//)"
            , "(^)"
            , "toFloat"
            , "round"
            , "floor"
            , "ceiling"
            , "truncate"
            , "(==)"
            , "(/=)"
            , "(<)"
            , "(>)"
            , "(<=)"
            , "(>=)"
            , "max"
            , "min"
            , "compare"
            , "Order"
            , "LT"
            , "EQ"
            , "GT"
            , "Bool"
            , "True"
            , "False"
            , "not"
            , "(&&)"
            , "(||)"
            , "xor"
            , "(++)"
            , "modBy"
            , "remainderBy"
            , "negate"
            , "abs"
            , "clamp"
            , "sqrt"
            , "logBase"
            , "e"
            , "pi"
            , "cos"
            , "sin"
            , "tan"
            , "acos"
            , "asin"
            , "atan"
            , "atan2"
            , "degrees"
            , "radians"
            , "turns"
            , "toPolar"
            , "fromPolar"
            , "isNaN"
            , "isInfinite"
            , "identity"
            , "always"
            , "(<|)"
            , "(|>)"
            , "(<<)"
            , "(>>)"
            , "Never"
            , "never"
            ]
                |> FastSet.fromList
        }
      )
    , ( [ "List" ], { alias = Nothing, exposes = FastSet.fromList [ "(::)", "List" ] } )
    , ( [ "Maybe" ], { alias = Nothing, exposes = FastSet.fromList [ "Maybe", "Just", "Nothing" ] } )
    , ( [ "Result" ], { alias = Nothing, exposes = FastSet.fromList [ "Result", "Ok", "Err" ] } )
    , ( [ "String" ], { alias = Nothing, exposes = FastSet.singleton "String" } )
    , ( [ "Char" ], { alias = Nothing, exposes = FastSet.singleton "Char" } )
    , ( [ "Tuple" ], { alias = Nothing, exposes = FastSet.empty } )
    , ( [ "Debug" ], { alias = Nothing, exposes = FastSet.empty } )
    , ( [ "Platform" ], { alias = Nothing, exposes = FastSet.singleton "Program" } )
    , ( [ "Platform", "Cmd" ], { alias = Just "Cmd", exposes = FastSet.singleton "Cmd" } )
    , ( [ "Platform", "Sub" ], { alias = Just "Sub", exposes = FastSet.singleton "Sub" } )
    ]
        |> FastDict.fromList


mergeInImport :
    { moduleName : Elm.Syntax.ModuleName.ModuleName
    , alias : Maybe String
    , exposes : FastSet.Set String
    }
    ->
        (FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            { alias : Maybe String
            , exposes : FastSet.Set String -- includes names of variants
            }
         ->
            FastDict.Dict
                Elm.Syntax.ModuleName.ModuleName
                { alias : Maybe String
                , exposes : FastSet.Set String -- includes names of variants
                }
        )
mergeInImport importInfoToAdd imports =
    FastDict.update importInfoToAdd.moduleName
        (\existingImport ->
            let
                newImportInfo : { alias : Maybe String, exposes : FastSet.Set String }
                newImportInfo =
                    case existingImport of
                        Nothing ->
                            { alias = importInfoToAdd.alias
                            , exposes = importInfoToAdd.exposes
                            }

                        Just import_ ->
                            { alias =
                                case import_.alias of
                                    Just alias ->
                                        alias |> Just

                                    Nothing ->
                                        importInfoToAdd.alias
                            , exposes = FastSet.union import_.exposes importInfoToAdd.exposes
                            }
            in
            Just newImportInfo
        )
        imports


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


{-| Try to apply a set of [`SourceEdit`](#SourceEdit)es to the relevant file source,
potentially failing with a [`SourceEditError`](#SourceEditError)
-}
sourceApplyEdits : List SourceEdit -> String -> Result SourceEditError String
sourceApplyEdits fixes sourceCode =
    if fixes |> sourceEditsContainRangeCollisions then
        FixHasCollisionsInRanges |> Err

    else
        let
            resultAfterFix : String
            resultAfterFix =
                fixes
                    |> List.sortWith
                        (\a b ->
                            -- flipped order
                            Elm.Syntax.Range.compareLocations (b |> fixRange |> .start) (a |> fixRange |> .start)
                        )
                    |> List.foldl applyFixSingle (sourceCode |> String.lines)
                    |> String.join "\n"
        in
        if sourceCode == resultAfterFix then
            AfterFixIsUnchanged |> Err

        else
            resultAfterFix |> Ok


applyFixSingle : SourceEdit -> (List String -> List String)
applyFixSingle fixToApply lines =
    case fixToApply of
        SourceRangeReplacement replace ->
            let
                linesBefore : List String
                linesBefore =
                    lines |> List.take (replace.range.start.row - 1)

                linesAfter : List String
                linesAfter =
                    lines |> List.drop replace.range.end.row

                startLine : String
                startLine =
                    lines
                        |> ListLocalExtra.elementAtIndex (replace.range.start.row - 1)
                        |> Maybe.withDefault ""
                        |> Unicode.left (replace.range.start.column - 1)

                endLine : String
                endLine =
                    lines
                        |> ListLocalExtra.elementAtIndex (replace.range.end.row - 1)
                        |> Maybe.withDefault ""
                        |> Unicode.dropLeft (replace.range.end.column - 1)
            in
            linesBefore
                ++ (replace.replacement
                        |> String.lines
                        |> ListLocalExtra.headMap (\replacementFirstLine -> startLine ++ replacementFirstLine)
                        |> ListLocalExtra.lastMap (\replacementLastLine -> replacementLastLine ++ endLine)
                   )
                ++ linesAfter


sourceEditsContainRangeCollisions : List SourceEdit -> Bool
sourceEditsContainRangeCollisions fixes =
    fixes
        |> ListLocalExtra.anyPair
            (\edit0 edit1 ->
                rangesCollide (edit0 |> fixRange) (edit1 |> fixRange)
            )


rangesCollide : Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range -> Bool
rangesCollide a b =
    case Elm.Syntax.Range.compareLocations a.end b.start of
        LT ->
            False

        EQ ->
            False

        GT ->
            case Elm.Syntax.Range.compareLocations b.end a.start of
                LT ->
                    False

                EQ ->
                    False

                GT ->
                    True



-- test


{-| The default project after `elm init` minus the `elm/html` and `elm/browser` dependencies.
Equivalent to (copy and adapt if necessary)

    { elmJson = """
        {
            "type": "application",
            "source-directories": [
                "src"
            ],
            "elm-version": "0.19.1",
            "dependencies": {
                "direct": {
                    "elm/core": "1.0.5"
                },
                "indirect": {
                    "elm/json": "1.1.3"
                }
            },
            "test-dependencies": {
                "direct": {},
                "indirect": {}
            }
        }
        """
    , directDependencies = []
    }

(elm/core is automatically part of every tested project)

-}
applicationConfigurationMinimal : { elmJson : String, directDependencies : List { elmJson : String, docsJson : String } }
applicationConfigurationMinimal =
    { elmJson = """
        {
            "type": "application",
            "source-directories": [
                "src"
            ],
            "elm-version": "0.19.1",
            "dependencies": {
                "direct": {
                    "elm/core": "1.0.5"
                },
                "indirect": {
                    "elm/json": "1.1.3"
                }
            },
            "test-dependencies": {
                "direct": {},
                "indirect": {}
            }
        }
        """
    , directDependencies = []
    }


{-| The result of running a review on a project
-}
type alias ReviewResult =
    Result
        (List String)
        (List
            { path : String
            , source : String
            , errors : List FileReviewError
            }
        )


type alias SuccessfulRunResult =
    { path : String
    , source : String
    , errors : List FileReviewError
    }


{-| An expectation that an error with this shape will be reported.
-}
type alias ExpectedFileError =
    { message : String
    , details : List String
    , range : ExpectedErrorRange
    , fixedFiles : List { path : String, source : String }
    }


{-| An expectation for the reported error range.
If the section to mark is unique in the source, use `Under "your section source"`.
If the section occurs multiple times in the source, use `UnderExactly { section = "your section source", startingAt = { row = ..., column = ... } }`
-}
type ExpectedErrorRange
    = ExpectUnder String
    | ExpectUnderExactly { section : String, startingAt : Elm.Syntax.Range.Location }


expectationsViolated : List String -> Expect.Expectation
expectationsViolated =
    \errors ->
        case errors of
            [] ->
                Expect.pass

            error0 :: error1Up ->
                Expect.fail ((error0 :: error1Up) |> String.join "\n\n\n\n")


{-| Run a `Review` on a project, matching all reported module errors, `elm.json` errors and extra file errors
with your provided expected errors.

    import DebugForbid
    import Review
    import Test

    tests : Test.Test
    tests =
        Test.describe "DebugForbid"
            [ Test.test "report Debug.log use"
                (\() ->
                    Review.test
                        { files =
                            [ { path = "src/A.elm"
                              , source = """
                                    module A exposing (a)
                                    import Html
                                    a =
                                        Debug.log "some" "message"
                                    """
                              }
                            ]
                        , projectConfiguration = Review.applicationConfigurationMinimal
                        , review = DebugForbid.review
                        , expectedErrors =
                            [ { path = "src/A.elm"
                              , message = "Remove the use of `Debug` before shipping to production"
                              , details = [ "Compiling elm code in optimized mode does not allow these helpers." ]
                              , range = Review.ExpectUnder "Debug.log"
                              , fixedFiles =
                                    [ { path = "src/A.elm"
                                      , source = """
                                        module A exposing (a)
                                        a =
                                            "message"
                                        """
                                      }
                                    ]
                              }
                            ]
                        }
                )
            ]

The extra indentation specified by the first line will be stripped of all the provided source strings.

You can also specify a custom project config in case you need to test a package
or want to add dependencies (elm/core is automatically part of every tested project)
by supplying the raw sources for the `elm.json` and the direct dependency `docs.json`
and `elm.json` you can find in your elm root folder.

-}
test :
    { files : List { path : String, source : String }
    , projectConfiguration :
        { elmJson : String, directDependencies : List { elmJson : String, docsJson : String } }
    , review : Review
    , expectedErrors :
        List
            { path : String
            , message : String
            , details : List String
            , range : ExpectedErrorRange
            , fixedFiles : List { path : String, source : String }
            }
    }
    -> Expect.Expectation
test config =
    let
        elmJsonSourceNormal : String
        elmJsonSourceNormal =
            config.projectConfiguration.elmJson |> multiLineStringNormalize
    in
    case elmJsonSourceNormal |> Json.Decode.decodeString Elm.Project.decoder of
        Err elmJsonParseError ->
            [ elmJsonParseError |> elmJsonParsingFailure ] |> expectationsViolated

        Ok elmJsonProject ->
            let
                filesWithNormalSource : List { path : String, source : String }
                filesWithNormalSource =
                    config.files
                        |> List.map
                            (\file -> { path = file.path, source = file.source |> multiLineStringNormalize })

                expectedErrorsWithNormalizedFixedSource : List { path : String, errors : List ExpectedFileError }
                expectedErrorsWithNormalizedFixedSource =
                    config.expectedErrors
                        |> List.foldl
                            (\error soFar ->
                                soFar
                                    |> FastDict.update error.path
                                        (\errorsAtPathSoFar ->
                                            (errorsAtPathSoFar |> Maybe.withDefault [])
                                                |> (::)
                                                    { message = error.message
                                                    , details = error.details
                                                    , range = error.range
                                                    , fixedFiles =
                                                        error.fixedFiles
                                                            |> List.map
                                                                (\fixedFile ->
                                                                    { path = fixedFile.path
                                                                    , source = fixedFile.source |> multiLineStringNormalize
                                                                    }
                                                                )
                                                    }
                                                |> Just
                                        )
                            )
                            FastDict.empty
                        |> FastDict.toList
                        |> List.map (\( path, errors ) -> { path = path, errors = errors })
            in
            case expectedErrorsWithNormalizedFixedSource |> invalidExpectedErrorsIn { files = filesWithNormalSource, project = elmJsonProject } of
                [] ->
                    let
                        normalSourceByPath : FastDict.Dict String String
                        normalSourceByPath =
                            filesWithNormalSource
                                |> FastDictLocalExtra.fromListMap
                                    (\file -> { key = file.path, value = file.source })
                    in
                    toReviewResult config.review
                        { files = filesWithNormalSource
                        , directDependencies = config.projectConfiguration.directDependencies
                        , elmJson = { source = elmJsonSourceNormal, project = elmJsonProject }
                        }
                        |> expectErrors normalSourceByPath expectedErrorsWithNormalizedFixedSource

                error0 :: error1Up ->
                    (error0 :: error1Up) |> expectationsViolated


multiLineStringNormalize : String -> String
multiLineStringNormalize =
    \string ->
        case string |> String.lines of
            [] ->
                ""

            [ onlyLine ] ->
                onlyLine

            firstLine :: secondLine :: thirdLineUp ->
                case firstLine |> String.trim of
                    "" ->
                        let
                            indentationToRemove : Int
                            indentationToRemove =
                                secondLine |> lineIndentation
                        in
                        (secondLine :: thirdLineUp)
                            |> List.map (String.dropLeft indentationToRemove)
                            |> String.join "\n"

                    _ ->
                        string


lineIndentation : String -> Int
lineIndentation =
    \line ->
        line
            |> String.foldl
                (\char soFar ->
                    if soFar.done then
                        soFar

                    else
                        case char of
                            ' ' ->
                                { indentation = soFar.indentation + 1, done = False }

                            _ ->
                                { indentation = soFar.indentation, done = True }
                )
                { indentation = 0, done = False }
            |> .indentation


toReviewResult :
    Review
    ->
        { files : List { path : String, source : String }
        , elmJson : { source : String, project : Elm.Project.Project }
        , directDependencies : List { elmJson : String, docsJson : String }
        }
    -> ReviewResult
toReviewResult review project =
    let
        sourceDirectories : List String
        sourceDirectories =
            project.elmJson.project |> ElmJson.LocalExtra.sourceDirectories

        pathIsModule : String -> Bool
        pathIsModule =
            \path ->
                (path |> String.endsWith ".elm")
                    && (sourceDirectories |> List.any (\dir -> path |> String.startsWith dir))

        ( moduleFiles, extraFiles ) =
            project.files
                |> List.partition
                    (\file -> file.path |> pathIsModule)

        moduleParseResults :
            Result
                (List { path : String })
                (List { path : String, source : String, syntax : Elm.Syntax.File.File })
        moduleParseResults =
            moduleFiles
                |> List.foldl
                    (\moduleUnparsed soFar ->
                        case moduleUnparsed.source |> Elm.Parser.parseToFile of
                            Err _ ->
                                (case soFar of
                                    Ok _ ->
                                        []

                                    Err failedToParseSoFar ->
                                        failedToParseSoFar
                                )
                                    |> (::) { path = moduleUnparsed.path }
                                    |> Err

                            Ok syntax ->
                                case soFar of
                                    Ok parsedSoFar ->
                                        parsedSoFar
                                            |> (::)
                                                { path = moduleUnparsed.path
                                                , source = moduleUnparsed.source
                                                , syntax = syntax
                                                }
                                            |> Ok

                                    Err failedToParseSoFar ->
                                        failedToParseSoFar |> Err
                    )
                    (Ok [])
    in
    case moduleParseResults of
        Err modulesThatFailedToParse ->
            modulesThatFailedToParse
                |> List.map moduleFailedToParse
                |> Err

        Ok modulesParsed ->
            let
                directDependenciesParsed : List { parsed : Maybe { elmJson : Elm.Project.Project, docsJson : List Elm.Docs.Module }, errors : List String }
                directDependenciesParsed =
                    project.directDependencies
                        |> List.map
                            (\directDependency ->
                                let
                                    elmJsonParseResult : Result Json.Decode.Error Elm.Project.Project
                                    elmJsonParseResult =
                                        directDependency.elmJson |> Json.Decode.decodeString Elm.Project.decoder

                                    docsJsonParseResult : Result Json.Decode.Error (List Elm.Docs.Module)
                                    docsJsonParseResult =
                                        directDependency.docsJson |> Json.Decode.decodeString (Json.Decode.list Elm.Docs.decoder)
                                in
                                case ( elmJsonParseResult, docsJsonParseResult ) of
                                    ( Ok elmJsonParsed, Ok docsJsonParsed ) ->
                                        { parsed = Just { elmJson = elmJsonParsed, docsJson = docsJsonParsed }
                                        , errors = []
                                        }

                                    ( Ok _, Err docsJsonParseError ) ->
                                        { parsed = Nothing
                                        , errors = [ dependencyDocsJsonParsingFailure docsJsonParseError ]
                                        }

                                    ( Err elmJsonParseError, Ok _ ) ->
                                        { parsed = Nothing
                                        , errors = [ dependencyElmJsonParsingFailure elmJsonParseError ]
                                        }

                                    ( Err elmJsonParseError, Err docsJsonParseError ) ->
                                        { parsed = Nothing
                                        , errors =
                                            [ dependencyDocsJsonParsingFailure docsJsonParseError
                                            , dependencyElmJsonParsingFailure elmJsonParseError
                                            ]
                                        }
                            )
            in
            case directDependenciesParsed |> List.concatMap .errors of
                directDependenciesFailedToParse0 :: directDependenciesFailedToParse1Up ->
                    (directDependenciesFailedToParse0 :: directDependenciesFailedToParse1Up)
                        |> Err

                [] ->
                    let
                        runErrorsByPath : FastDict.Dict String (List FileReviewError)
                        runErrorsByPath =
                            { elmJson = project.elmJson
                            , directDependencies =
                                directDependenciesParsed
                                    |> List.filterMap .parsed
                            , addedOrChangedExtraFiles = extraFiles
                            , addedOrChangedModules = modulesParsed
                            , removedExtraFilePaths = []
                            , removedModulePaths = []
                            }
                                |> run review
                                |> .errorsByPath

                        fileErrors : List SuccessfulRunResult
                        fileErrors =
                            (modulesParsed
                                |> List.map
                                    (\module_ ->
                                        { path = module_.path
                                        , source = module_.source
                                        , errors =
                                            runErrorsByPath
                                                |> FastDict.get module_.path
                                                |> Maybe.withDefault []
                                        }
                                    )
                            )
                                ++ (case runErrorsByPath |> FastDict.get "elm.json" of
                                        Just errorsForElmJson ->
                                            [ { path = "elm.json"
                                              , source = project.elmJson.source
                                              , errors = errorsForElmJson
                                              }
                                            ]

                                        Nothing ->
                                            []
                                   )
                                ++ (extraFiles
                                        |> List.filterMap
                                            (\extraFile ->
                                                runErrorsByPath
                                                    |> FastDict.get extraFile.path
                                                    |> Maybe.map
                                                        (\errorsForExtraFile ->
                                                            { path = extraFile.path
                                                            , source = extraFile.source
                                                            , errors = errorsForExtraFile
                                                            }
                                                        )
                                            )
                                   )
                    in
                    fileErrors |> Ok


failureMessage : String -> String -> String
failureMessage title content =
    [ title |> Ansi.bold |> Ansi.red, "\n\n", content ] |> String.concat


moduleFailedToParse : { path : String } -> String
moduleFailedToParse file =
    failureMessage "module failed to parse"
        ([ """I could not parse the provided elm module source code at path """
         , file.path
         , ".\n\n"
         , """Hint: Maybe you forgot to add the module definition at the top, like module A exposing (a)"""
         ]
            |> String.concat
        )


dependencyElmJsonParsingFailure : Json.Decode.Error -> String
dependencyElmJsonParsingFailure jsonDecodeError =
    failureMessage "dependency elm.json failed to parse"
        ([ """I could not parse a provided dependency elm.json source: """
         , jsonDecodeError |> Json.Decode.errorToString
         , "\n\n"
         , """Hint: Maybe you copy pasted the wrong file?"""
         ]
            |> String.concat
        )


dependencyDocsJsonParsingFailure : Json.Decode.Error -> String
dependencyDocsJsonParsingFailure jsonDecodeError =
    failureMessage "dependency docs.json failed to parse"
        ([ """I could not parse a provided dependency docs.json source: """
         , jsonDecodeError |> Json.Decode.errorToString
         , "\n\n"
         , """Hint: Maybe you copy pasted the wrong file?"""
         ]
            |> String.concat
        )


invalidExpectedErrorsIn :
    { files : List { path : String, source : String }
    , project : Elm.Project.Project
    }
    -> (List { path : String, errors : List ExpectedFileError } -> List String)
invalidExpectedErrorsIn project expectedErrors =
    let
        sourceDirectories : List String
        sourceDirectories =
            project.project |> ElmJson.LocalExtra.sourceDirectories

        projectPathsExceptElmJson : FastSet.Set String
        projectPathsExceptElmJson =
            project.files |> FastSetLocalExtra.fromListMap .path

        pathIsModule : String -> Bool
        pathIsModule path =
            (path |> String.endsWith ".elm")
                && (sourceDirectories |> List.any (\dir -> path |> String.startsWith dir))

        modulePaths : FastSet.Set String
        modulePaths =
            projectPathsExceptElmJson |> FastSet.filter pathIsModule

        projectPaths : FastSet.Set String
        projectPaths =
            projectPathsExceptElmJson |> FastSet.insert "elm.json"
    in
    (expectedErrors
        |> List.filterMap
            (\expectedErrorsAtPath ->
                if projectPaths |> FastSet.member expectedErrorsAtPath.path then
                    Nothing

                else
                    unknownFilesInExpectedErrors expectedErrorsAtPath.path
                        |> Just
            )
    )
        ++ (expectedErrors
                |> List.concatMap
                    (\expectedFileErrors ->
                        case expectedFileErrors.path of
                            "elm.json" ->
                                expectedFileErrors.errors

                            _ ->
                                []
                    )
                |> List.concatMap
                    (\expectedError ->
                        expectedError.fixedFiles
                            |> List.filterMap
                                (\fixedFile ->
                                    case fixedFile.source |> Json.Decode.decodeString Elm.Project.decoder of
                                        Ok _ ->
                                            Nothing

                                        Err jsonDecodeError ->
                                            elmJsonFixedSourceParsingFailure
                                                jsonDecodeError
                                                { message = expectedError.message, details = expectedError.details }
                                                |> Just
                                )
                    )
           )
        ++ (expectedErrors
                |> List.concatMap
                    (\expectedFileErrors ->
                        if modulePaths |> FastSet.member expectedFileErrors.path then
                            expectedFileErrors.errors
                                |> List.concatMap
                                    (\expectedError ->
                                        expectedError.fixedFiles
                                            |> List.filterMap
                                                (\fixedFile ->
                                                    case fixedFile.source |> Elm.Parser.parseToFile of
                                                        Ok _ ->
                                                            Nothing

                                                        Err _ ->
                                                            moduleFixedSourceParsingFailure
                                                                { path = expectedFileErrors.path
                                                                , errorInfo =
                                                                    { message = expectedError.message
                                                                    , details = expectedError.details
                                                                    }
                                                                }
                                                                |> Just
                                                )
                                    )

                        else
                            []
                    )
           )


elmJsonFixedSourceParsingFailure : Json.Decode.Error -> { message : String, details : List String } -> String
elmJsonFixedSourceParsingFailure decodeError errorInfo =
    failureMessage "expected fixed elm.json failed to parse"
        ([ """I could not decode the expected fixed elm.json for the error with the message

  """
         , errorInfo.message
         , """"

because """
         , decodeError |> Json.Decode.errorToString
         ]
            |> String.concat
        )


moduleFixedSourceParsingFailure : { path : String, errorInfo : { message : String, details : List String } } -> String
moduleFixedSourceParsingFailure info =
    failureMessage "expected fixed module failed to parse"
        ([ "I could not parse the provided expected fixed elm module source code at path "
         , info.path
         , """ for the error with the message

  """
         , info.errorInfo.message
         , """

Hint: Maybe you forgot to add the module definition at the top, like module A exposing (a)?"""
         ]
            |> String.concat
        )


unknownFilesInExpectedErrors : String -> String
unknownFilesInExpectedErrors path =
    failureMessage "expected errors use unknown path"
        ([ """I expected errors for the file at path """
         , path
         , """ but I couldn't find an entry with its source in the test files field with that path."""
         ]
            |> String.concat
        )


expectErrors : FastDict.Dict String String -> List { path : String, errors : List ExpectedFileError } -> (ReviewResult -> Expect.Expectation)
expectErrors sourceByPath expectationsList =
    \reviewResult ->
        case reviewResult of
            Err errors ->
                expectationsViolated errors

            Ok runResults ->
                let
                    expectations : FastDict.Dict String (List ExpectedFileError)
                    expectations =
                        expectationsList
                            |> List.foldl
                                (\fileExpectation soFar ->
                                    case fileExpectation.errors of
                                        [] ->
                                            soFar

                                        error0 :: errors1Up ->
                                            soFar |> FastDict.insert fileExpectation.path (error0 :: errors1Up)
                                )
                                FastDict.empty
                in
                Expect.all
                    (runResults
                        |> List.map
                            (\runResult () ->
                                checkAllErrorsMatch
                                    { sourceByPath = sourceByPath
                                    , runResult = runResult
                                    , unorderedExpectedErrors =
                                        expectations
                                            |> FastDict.get runResult.path
                                            |> Maybe.withDefault []
                                    }
                            )
                    )
                    ()


checkAllErrorsMatch :
    { sourceByPath : FastDict.Dict String String
    , runResult : SuccessfulRunResult
    , unorderedExpectedErrors : List ExpectedFileError
    }
    -> Expect.Expectation
checkAllErrorsMatch toCheck =
    let
        errorsOrdered : { expected : List ExpectedFileError, reviewErrors : List FileReviewError }
        errorsOrdered =
            reorderErrors
                { source = toCheck.runResult.source
                , expectedErrors = toCheck.unorderedExpectedErrors
                , reviewErrors = toCheck.runResult.errors
                , pairs = []
                , expectedErrorsWithNoMatch = []
                }
    in
    expectErrorsMatch
        { sourceByPath = toCheck.sourceByPath
        , runResult = toCheck.runResult
        , expected = errorsOrdered.expected
        , reviewErrors = errorsOrdered.reviewErrors
        }


reorderErrors :
    { source : String
    , expectedErrors : List ExpectedFileError
    , reviewErrors : List FileReviewError
    , pairs : List { expected : ExpectedFileError, reviewError : FileReviewError }
    , expectedErrorsWithNoMatch : List ExpectedFileError
    }
    -> { expected : List ExpectedFileError, reviewErrors : List FileReviewError }
reorderErrors reorderState =
    case reorderState.expectedErrors of
        [] ->
            { expected =
                (reorderState.expectedErrorsWithNoMatch ++ (reorderState.pairs |> List.map .expected))
                    |> List.reverse
            , reviewErrors =
                (reorderState.reviewErrors ++ (reorderState.pairs |> List.map .reviewError))
                    |> List.reverse
            }

        expectedError :: restOfExpectedErrors ->
            case findBestMatchingReviewError { source = reorderState.source, expected = expectedError, reviewErrorsRemaining = reorderState.reviewErrors, bestMatchError = Nothing, bestMatchConfidenceLevel = 0 } of
                Just reviewError ->
                    reorderErrors
                        { reorderState
                            | pairs = { expected = expectedError, reviewError = reviewError } :: reorderState.pairs
                            , reviewErrors = listRemoveFirstMember reviewError reorderState.reviewErrors
                            , expectedErrors = restOfExpectedErrors
                        }

                Nothing ->
                    reorderErrors
                        { reorderState
                            | expectedErrorsWithNoMatch = expectedError :: reorderState.expectedErrorsWithNoMatch
                            , expectedErrors = restOfExpectedErrors
                        }


listRemoveFirstMember : a -> (List a -> List a)
listRemoveFirstMember elementToRemove list =
    -- IGNORE TCO
    case list of
        [] ->
            []

        head :: tail ->
            if head == elementToRemove then
                tail

            else
                head :: listRemoveFirstMember elementToRemove tail


findBestMatchingReviewError : { source : String, expected : ExpectedFileError, reviewErrorsRemaining : List FileReviewError, bestMatchError : Maybe FileReviewError, bestMatchConfidenceLevel : Int } -> Maybe FileReviewError
findBestMatchingReviewError matchInfo =
    case matchInfo.reviewErrorsRemaining of
        [] ->
            matchInfo.bestMatchError

        reviewError :: restOfReviewErrors ->
            let
                confidenceLevel : Int
                confidenceLevel =
                    matchingConfidenceLevel matchInfo.source matchInfo.expected reviewError
            in
            if confidenceLevel > matchInfo.bestMatchConfidenceLevel then
                findBestMatchingReviewError
                    { source = matchInfo.source
                    , expected = matchInfo.expected
                    , reviewErrorsRemaining = restOfReviewErrors
                    , bestMatchError = Just reviewError
                    , bestMatchConfidenceLevel = confidenceLevel
                    }

            else
                findBestMatchingReviewError
                    { source = matchInfo.source
                    , expected = matchInfo.expected
                    , reviewErrorsRemaining = restOfReviewErrors
                    , bestMatchError = matchInfo.bestMatchError
                    , bestMatchConfidenceLevel = matchInfo.bestMatchConfidenceLevel
                    }


matchingConfidenceLevel : String -> ExpectedFileError -> FileReviewError -> Int
matchingConfidenceLevel source expectedErrorDetails reviewError =
    if expectedErrorDetails.message /= reviewError.message then
        0

    else
        case expectedErrorDetails.range of
            ExpectUnder under ->
                if (source |> sourceExtractInRange reviewError.range) /= under then
                    1

                else
                    2

            ExpectUnderExactly underExactly ->
                if (source |> sourceExtractInRange reviewError.range) /= underExactly.section then
                    1

                else if underExactly.startingAt /= reviewError.range.start then
                    2

                else
                    3


expectErrorsMatch :
    { sourceByPath : FastDict.Dict String String
    , runResult : SuccessfulRunResult
    , expected : List ExpectedFileError
    , reviewErrors : List FileReviewError
    }
    -> Expect.Expectation
expectErrorsMatch toCheckForMatchingErrors =
    -- IGNORE TCO
    case ( toCheckForMatchingErrors.expected, toCheckForMatchingErrors.reviewErrors ) of
        ( [], [] ) ->
            Expect.pass

        ( [], error :: restOfErrors ) ->
            tooManyErrors
                toCheckForMatchingErrors.runResult.path
                (error :: restOfErrors)
                |> Expect.fail

        ( expected :: restOfExpectedErrors, [] ) ->
            (expected :: restOfExpectedErrors)
                |> List.map expectedErrorForMessage
                |> tooFewErrors toCheckForMatchingErrors.runResult.path
                |> Expect.fail

        ( expected :: restOfExpectedErrors, reviewError :: restOfErrors ) ->
            Expect.all
                [ \() ->
                    expectErrorsMatch
                        { sourceByPath = toCheckForMatchingErrors.sourceByPath
                        , runResult = toCheckForMatchingErrors.runResult
                        , expected = restOfExpectedErrors
                        , reviewErrors = restOfErrors
                        }
                , \() ->
                    reviewError.message
                        |> Expect.equal expected.message
                        |> Expect.onFail
                            (messageMismatch
                                (expectedErrorForMessage expected)
                                reviewError
                            )
                , \() ->
                    checkMessageAppearsUnder
                        { source = toCheckForMatchingErrors.runResult.source
                        , reviewError = reviewError
                        , expected = expected
                        }
                , \() -> checkDetailsAreCorrect reviewError expected
                , \() ->
                    checkFixesAreCorrect
                        { sourceByPath = toCheckForMatchingErrors.sourceByPath
                        , reviewError = reviewError
                        , expected = expected
                        }
                ]
                ()


expectedErrorForMessage :
    { error_
        | message : String
        , details : List String
        , range : ExpectedErrorRange
    }
    -> { message : String, details : List String, under : String }
expectedErrorForMessage =
    \expectedError ->
        { message = expectedError.message
        , details = expectedError.details
        , under =
            case expectedError.range of
                ExpectUnder section ->
                    section

                ExpectUnderExactly underExactly ->
                    underExactly.section
        }


checkMessageAppearsUnder :
    { source : String, reviewError : FileReviewError, expected : ExpectedFileError }
    -> Expect.Expectation
checkMessageAppearsUnder toCheck =
    let
        errorSourceInRange : String
        errorSourceInRange =
            toCheck.source |> sourceExtractInRange toCheck.reviewError.range
    in
    case toCheck.expected.range of
        ExpectUnder under ->
            Expect.all
                [ \() ->
                    errorSourceInRange
                        |> Expect.equal under
                        |> Expect.onFail
                            (underMismatch
                                toCheck.reviewError
                                { under = under, errorSourceInRange = errorSourceInRange }
                            )
                , \() ->
                    case toCheck.source |> String.indexes under of
                        [ _ ] ->
                            Expect.pass

                        occurrencesInSourceCode ->
                            Expect.fail
                                (locationIsAmbiguousInSourceCode toCheck.source
                                    toCheck.reviewError
                                    under
                                    occurrencesInSourceCode
                                )
                ]
                ()

        ExpectUnderExactly underExactly ->
            Expect.all
                [ \() ->
                    errorSourceInRange
                        |> Expect.equal underExactly.section
                        |> Expect.onFail
                            (underMismatch toCheck.reviewError
                                { under = underExactly.section, errorSourceInRange = errorSourceInRange }
                            )
                , \() ->
                    toCheck.reviewError.range.start
                        |> Expect.equal underExactly.startingAt
                        |> Expect.onFail
                            (wrongLocation toCheck.reviewError
                                { start = underExactly.startingAt
                                , end =
                                    { row = underExactly.startingAt.row
                                    , column = underExactly.startingAt.column + (underExactly.section |> String.length)
                                    }
                                }
                                underExactly.section
                            )
                ]
                ()


formatSourceCodeWithFormatter : (List String -> List String) -> List String -> String
formatSourceCodeWithFormatter formatter lines =
    if List.length lines == 1 then
        formatter lines
            |> String.join "\n"

    else
        formatter lines
            |> List.map
                (\str ->
                    case str of
                        "" ->
                            ""

                        nonBlankLine ->
                            "    " ++ nonBlankLine
                )
            |> String.join "\n"


formatSourceCode : String -> String
formatSourceCode string =
    formatSourceCodeWithFormatter identity (string |> String.lines)


underMismatch : { error_ | range : Elm.Syntax.Range.Range, message : String, details : List String } -> { under : String, errorSourceInRange : String } -> String
underMismatch error range =
    failureMessage "error location doesn't match"
        ([ """I found an error with the message

  """
         , error.message
         , """

and I was expecting it to be under

  """
         , range.under |> formatSourceCode
         , """

but I found it under

  """
         , range.errorSourceInRange |> formatSourceCode
         , """

Hint: Maybe you're passing the range of a wrong node to the review error?"""
         ]
            |> String.concat
        )


locationToCodeString : Elm.Syntax.Range.Location -> String
locationToCodeString =
    \location ->
        [ "{ row = "
        , String.fromInt location.row
        , ", column = "
        , String.fromInt location.column
        , " }"
        ]
            |> String.concat


wrongLocation : { error_ | range : Elm.Syntax.Range.Range, message : String, details : List String } -> Elm.Syntax.Range.Range -> String -> String
wrongLocation error range under =
    failureMessage "exact error location doesn't match"
        ([ """I was looking for the error with the message

  """
         , error.message
         , """

under the code

  """
         , under |> formatSourceCode
         , """

and I found it, but the exact location you specified is not the one I found.

I was expecting the error at

  """
         , range.start |> locationToCodeString
         , """

but I found it at

  """
         , error.range.start |> locationToCodeString
         ]
            |> String.concat
        )


locationIsAmbiguousInSourceCode : String -> { error_ | range : Elm.Syntax.Range.Range, message : String, details : List String } -> String -> List Int -> String
locationIsAmbiguousInSourceCode sourceCode error under occurrencesInSourceCode =
    failureMessage "expected error location is ambiguous"
        ([ """The exact location where the error appears is ambiguous.

You are looking for the error message

  """
         , error.message
         , """

and expecting to see it under

  """
         , under |> formatSourceCode
         , """

I found """
         , occurrencesInSourceCode |> List.length |> String.fromInt
         , """ locations where that code appeared. Switch from Review.ExpectUnder to
Review.ExpectUnderExactly to make the range you were targeting unambiguous.

Tip: I found them starting at:
"""
         , occurrencesInSourceCode
            |> List.map
                (\occurrence ->
                    "  - "
                        ++ (occurrence
                                |> startingLocationInSource sourceCode
                                |> locationToCodeString
                           )
                )
            |> String.join "\n"
         ]
            |> String.concat
        )


startingLocationInSource : String -> Int -> Elm.Syntax.Range.Location
startingLocationInSource sourceCode position =
    let
        linesBeforeAndIncludingPosition : List String
        linesBeforeAndIncludingPosition =
            sourceCode
                |> String.slice 0 position
                |> String.lines
    in
    { row = linesBeforeAndIncludingPosition |> List.length
    , column =
        (linesBeforeAndIncludingPosition
            |> ListLocalExtra.last
            |> Maybe.withDefault ""
            |> String.length
        )
            + 1
    }


checkDetailsAreCorrect : FileReviewError -> ExpectedFileError -> Expect.Expectation
checkDetailsAreCorrect error expectedError =
    Expect.all
        [ \() ->
            List.isEmpty error.details
                |> Expect.equal False
                |> Expect.onFail (emptyDetails (.message error))
        , \() ->
            error.details
                |> Expect.equal expectedError.details
                |> Expect.onFail (unexpectedDetails expectedError.details error)
        ]
        ()


unexpectedDetails : List String -> { error_ | range : Elm.Syntax.Range.Range, message : String, details : List String } -> String
unexpectedDetails expectedDetails error =
    failureMessage "error details don't match"
        ([ """I found an error for a file with the message

  """
         , error.message
         , """

and I was expecting its details to be

  """
         , expectedDetails |> List.map (\paragraph -> "  " ++ paragraph) |> String.join "\n\n"
         , """

but I found the details

  """
         , error.details |> List.map (\paragraph -> "  " ++ paragraph) |> String.join "\n\n"
         ]
            |> String.concat
        )


emptyDetails : String -> String
emptyDetails errorMessage =
    failureMessage "error details are empty"
        ([ """I found an error with the message

  """
         , errorMessage
         , """

but its details were empty. Having details will
help the user who encounters the problem by for example
  - explaining what the problem is
  - explaining the reasoning behind the problem
  - giving suggestions on how to solve the problem or alternatives"""
         ]
            |> String.concat
        )


checkFixesAreCorrect :
    { sourceByPath : FastDict.Dict String String
    , reviewError : FileReviewError
    , expected : ExpectedFileError
    }
    -> Expect.Expectation
checkFixesAreCorrect toCheck =
    let
        expectedFixedSourcedByPath : FastDict.Dict String String
        expectedFixedSourcedByPath =
            toCheck.expected.fixedFiles
                |> FastDictLocalExtra.fromListMap
                    (\file -> { key = file.path, value = file.source })
    in
    FastDict.merge
        (\path _ -> (::) (missingSourceEditsForFile path toCheck.expected))
        (\path expectedFixedSource reviewEdits ->
            ListLocalExtra.consJust
                (checkFileFixesAreCorrect
                    toCheck.expected
                    (toCheck.sourceByPath |> FastDict.get path |> Maybe.withDefault "")
                    expectedFixedSource
                    reviewEdits
                )
        )
        (\path _ -> (::) (unexpectedEditsToFile path toCheck.expected))
        expectedFixedSourcedByPath
        toCheck.reviewError.fixEditsByPath
        []
        |> expectationsViolated


checkFileFixesAreCorrect :
    { expectedError_
        | range : ExpectedErrorRange
        , message : String
        , details : List String
    }
    -> String
    -> String
    -> List SourceEdit
    -> Maybe String
checkFileFixesAreCorrect expectedError source expectedFixedSource reviewErrorEdits =
    case sourceApplyEdits reviewErrorEdits source of
        Ok fixedSource ->
            if fixedSource == expectedFixedSource then
                Nothing

            else if removeWhitespace fixedSource == removeWhitespace expectedFixedSource then
                Just (fixedSourceWhitespaceMismatch fixedSource expectedFixedSource expectedError)

            else
                Just (fixedSourceMismatch fixedSource expectedFixedSource expectedError)

        Err AfterFixIsUnchanged ->
            Just (unchangedSourceAfterFix expectedError)

        Err FixHasCollisionsInRanges ->
            Just (hasCollisionsInFixRanges expectedError)


removeWhitespace : String -> String
removeWhitespace =
    \string -> string |> String.replace " " "" |> String.replace "\n" ""


fixedSourceMismatch : String -> String -> { error_ | message : String, details : List String } -> String
fixedSourceMismatch resultingSource expectedSource error =
    failureMessage "fixed source doesn't match"
        ([ """I found a different fixed source code than expected for the error with the message

"""
         , error.message
         , """

after the fixes have been applied, I expected the source to be

"""
         , formatSourceCode expectedSource
         , """

but I found the source

"""
         , formatSourceCode resultingSource
         ]
            |> String.concat
        )


fixedSourceWhitespaceMismatch : String -> String -> { error_ | message : String, details : List String } -> String
fixedSourceWhitespaceMismatch resultingSource expectedSource error =
    let
        ( expected, resulting ) =
            highlightDifferencesInSourceCodes ( resultingSource, expectedSource )
    in
    failureMessage "fixed source whitespace doesn't match"
        ([ """I found a different fixed source than expected for the error with the message

  """
         , error.message
         , """

The problem is related to """
         , "whitespace" |> Ansi.bold |> Ansi.yellow
         , """.
after the fixes have been applied, I expected the source to be

"""
         , expected
         , """

but I found the source

"""
         , resulting
         ]
            |> String.concat
        )


highlightDifferencesInSourceCodes : ( String, String ) -> ( String, String )
highlightDifferencesInSourceCodes ( a, b ) =
    let
        ( resA, resB ) =
            highlightWhiteSpaceDifferences ( a, b )
    in
    ( formatSourceCodeWithFormatter replaceWhitespace (resA |> String.lines)
    , formatSourceCodeWithFormatter replaceWhitespace (resB |> String.lines)
    )


highlightWhiteSpaceDifferences : ( String, String ) -> ( String, String )
highlightWhiteSpaceDifferences ( aString, bString ) =
    Diff.diff (aString |> String.toList) (bString |> String.toList)
        |> List.foldl
            (\change ( a, b ) ->
                case change of
                    Diff.NoChange str ->
                        ( a ++ String.fromChar str, b ++ String.fromChar str )

                    Diff.Added '\n' ->
                        ( [ a, Ansi.backgroundRed "↵", "\n" ] |> String.concat, b )

                    Diff.Added str ->
                        ( a ++ Ansi.backgroundRed (String.fromChar str), b )

                    Diff.Removed '\n' ->
                        ( a, [ b, Ansi.backgroundRed "↵", "\n" ] |> String.concat )

                    Diff.Removed str ->
                        ( a, b ++ Ansi.backgroundRed (String.fromChar str) )
            )
            ( "", "" )


replaceWhitespace : List String -> List String
replaceWhitespace lines =
    lines
        |> List.map (String.replace " " (Ansi.cyan "·"))
        |> String.join (Ansi.cyan "\n")
        |> String.split "\n"


unchangedSourceAfterFix : { error_ | message : String, details : List String } -> String
unchangedSourceAfterFix error =
    failureMessage "applying edits did not change the source"
        ([ """After applying the fixes provided by the error with the message

  """
         , error.message
         , """

I expected the fix to make some changes to the source, but it resulted
in the same source as before the fixes.

This is problematic because I will tell the user that this rule provides an
automatic fix, but I will have to disappoint them when I later find out it
doesn't do anything."""
         ]
            |> String.concat
        )


hasCollisionsInFixRanges : { error_ | message : String, details : List String } -> String
hasCollisionsInFixRanges error =
    failureMessage "source edit ranges collide"
        ([ """When applying the fixes provided by the error with the message

  """
         , error.message
         , """

I found that some fixes were targeting (partially or completely) the same
section of code. The problem with that is that I can't determine which fix
to apply first, and the result will be different and potentially invalid
based on the order in which I apply these fixes.

For this reason, I require that the ranges (for replacing and removing) and
the positions (for inserting) of every fix to be mutually exclusive.

Hint: Maybe you duplicated a fix, or you targeted the wrong node for one
of your fixes."""
         ]
            |> String.concat
        )


missingSourceEditsForFile : String -> { error_ | message : String, details : List String } -> String
missingSourceEditsForFile path expectedError =
    failureMessage "file fix edits are missing"
        ([ """The error with the message

  """
         , expectedError.message
         , """

did not provide edits as a fix for the file at the path """
         , path
         , """ which I had expected."""
         ]
            |> String.concat
        )


unexpectedEditsToFile : String -> { error_ | message : String, details : List String } -> String
unexpectedEditsToFile path error =
    failureMessage "source edits to file not expected"
        ([ """The error with the message

  """
         , error.message
         , """

provided a fix with source edits for the file at path """
         , path
         , """ for which I expected no edits.

To expect edits, add an entry { path = \""""
         , path
         , """", source = ..your expected changed source.. } to your error's fixedFiles field."""
         ]
            |> String.concat
        )


messageMismatch : { expectedError_ | message : String, details : List String } -> { error_ | message : String, details : List String } -> String
messageMismatch expectedError reviewError =
    failureMessage "error message doesn't match"
        ([ """I was looking for the error with the message

  """
         , expectedError.message
         , """

but I found the error message

  """
         , reviewError.message
         ]
            |> String.concat
        )


tooFewErrors : String -> List { message : String, details : List String, under : String } -> String
tooFewErrors path missingExpectedErrors =
    failureMessage "missing errors"
        ([ "I expected to see more errors for the file at path "
         , path
         , """.
Here are those I could not find:

"""
         , missingExpectedErrors
            |> List.map (\expectedError -> "  - " ++ expectedError.message)
            |> String.join "\n"
         ]
            |> String.concat
        )


tooManyErrors : String -> List { error_ | range : Elm.Syntax.Range.Range, message : String, details : List String } -> String
tooManyErrors path extraErrors =
    failureMessage "unexpected additional errors"
        ([ "I found too many errors for the file at path "
         , path
         , ":\n\n"
         , extraErrors
            |> List.map
                (\error ->
                    [ "  - ", error.message, "\n    at ", rangeToCodeString error.range ] |> String.concat
                )
            |> String.join "\n"
         ]
            |> String.concat
        )


rangeToCodeString : Elm.Syntax.Range.Range -> String
rangeToCodeString =
    \range ->
        [ "{ start = "
        , range.start |> locationToCodeString
        , ", end = "
        , range.end |> locationToCodeString
        , " }"
        ]
            |> String.concat


elmJsonParsingFailure : Json.Decode.Error -> String
elmJsonParsingFailure decodeError =
    failureMessage "elm.json failed to parse"
        ([ """I could not decode the provided elm.json because """
         , decodeError |> Json.Decode.errorToString
         , "\n\n"
         , """Hint: Try to start with a working elm.json from one of your projects and add dependencies from there"""
         ]
            |> String.concat
        )


type alias FileReviewError =
    { range : Elm.Syntax.Range.Range
    , message : String
    , details : List String
    , fixEditsByPath : FastDict.Dict String (List SourceEdit)
    }
