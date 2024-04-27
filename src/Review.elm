module Review exposing
    ( Review, ignoreErrorsForFilesWhere
    , create
    , inspectDirectDependency, inspectDirectIndependency, inspectElmJson, inspectModule, inspectReadme
    , ElmJsonKey(..), ModuleData, ModuleKey(..), ReadmeKey(..)
    , inspectComposable, inspectMap
    , ContextToErrors(..), ErrorTarget(..), Error, ErrorWithoutFixes
    , Fix(..), FixProblem(..), fixInsertAt, fixRemoveRange, fixReplaceRangeBy
    , declarationToFunction, expressionSubs, functionDeclarationExpression, moduleHeaderDocumentation, moduleHeaderNameNode, projectConfigSourceDirectories
    , run, FixMode(..)
    )

{-| `elm-review-simple` scans the modules, `elm.json`, dependencies and `README.md` from your project.
All this data is then fed into your reviews, which in turn inspect it to report problems.

@docs Review, ignoreErrorsForFilesWhere


# write


## what makes a good rule

  - check [whether a rule should be written](./#when-to-write-or-enable-a-rule),
  - name: (`OnlyUsedVariables`, `DebugForbid`, ...) should try to convey
    really quickly what kind of pattern we're dealing with. Ideally, a user who
    encounters this pattern for the first time could guess the problem just from the
    name. And a user who encountered it several times should know how to fix the
    problem just from the name too
  - documentation: explain when (not) to
    enable the rule in the user's review configuration. For instance, for a rule that
    makes sure that a package is publishable by ensuring that all docs are valid,
    the rule might say something along the lines of "If you are writing an
    application, then you should not use this rule". Additionally, it could give a few examples of patterns that will be reported and
    of patterns that will not be reported, so that users can have a better grasp of
    what to expect
  - error `message`: half-sentence on what _is_ undesired here. A user
    that has encountered this error multiple times should know exactly what to do.
    Example: "`foo` is never used" → a user who
    knows the rule knows that a function can be removed and which one
  - error `details`: additional information such as the rationale and suggestions
    for a solution or alternative.
  - error report range: where the squiggly lines appear under. Make this section as small as
    possible. For instance, in a rule that would forbid
    `Debug.log`, you would the error to appear under `Debug.log`, not on the whole
    function call

Take the elm compiler errors as inspiration in terms of helpfulness.


## use Test-Driven Development!

[`Review.Test`](./Review-Test) works with [`elm-test`](https://github.com/elm-explorations/test).
I recommend reading through [`the strategies for effective testing`](./Review-Test#strategies-for-effective-testing) before
starting writing a rule.


# writing

Each module is provided as a tree-like structure which represents your source code, using the
[`elm-syntax` package](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/).
All the unknown imports you'll see will be coming from there. You are likely to
need to have the documentation for that package open when writing a rule.

@docs Review, create


## inspecting

Look at the global picture of an Elm project to collect context.

@docs inspectDirectDependency, inspectDirectIndependency, inspectElmJson, inspectModule, inspectReadme
@docs ElmJsonKey, ModuleData, ModuleKey, ReadmeKey
@docs inspectComposable, inspectMap


## reporting

@docs ContextToErrors, ErrorTarget, Error, ErrorWithoutFixes
@docs Fix, FixProblem, fixInsertAt, fixRemoveRange, fixReplaceRangeBy


## helpers

@docs declarationToFunction, expressionSubs, functionDeclarationExpression, moduleHeaderDocumentation, moduleHeaderNameNode, projectConfigSourceDirectories


# running

@docs run, FixMode

-}

import Array
import Dict exposing (Dict)
import Elm.Docs
import Elm.Package
import Elm.Parser
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Location, Range)
import Json.Decode
import Json.Encode as Encode
import Path
import Review.Cache.ContentHash as ContentHash exposing (ContentHash)
import Review.Cache.ContextHash as ContextHash exposing (ComparableContextHash, ContextHash)
import Review.Cache.EndAnalysis as EndAnalysisCache
import Review.Cache.ProjectFile as ProjectFileCache
import Review.ElmProjectEncoder
import Review.FilePath exposing (FilePath)
import Rope exposing (Rope)
import Serialize
import Set exposing (Set)
import Unicode


computeDirectDependencies :
    { a
        | elmJson : Maybe { path : String, raw : String, project : Elm.Project.Project, contentHash : ContentHash }
        , dependencies : Dict String { name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
    }
    -> Dict String { name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
computeDirectDependencies project =
    case Maybe.map .project project.elmJson of
        Nothing ->
            project.dependencies

        Just (Elm.Project.Application { depsDirect, testDepsDirect }) ->
            let
                allDeps : List String
                allDeps =
                    List.map (\( name, _ ) -> Elm.Package.toString name) (depsDirect ++ testDepsDirect)
            in
            Dict.filter (\depName _ -> List.member depName allDeps) project.dependencies

        Just (Elm.Project.Package { deps, testDeps }) ->
            let
                allDeps : List String
                allDeps =
                    List.map (\( name, _ ) -> Elm.Package.toString name) (deps ++ testDeps)
            in
            Dict.filter (\depName _ -> List.member depName allDeps) project.dependencies


projectConfigSourceDirectories : Elm.Project.Project -> List String
projectConfigSourceDirectories elmJson =
    case elmJson of
        Elm.Project.Application application ->
            application.dirs |> List.map (\dir -> dir |> removeDotSlashAtBeginning |> Path.makeOSAgnostic |> endWithSlash)

        Elm.Project.Package _ ->
            [ "src/" ]


removeDotSlashAtBeginning : String -> String
removeDotSlashAtBeginning dir =
    if String.startsWith "./" dir then
        String.dropLeft 2 dir

    else
        dir


endWithSlash : String -> String
endWithSlash dir =
    if String.endsWith "/" dir then
        dir

    else
        dir ++ "/"


type alias ValidProject =
    { modules :
        List
            { path : String
            , source : String
            , ast : Elm.Syntax.File.File
            , contentHash : ContentHash
            }
    , elmJson : Maybe { path : String, raw : String, project : Elm.Project.Project, contentHash : ContentHash }
    , readme : Maybe { path : String, content : String, contentHash : ContentHash }
    , indirectDependencies : List { name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
    , directDependencies : List { name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
    }


projectToValid :
    { modules : List { path : String, source : String }
    , elmJson : Maybe { path : String, source : String }
    , readme : Maybe { path : String, content : String }
    , indirectDependencies : List { name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
    , directDependencies : List { name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
    }
    -> Result { filesAtPathFailedToParse : Set String } ValidProject
projectToValid p =
    let
        modulesParsed =
            p.modules
                |> List.foldl
                    (\moduleInfo soFar ->
                        case moduleInfo.source |> Elm.Parser.parseToFile of
                            Ok ast ->
                                case soFar of
                                    Err soFarErrors ->
                                        soFarErrors |> Err

                                    Ok soFarModules ->
                                        soFarModules
                                            |> (::)
                                                { path = moduleInfo.path
                                                , ast = ast
                                                , source = moduleInfo.source
                                                , contentHash = moduleInfo.source |> ContentHash.hash
                                                }
                                            |> Ok

                            Err _ ->
                                Err
                                    (case soFar of
                                        Ok _ ->
                                            moduleInfo.path |> Set.singleton

                                        Err soFarErrors ->
                                            soFarErrors |> Set.insert moduleInfo.path
                                    )
                    )
                    (Ok [])
    in
    case modulesParsed of
        Err moduleNamesWithParseErrors ->
            Err { filesAtPathFailedToParse = moduleNamesWithParseErrors }

        Ok modules ->
            let
                readme : Maybe { path : String, content : String, contentHash : ContentHash }
                readme =
                    p.readme |> Debug.todo ""
            in
            case p.elmJson of
                Nothing ->
                    { modules = modules
                    , elmJson = Nothing
                    , readme = readme
                    , indirectDependencies = p.indirectDependencies
                    , directDependencies = p.directDependencies
                    }
                        |> Ok

                Just elmJson ->
                    case elmJson |> .source |> Json.Decode.decodeString Elm.Project.decoder of
                        Err projectFailedToDecode ->
                            Err { filesAtPathFailedToParse = elmJson.path |> Set.singleton }

                        Ok project ->
                            { modules = modules
                            , elmJson =
                                Just
                                    { path = elmJson.path
                                    , raw = elmJson.source
                                    , project = project
                                    , contentHash = elmJson.source |> ContentHash.hash
                                    }
                            , readme = readme
                            , indirectDependencies = p.indirectDependencies
                            , directDependencies = p.directDependencies
                            }
                                |> Ok


syntaxFileSanitize : Elm.Syntax.File.File -> Elm.Syntax.File.File
syntaxFileSanitize =
    \syntaxFile ->
        { syntaxFile
            | comments =
                syntaxFile.comments
                    |> List.sortBy (\(Node range _) -> ( range.start.row, range.start.column ))
        }


toReview :
    { name : String
    , contextMerge : context -> context -> context
    , contextSerialize : Serialize.Codec String context
    , directDependencyToContext : Dict String ({ name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> context)
    , indirectDependencyToContext : Dict String ({ name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> context)
    , elmJsonToContext : Dict String (Maybe { key : ElmJsonKey, project : Elm.Project.Project } -> context)
    , readmeToContext : Dict String (Maybe { key : ReadmeKey, content : String } -> context)
    , projectModuleToContext : Dict String (ModuleData -> context)
    , toErrors : ContextToErrors context
    }
    -> Review
toReview =
    \review ->
        let
            serialize : context -> Serialized
            serialize =
                \context ->
                    context
                        |> Serialize.encodeToString review.contextSerialize
                        |> SerializedBase64

            deserialize : Serialized -> Result (Serialize.Error String) context
            deserialize =
                \(SerializedBase64 serializedBase64) ->
                    serializedBase64 |> Serialize.decodeFromString review.contextSerialize
        in
        { name = review.name
        , contextMerge =
            \a b ->
                Result.map2
                    (\aContext bContext ->
                        review.contextMerge aContext bContext |> serialize
                    )
                    (a |> deserialize)
                    (b |> deserialize)
        , directDependencyToContext =
            review.directDependencyToContext
                |> Dict.map (\_ toContext data -> data |> toContext |> serialize)
        , indirectDependencyToContext =
            review.indirectDependencyToContext
                |> Dict.map (\_ toContext data -> data |> toContext |> serialize)
        , elmJsonToContext =
            review.elmJsonToContext
                |> Dict.map (\_ toContext data -> data |> toContext |> serialize)
        , readmeToContext =
            review.readmeToContext
                |> Dict.map (\_ toContext data -> data |> toContext |> serialize)
        , projectModuleToContext =
            review.projectModuleToContext
                |> Dict.map (\_ toContext data -> data |> toContext |> serialize)
        , toErrors =
            case review.toErrors of
                ErrorsWithoutFixes toErrors ->
                    (\context ->
                        context |> deserialize |> Result.map toErrors
                    )
                        |> SerializedErrorsWithoutFixes

                ErrorsWithFixes toErrors ->
                    (\context ->
                        context |> deserialize |> Result.map toErrors
                    )
                        |> SerializedErrorsWithFixes
        , ignoreErrorsForFiles = \_ -> False
        }


{-| Represents a construct able to analyze a project and report
unwanted patterns.

TODO

-}
type alias Review =
    { name : String
    , contextMerge : Serialized -> Serialized -> Result (Serialize.Error String) Serialized
    , directDependencyToContext : Dict String ({ name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> Serialized)
    , indirectDependencyToContext : Dict String ({ name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> Serialized)
    , elmJsonToContext : Dict String (Maybe { key : ElmJsonKey, project : Elm.Project.Project } -> Serialized)
    , readmeToContext : Dict String (Maybe { key : ReadmeKey, content : String } -> Serialized)
    , projectModuleToContext : Dict String (ModuleData -> Serialized)
    , toErrors : SerializedContextToErrors
    , ignoreErrorsForFiles : String -> Bool
    }


type Serialized
    = SerializedBase64 String


type alias Inspect context =
    Rope
        { name : InspectName
        , targetToContext : InspectSingleTargetToContext context
        }


{-| Allow multiple reviews to feed off the same collected context,
for example published in a package.

Examples:

  - "data to determine all bindings in scope"
  - "data to determine a given reference's full origin"
  - "data to determine the reference's minimum qualification"
  - "type information"

The given name should describe what's in the context,
like "module exposes including variants from project, direct and indirect dependencies"

-}
inspectComposable : String -> List (Inspect context) -> Inspect context
inspectComposable name inspects =
    inspects
        |> Rope.fromList
        |> Rope.concat
        |> Rope.map
            (\inspectSingle ->
                { inspectSingle
                    | name =
                        InspectName
                            (case inspectSingle.name of
                                InspectNameInheritFromParent ->
                                    name

                                InspectName existingName ->
                                    existingName
                            )
                }
            )


type InspectName
    = InspectNameInheritFromParent
    | InspectName String


type InspectSingleTargetToContext context
    = InspectDirectDependencyToContext ({ name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> context)
    | InspectIndirectDependencyToContext ({ name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> context)
    | InspectElmJsonToContext (Maybe { key : ElmJsonKey, project : Elm.Project.Project } -> context)
    | InspectReadmeToContext (Maybe { key : ReadmeKey, content : String } -> context)
    | InspectProjectModuleToContext (ModuleData -> context)


type ContextToErrors context
    = ErrorsWithFixes (context -> Rope Error)
    | ErrorsWithoutFixes (context -> Rope ErrorWithoutFixes)


type SerializedContextToErrors
    = SerializedErrorsWithFixes (Serialized -> Result (Serialize.Error String) (Rope Error))
    | SerializedErrorsWithoutFixes (Serialized -> Result (Serialize.Error String) (Rope ErrorWithoutFixes))


type alias ErrorWithoutFixes =
    { range : Range
    , message : String
    , details : List String
    , target : ErrorTarget
    }


type alias Error =
    { range : Range
    , message : String
    , details : List String
    , target : ErrorTarget
    , fixes : List Fix
    }


type ErrorTarget
    = ErrorTargetProjectModule ModuleKey
    | ErrorTargetProjectElmJson ElmJsonKey
    | ErrorTargetReadme ReadmeKey


inspectMap : (context -> contextMapped) -> (Inspect context -> Inspect contextMapped)
inspectMap contextChange =
    \inspect ->
        inspect
            |> Rope.map
                (\inspectSingle ->
                    { name = inspectSingle.name
                    , targetToContext = inspectSingle.targetToContext |> inspectSingleTargetToContextMap contextChange
                    }
                )


inspectSingleTargetToContextMap : (context -> contextMapped) -> (InspectSingleTargetToContext context -> InspectSingleTargetToContext contextMapped)
inspectSingleTargetToContextMap contextChange =
    \inspectSingle ->
        case inspectSingle of
            InspectDirectDependencyToContext toContext ->
                (\data -> data |> toContext |> contextChange) |> InspectDirectDependencyToContext

            InspectIndirectDependencyToContext toContext ->
                (\data -> data |> toContext |> contextChange) |> InspectIndirectDependencyToContext

            InspectElmJsonToContext toContext ->
                (\data -> data |> toContext |> contextChange) |> InspectElmJsonToContext

            InspectReadmeToContext toContext ->
                (\data -> data |> toContext |> contextChange) |> InspectReadmeToContext

            InspectProjectModuleToContext toContext ->
                (\data -> data |> toContext |> contextChange) |> InspectProjectModuleToContext


inspectModule : (ModuleData -> context) -> Inspect context
inspectModule moduleDataToContext =
    { name = InspectNameInheritFromParent
    , targetToContext = InspectProjectModuleToContext moduleDataToContext
    }
        |> Rope.singleton


inspectElmJson : (Maybe { key : ElmJsonKey, project : Elm.Project.Project } -> context) -> Inspect context
inspectElmJson moduleDataToContext =
    { name = InspectNameInheritFromParent
    , targetToContext = InspectElmJsonToContext moduleDataToContext
    }
        |> Rope.singleton


inspectReadme : (Maybe { key : ReadmeKey, content : String } -> context) -> Inspect context
inspectReadme moduleDataToContext =
    { name = InspectNameInheritFromParent
    , targetToContext = InspectReadmeToContext moduleDataToContext
    }
        |> Rope.singleton


inspectDirectDependency : ({ name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> context) -> Inspect context
inspectDirectDependency moduleDataToContext =
    { name = InspectNameInheritFromParent
    , targetToContext = InspectDirectDependencyToContext moduleDataToContext
    }
        |> Rope.singleton


inspectDirectIndependency : ({ name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> context) -> Inspect context
inspectDirectIndependency moduleDataToContext =
    { name = InspectNameInheritFromParent
    , targetToContext = InspectIndirectDependencyToContext moduleDataToContext
    }
        |> Rope.singleton


{-| Creates a schema for a project rule. Will require adding project visitors and calling
[`finish`](#finish) to create a usable [`Rule`](#Rule).

The first argument is the rule name. I _highly_ recommend naming it just like the
module name (including all the `.` there may be).

The second argument is the initial `context`, i.e. the data that the rule will
accumulate as the project will be traversed, and allows the rule to know/remember
what happens in other parts of the project.

**NOTE**: Do not store functions, JSON values or regular expressions in your project context, as they will be
compared internally, which [may cause Elm to crash](https://package.elm-lang.org/packages/elm/core/latest/Basics#==).

Project rules traverse the project in the following order:

  - Read and/or report errors in project files [`ProjectGlobalData`](#ProjectGlobalData)
  - The Elm modules one by one, visited by [`withModuleVisitor`](#withModuleVisitor),
    following the same traversal order as for module rules but without reading the project files (`elm.json`, ...).
  - A final evaluation when all modules have been visited, using [`withFinalProjectEvaluation`](#withFinalProjectEvaluation)

Evaluating/visiting a node means two things:

  - Detecting patterns and reporting errors
  - Collecting data in a "context", which will be either a `context` or a `moduleContext` depending on the part of the project being visited, to have more information available in a later
    part of the traversal evaluation.

-}
create :
    { name : String
    , contextMerge : context -> context -> context
    , contextSerialize : Serialize.Codec String context
    , inspect : List (Inspect context)
    , report : ContextToErrors context
    }
    -> Review
create definition =
    definition.inspect
        |> Rope.fromList
        |> Rope.concat
        |> Rope.foldl
            (\inspectSingle soFar ->
                let
                    name : String
                    name =
                        case inspectSingle.name of
                            InspectNameInheritFromParent ->
                                definition.name

                            InspectName customName ->
                                customName
                in
                case inspectSingle.targetToContext of
                    InspectDirectDependencyToContext toContext ->
                        { soFar | directDependencyToContext = soFar.directDependencyToContext |> Dict.insert name toContext }

                    InspectIndirectDependencyToContext toContext ->
                        { soFar | indirectDependencyToContext = soFar.indirectDependencyToContext |> Dict.insert name toContext }

                    InspectElmJsonToContext toContext ->
                        { soFar | elmJsonToContext = soFar.elmJsonToContext |> Dict.insert name toContext }

                    InspectReadmeToContext toContext ->
                        { soFar | readmeToContext = soFar.readmeToContext |> Dict.insert name toContext }

                    InspectProjectModuleToContext toContext ->
                        { soFar | projectModuleToContext = soFar.projectModuleToContext |> Dict.insert name toContext }
            )
            { name = definition.name
            , contextMerge = definition.contextMerge
            , contextSerialize = definition.contextSerialize
            , directDependencyToContext = Dict.empty
            , indirectDependencyToContext = Dict.empty
            , elmJsonToContext = Dict.empty
            , readmeToContext = Dict.empty
            , projectModuleToContext = Dict.empty
            , toErrors = definition.report
            }
        |> toReview


emptyCache : Cache
emptyCache =
    { elmJsonContext = Nothing
    , readmeContext = Nothing
    , indirectDependencyContexts = Dict.empty
    , directDependencyContexts = Dict.empty
    , moduleContexts = Dict.empty
    }


{-| A key to be able to report an error for a specific module. You need such a
key in order to use the [`errorForModule`](#errorForModule) function. This is to
prevent creating errors for modules you have not visited, or files that do not exist.

You can get a `ModuleKey` from the `projectToModule` and `moduleToProject`
functions that you define when using [`named`](#named).

-}
type ModuleKey
    = ModuleKey { path : String }


{-| A key to be able to report an error for the `elm.json` file. You need this
key in order to use the [`errorForElmJson`](#errorForElmJson) function. This is
to prevent creating errors for it if you have not visited it.

You can get a `ElmJsonKey` using the [`withElmJsonVisitor`](#withElmJsonVisitor) function.

-}
type ElmJsonKey
    = ElmJsonKey
        { path : String
        , raw : String
        , project : Elm.Project.Project
        }


{-| A key to be able to report an error for the `README.md` file. You need this
key in order to use the [`errorForReadme`](#errorForReadme) function. This is
to prevent creating errors for it if you have not visited it.

You can get a `ReadmeKey` using the [`withReadmeVisitor`](#withReadmeVisitor) function.

-}
type ReadmeKey
    = ReadmeKey
        { path : String
        , content : String
        }


{-| There are situations where you don't want review rules to report errors:

1.  You copied and updated over an external library because one of your needs wasn't met, and you don't want to modify it more than necessary.
2.  Your project contains generated source code, over which you have no control or for which you do not care that some rules are enforced (like the reports of unused variables).
3.  You want to introduce a rule progressively, because there are too many errors in the project for you to fix in one go. You can then ignore the parts of the project where the problem has not yet been solved, and fix them as you go.
4.  You wrote a rule that is very specific and should only be applied to a portion of your code.
5.  You wish to disable some rules for tests files (or enable some only for tests).

You can use the following functions to ignore errors in directories or files, or only report errors found in specific directories or files.

**NOTE**: Even though they can be used to disable any errors, I **strongly recommend against**
doing so if you are not in the situations listed above. I highly recommend you
leave a comment explaining the reason why you use these functions, or to
communicate with your colleagues if you see them adding exceptions without
reason or seemingly inappropriately.

-}
ignoreErrorsForFilesWhere : (String -> Bool) -> (Review -> Review)
ignoreErrorsForFilesWhere filterOut =
    \review ->
        { review
            | ignoreErrorsForFiles =
                \path ->
                    (path |> review.ignoreErrorsForFiles) || (path |> filterOut)
        }


type alias ProjectFileCache =
    ProjectFileCache.Entry Serialized


type alias FinalProjectEvaluationCache =
    EndAnalysisCache.Entry Serialized


{-| Review a project and gives back the errors raised by the given rules.

Note that you won't need to use this function when writing a rule. You should
only need it if you try to make `elm-review` run in a new environment.

    import Review.Project as Project exposing (Project)
    import Review.Rule as Rule exposing (Rule)

    config : List Rule
    config =
        [ Some.Rule.rule
        , Some.Other.Rule.rule
        ]

    project : Project
    project =
        Project.new
            |> Project.addModule { path = "src/A.elm", source = "module A exposing (a)\na = 1" }
            |> Project.addModule { path = "src/B.elm", source = "module B exposing (b)\nb = 1" }

    doReview =
        let
            { errors, rules, projectData, extracts } =
                -- Replace `config` by `rules` next time you call reviewV2
                -- Replace `Nothing` by `projectData` next time you call reviewV2
                Rule.review config Nothing project
        in
        doSomethingWithTheseValues

The resulting `List Rule` is the same list of rules given as input, but with an
updated internal cache to make it faster to re-run the rules on the same project.
If you plan on re-reviewing with the same rules and project, for instance to
review the project after a file has changed, you may want to store the rules in
your `Model`.

The rules are functions, so doing so will make your model unable to be
exported/imported with `elm/browser`'s debugger, and may cause a crash if you try
to compare them or the model that holds them.

-}
run :
    Review
    ->
        { fixMode : FixMode
        , ignoreFix : { ruleName : String, filePath : String } -> Bool
        }
    ->
        { modules :
            List
                { path : String
                , source : String
                }
        , elmJson : Maybe { path : String, raw : String }
        , readme : Maybe { path : String, content : String }
        , indirectDependencies : List { name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
        , directDependencies : List { name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
        }
    -> (Review -> { fixedErrors : FixedErrors, cache : Cache })
run review reviewOptions =
    \project ->
        Debug.todo ""


type alias FixedErrors =
    { count : Int
    , errors : Dict String (List Error)
    , shouldAbort : Bool
    }


fixedErrorsEmpty : FixedErrors
fixedErrorsEmpty =
    { count = 0
    , errors = Dict.empty
    , shouldAbort = False
    }


fixedErrorsInsert : Error -> FixedErrors -> FixedErrors
fixedErrorsInsert error =
    \fixedErrors ->
        { count = fixedErrors.count + 1
        , errors =
            Dict.update
                (error.target |> errorTargetFilePath)
                (\errors -> Just (error :: Maybe.withDefault [] errors))
                fixedErrors.errors
        , shouldAbort =
            case error.target of
                ErrorTargetProjectElmJson _ ->
                    True

                _ ->
                    fixedErrors.shouldAbort
        }


type FixMode
    = Disabled
    | Enabled (Maybe Int)


shouldApplyFix :
    { fixMode : FixMode
    , ignoreFix : { ruleName : String, filePath : String } -> Bool
    }
    -> Maybe ({ ruleName : String, filePath : String } -> Bool)
shouldApplyFix reviewOptionsData =
    case reviewOptionsData.fixMode of
        Enabled _ ->
            -- TODO Only look for errors in rules that mention they will provide fixes.
            Just (\err -> not (reviewOptionsData.ignoreFix err))

        Disabled ->
            Nothing


shouldContinueLookingForFixes :
    { fixMode : FixMode
    , ignoreFix : { ruleName : String, filePath : String } -> Bool
    }
    -> FixedErrors
    -> Bool
shouldContinueLookingForFixes reviewOptionsData fixedErrors =
    case reviewOptionsData.fixMode of
        Enabled (Just fixLimit) ->
            not (.shouldAbort fixedErrors) && (fixLimit > .count fixedErrors)

        Enabled Nothing ->
            not (.shouldAbort fixedErrors)

        Disabled ->
            False



-- VISIT PROJECT


type alias Cache =
    { elmJsonContext : Maybe ProjectFileCache
    , readmeContext : Maybe ProjectFileCache
    , indirectDependencyContexts : Dict String ProjectFileCache
    , directDependencyContexts : Dict String ProjectFileCache
    , moduleContexts : Dict String ProjectFileCache
    }


errorTargetFilePath : ErrorTarget -> String
errorTargetFilePath =
    \errorTarget ->
        case errorTarget of
            ErrorTargetProjectModule (ModuleKey moduleInfo) ->
                moduleInfo.path

            ErrorTargetProjectElmJson (ElmJsonKey elmJsonInfo) ->
                elmJsonInfo.path

            ErrorTargetReadme (ReadmeKey readmeInfo) ->
                readmeInfo.path


declarationToFunction : Node Declaration -> Maybe Function
declarationToFunction declaration =
    case Node.value declaration of
        Declaration.FunctionDeclaration functionDeclaration ->
            functionDeclaration |> Just

        _ ->
            Nothing


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[expressions](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Expression)
(`1`, `True`, `add 1 2`, `1 + 2`), collect data in the `context` and/or report patterns.
The expressions are visited in pre-order depth-first search, meaning that an
expression will be visited, then its first child, the first child's children
(and so on), then the second child (and so on).

Contrary to [`withExpressionVisitor`](#withExpressionVisitor), the
visitor function will be called only once, when the expression is "entered",
meaning before its children are visited.

The following example forbids the use of `Debug.log` even when it is imported like
`import Debug exposing (log)`.

    import Elm.Syntax.Exposing as Exposing exposing (TopLevelExpose)
    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Import exposing (Import)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Rule)

    type Context
        = DebugLogWasNotImported
        | DebugLogWasImported

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebugEvenIfImported" DebugLogWasNotImported
            |> Rule.withImportVisitor importVisitor
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    importVisitor : Node Import -> Context -> ( List Rule.Error, Context )
    importVisitor node context =
        case ( Node.value node |> .moduleName |> Node.value, (Node.value node).exposingList |> Maybe.map Node.value ) of
            ( [ "Debug" ], Just (Exposing.All _) ) ->
                ( [], DebugLogWasImported )

            ( [ "Debug" ], Just (Exposing.Explicit exposedFunctions) ) ->
                let
                    isLogFunction : Node Exposing.TopLevelExpose -> Bool
                    isLogFunction exposeNode =
                        case Node.value exposeNode of
                            Exposing.FunctionExpose "log" ->
                                True

                            _ ->
                                False
                in
                if List.any isLogFunction exposedFunctions then
                    ( [], DebugLogWasImported )

                else
                    ( [], DebugLogWasNotImported )

            _ ->
                ( [], DebugLogWasNotImported )

    expressionVisitor : Node Expression -> Context -> ( List Rule.Error, Context )
    expressionVisitor node context =
        case context of
            DebugLogWasNotImported ->
                ( [], context )

            DebugLogWasImported ->
                case Node.value node of
                    Expression.FunctionOrValue [] "log" ->
                        ( [ Rule.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "The `Debug` module is useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production." ]
                                }
                                (Node.range node)
                          ]
                        , context
                        )

                    _ ->
                        ( [], context )

-}
functionDeclarationExpression : Function -> Node Expression
functionDeclarationExpression functionDeclaration =
    Node.value functionDeclaration.declaration |> .expression


{-| Feels simple enough to implement on the fly. TODO remove?
-}
expressionFold : (Node Expression -> (folded -> folded)) -> folded -> Node Expression -> folded
expressionFold reduce initialFolded node =
    (node :: expressionSubs node)
        |> List.foldl reduce initialFolded


extractSourceCode : List String -> Range -> String
extractSourceCode lines range =
    lines
        |> List.drop (range.start.row - 1)
        |> List.take (range.end.row - range.start.row + 1)
        |> mapLast (String.slice 0 (range.end.column - 1))
        |> String.join "\n"
        |> String.dropLeft (range.start.column - 1)


mapLast : (a -> a) -> List a -> List a
mapLast mapper lines =
    case List.reverse lines of
        [] ->
            lines

        first :: rest ->
            List.reverse (mapper first :: rest)


expressionSubs : Node Expression -> List (Node Expression)
expressionSubs node =
    case Node.value node of
        Expression.Application expressions ->
            expressions

        Expression.ListExpr elements ->
            elements

        Expression.RecordExpr fields ->
            List.map (\(Node _ ( _, expr )) -> expr) fields

        Expression.RecordUpdateExpression _ setters ->
            List.map (\(Node _ ( _, expr )) -> expr) setters

        Expression.ParenthesizedExpression expr ->
            [ expr ]

        Expression.OperatorApplication _ direction left right ->
            case direction of
                Infix.Left ->
                    [ left, right ]

                Infix.Right ->
                    [ right, left ]

                Infix.Non ->
                    [ left, right ]

        Expression.IfBlock cond then_ else_ ->
            [ cond, then_, else_ ]

        Expression.LetExpression { expression, declarations } ->
            List.foldr
                (\declaration acc ->
                    case Node.value declaration of
                        Expression.LetFunction function ->
                            functionDeclarationExpression function :: acc

                        Expression.LetDestructuring _ expr ->
                            expr :: acc
                )
                [ expression ]
                declarations

        Expression.CaseExpression { expression, cases } ->
            expression
                :: List.map (\( _, caseExpression ) -> caseExpression) cases

        Expression.LambdaExpression { expression } ->
            [ expression ]

        Expression.TupledExpression expressions ->
            expressions

        Expression.Negation expr ->
            [ expr ]

        Expression.RecordAccess expr _ ->
            [ expr ]

        _ ->
            []


moduleHeaderNameNode : Node Module -> Node ModuleName
moduleHeaderNameNode node =
    case Node.value node of
        Module.NormalModule data ->
            data.moduleName

        Module.PortModule data ->
            data.moduleName

        Module.EffectModule data ->
            data.moduleName


{-| Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's documentation, collect data in
the `context` and/or report patterns.

This visitor will give you access to the module documentation comment. Modules don't always have a documentation.
When that is the case, the visitor will be called with the `Nothing` as the module documentation.

-}
moduleHeaderDocumentation : Elm.Syntax.File.File -> Maybe (Node String)
moduleHeaderDocumentation ast =
    let
        cutOffLine : Int
        cutOffLine =
            case ast.imports of
                firstImport :: _ ->
                    (Node.range firstImport).start.row

                [] ->
                    case ast.declarations of
                        firstDeclaration :: _ ->
                            (Node.range firstDeclaration).start.row

                        [] ->
                            -- Should not happen, as every module should have at least one declaration
                            0
    in
    findModuleDocumentationBeforeCutOffLine cutOffLine ast.comments


findModuleDocumentationBeforeCutOffLine : Int -> List (Node String) -> Maybe (Node String)
findModuleDocumentationBeforeCutOffLine cutOffLine comments =
    case comments of
        [] ->
            Nothing

        ((Node range content) as comment) :: restOfComments ->
            if range.start.row > cutOffLine then
                Nothing

            else if String.startsWith "{-|" content then
                Just comment

            else
                findModuleDocumentationBeforeCutOffLine cutOffLine restOfComments


{-| All the info you can get from a module in a review context.


### ast

Request the full [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) for the current module.

This can be useful if you wish to avoid initializing the module context with dummy data future node visits can replace them.

For instance, if you wish to know what is exposed from a module, you may need to visit the module definition and then
the list of declarations. If you need this information earlier on, you will have to provide dummy data at context
initialization and store some intermediary data.

Using the full AST, you can simplify the implementation by computing the data in the context creator, without the use of visitors.


### module definition

Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[module definition](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Module) (`module SomeModuleName exposing (a, b)`), collect data in the `context` and/or report patterns.

The following example forbids the use of `Html.button` except in the "Button" module.
The example is simplified to only forbid the use of the `Html.button` expression.

    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Module as Module exposing (Module)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Rule)

    type Context
        = HtmlButtonIsAllowed
        | HtmlButtonIsForbidden

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoHtmlButton" HtmlButtonIsForbidden
            |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    moduleDefinitionVisitor : Node Module -> Context -> ( List Rule.Error, Context )
    moduleDefinitionVisitor node context =
        if (Node.value node |> Module.moduleName) == [ "Button" ] then
            ( [], HtmlButtonIsAllowed )

        else
            ( [], HtmlButtonIsForbidden )

    expressionVisitor : Node Expression -> Context -> ( List Rule.Error, Context )
    expressionVisitor node context =
        case context of
            HtmlButtonIsAllowed ->
                ( [], context )

            HtmlButtonIsForbidden ->
                case Node.value node of
                    Expression.FunctionOrValue [ "Html" ] "button" ->
                        ( [ Rule.error
                                { message = "Do not use `Html.button` directly"
                                , details = [ "At fruits.com, we've built a nice `Button` module that suits our needs better. Using this module instead of `Html.button` ensures we have a consistent button experience across the website." ]
                                }
                                (Node.range node)
                          ]
                        , context
                        )

                    _ ->
                        ( [], context )

            _ ->
                ( [], context )

Tip: The rule above is very brittle. What if `button` was imported using `import Html exposing (button)` or `import Html exposing (..)`, or if `Html` was aliased (`import Html as H`)? Then the rule above would
not catch and report the use `Html.button`. To handle this, check out [`withModuleNameLookupTable`](#withModuleNameLookupTable).

Through a couple of helpers, you can get to the specific information you need faster:

  - [`moduleHeaderNameNode`](#moduleHeaderNameNode)
  - [`moduleHeaderDocumentation`](#moduleHeaderDocumentation)


### imports

Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[import statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Import)
(`import Html as H exposing (div)`) in order of their definition, collect data
in the `context` and/or report patterns.

The following example forbids importing both `Element` (`elm-ui`) and
`Html.Styled` (`elm-css`).

    import Elm.Syntax.Import exposing (Import)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Rule)

    type alias Context =
        { elmUiWasImported : Bool
        , elmCssWasImported : Bool
        }

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoUsingBothHtmlAndHtmlStyled" initialContext
            |> Rule.withImportVisitor importVisitor
            |> Rule.fromModuleRuleSchema

    initialContext : Context
    initialContext =
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

    importVisitor : Node Import -> Context -> ( List Rule.Error, Context )
    importVisitor node context =
        case Node.value node |> .moduleName |> Node.value of
            [ "Element" ] ->
                if context.elmCssWasImported then
                    ( [ error node ]
                    , { context | elmUiWasImported = True }
                    )

                else
                    ( [ error node ]
                    , { context | elmUiWasImported = True }
                    )

            [ "Html", "Styled" ] ->
                if context.elmUiWasImported then
                    ( [ error node ]
                    , { context | elmCssWasImported = True }
                    )

                else
                    ( [ error node ]
                    , { context | elmCssWasImported = True }
                    )

            _ ->
                ( [], context )

This example was written in a different way in the example for [`withFinalModuleEvaluation`](#withFinalModuleEvaluation).


### comments

Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's comments, collect data in
the `context` and/or report patterns.

This visitor will give you access to the list of comments (in source order) in
the module all at once. Note that comments that are parsed as documentation comments by
[`elm-syntax`](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/)
are not included in this list.

As such, the following comments are included (✅) / excluded (❌):

  - ✅ Module documentation (`{-| -}`)
  - ✅ Port documentation comments (`{-| -}`)
  - ✅ Top-level comments not internal to a function/type/etc.
  - ✅ Comments internal to a function/type/etc.
  - ❌ Function/type/type alias documentation comments (`{-| -}`)

Tip: If you only need to access the module documentation, you should use
[`withModuleDocumentationVisitor`](#withModuleDocumentationVisitor) instead.


### declarations

Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`),
collect data and/or report patterns. The declarations will be visited in the order of their definition.

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
    moduleDefinitionVisitor node context =
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
    declarationVisitor node direction context =
        case Node.value node of
            Declaration.FunctionDeclaration { documentation, declaration } ->
                let
                    functionName : String
                    functionName =
                        Node.value declaration |> .name |> Node.value
                in
                if documentation == Nothing && isExposed context functionName then
                    ( [ Rule.error
                            { message = "Exposed function " ++ functionName ++ " is missing a type annotation"
                            , details =
                                [ "Type annotations are very helpful for people who use the module. It can give a lot of information without having to read the contents of the function."
                                , "To add a type annotation, add a line like `" functionName ++ " : ()`, and replace the `()` by the type of the function. If you don't replace `()`, the compiler should give you a suggestion of what the type should be."
                                ]
                            }
                            (Node.range node)
                      ]
                    , context
                    )

                else
                    ( [], context )

            _ ->
                ( [], context )

    isExposed : ExposedFunctions -> String -> Bool
    isExposed exposedFunctions name =
        case exposedFunctions of
            All ->
                True

            OnlySome exposedList ->
                List.member name exposedList


#### on exit

Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`),
collect data and/or report patterns. The declarations will be visited in the order of their definition.

The following example reports unused parameters from top-level declarations.

    import Elm.Syntax.Declaration as Declaration exposing (Declaration)
    import Elm.Syntax.Expression as Expression exposing (Expression)
    import Elm.Syntax.Node as Node exposing (Node)
    import Review.Rule as Rule exposing (Rule)

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoDebugEvenIfImported" DebugLogWasNotImported
            |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
            |> Rule.withDeclarationExitVisitor declarationExitVisitor
            -- Omitted, but this marks parameters as used
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    declarationEnterVisitor : Node Declaration -> Context -> ( List Rule.Error, Context )
    declarationEnterVisitor node context =
        case Node.value node of
            Declaration.FunctionDeclaration function ->
                ( [], registerArguments context function )

            _ ->
                ( [], context )

    declarationExitVisitor : Node Declaration -> Context -> ( List Rule.Error, Context )
    declarationExitVisitor node context =
        case Node.value node of
            -- When exiting the function expression, report the parameters that were not used.
            Declaration.FunctionDeclaration function ->
                ( unusedParameters context |> List.map createError, removeArguments context )

            _ ->
                ( [], context )


### TODO merge with declarations documentation

Add a visitor to the [`ModuleRuleSchema`](#ModuleRuleSchema) which will visit the module's
[declaration statements](https://package.elm-lang.org/packages/stil4m/elm-syntax/7.2.1/Elm-Syntax-Declaration)
(`someVar = add 1 2`, `type Bool = True | False`, `port output : Json.Encode.Value -> Cmd msg`),
to collect data and/or report patterns. The declarations will be in the same
order that they appear in the source code.

It is similar to [withDeclarationVisitor](#withDeclarationVisitor), but the
visitor used with this function is called before the visitor added with
[withDeclarationVisitor](#withDeclarationVisitor). You can use this visitor in
order to look ahead and add the module's types and variables into your context,
before visiting the contents of the module using [withDeclarationVisitor](#withDeclarationVisitor)
and [withExpressionEnterVisitor](#withExpressionEnterVisitor). Otherwise, using
[withDeclarationVisitor](#withDeclarationVisitor) is probably a simpler choice.


### module key

The [module key](#ModuleKey) for this module.

    rule : Rule
    rule =
        Rule.named "NoMissingSubscriptionsCall" initialProjectContext
            |> Rule.withModuleVisitor
                { moduleVisitor = moduleVisitor
                , foldProjectContexts = foldProjectContexts
                }

    moduleToProject : Rule.ContextCreator Context
    moduleToProject =
        Rule.createContext
            (\moduleKey -> { moduleKey = moduleKey })
            |> Rule.withModuleKey


### filePath

Request the file path for this module, relative to the project's `elm.json`.

Using [`newModuleRuleSchema`](#newModuleRuleSchema):

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "YourRuleName" initialContext
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    initialContext : Rule.ContextCreator () Context
    initialContext =
        Rule.createContext
            (\filePath () -> { filePath = filePath })
            |> Rule.withFilePath

Using [`withModuleContext`](#withModuleContext) in a project rule:

    rule : Rule
    rule =
        Rule.named "YourRuleName" initialProjectContext
            |> Rule.withModuleVisitor moduleVisitor
            |> Rule.withModuleContext
                { projectToModule = projectToModule
                , moduleToProject = moduleToProject
                , foldProjectContexts = foldProjectContexts
                }

    moduleToProject : Rule.ContextCreator Context
    moduleToProject =
        Rule.createContext
            (\filePath -> { filePath = filePath })
            |> Rule.withFilePath


### extractSourceCode

Requests access to a function that gives you the source code at a given range.

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "YourRuleName" initialContext
            |> Rule.withExpressionEnterVisitor expressionVisitor
            |> Rule.fromModuleRuleSchema

    type alias Context =
        { extractSourceCode : Range -> String
        }

    initialContext : Rule.ContextCreator Context
    initialContext =
        Rule.createContext
            (\extractSourceCode -> { extractSourceCode = extractSourceCode })
            |> Rule.withSourceCodeExtractor

The motivation for this capability was for allowing to provide higher-quality fixes, especially where you'd need to **move** or **copy**
code from one place to another (example: [when switching the branches of an if expression](https://github.com/jfmengels/elm-review/blob/master/tests/NoNegationInIfCondition.elm)).

I discourage using this functionality to explore the source code, as the different visitor functions make for a nicer
experience.


### isFileIgnored

Request to know whether the errors for the current module has been ignored for this particular rule.
This may be useful to reduce the amount of work related to ignored files — like collecting unnecessary data or reporting
errors — when that will ignored anyway.

Note that for module rules, ignored files will be skipped automatically anyway.

    contextCreator : Rule.ContextCreator Context
    contextCreator =
        Rule.createContext
            (\isFileIgnored ->
                { isFileIgnored = isFileIgnored

                -- ...other fields
                }
            )
            |> Rule.withIsFileIgnored

-}
type alias ModuleData =
    { fileSyntax : Elm.Syntax.File.File
    , key : ModuleKey
    , rawSourceCodeLines : List String
    , filePath : String
    }


{-| Represents (part of a) fix that will be applied to a file's source code in order to
automatically fix a review error.
-}
type Fix
    = FixRangeReplacement { range : Range, replacement : String }


{-| Remove the code in between a range.
-}
fixRemoveRange : Range -> Fix
fixRemoveRange rangeToRemove =
    fixReplaceRangeBy rangeToRemove ""


{-| Replace the code in between a range by some other code.
-}
fixReplaceRangeBy : Range -> String -> Fix
fixReplaceRangeBy range replacement =
    FixRangeReplacement { range = range, replacement = replacement }


{-| Insert some code at the given position.
-}
fixInsertAt : Location -> String -> Fix
fixInsertAt location toInsert =
    fixReplaceRangeBy { start = location, end = location } toInsert



-- APPLY FIX


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
                        getRowAtLine lines (replace.range.start.row - 1)
                            |> Unicode.left (replace.range.start.column - 1)

                    endLine : String
                    endLine =
                        getRowAtLine lines (replace.range.end.row - 1)
                            |> Unicode.dropLeft (replace.range.end.column - 1)
                in
                List.concat
                    [ linesBefore
                    , startLine ++ replace.replacement ++ endLine |> String.lines
                    , linesAfter
                    ]


getRowAtLine : List String -> Int -> String
getRowAtLine lines rowIndex =
    case lines |> List.drop rowIndex |> List.head of
        Just line ->
            if String.trim line /= "" then
                line

            else
                ""

        Nothing ->
            ""



-- FIX ELM MODULE


{-| Apply the changes on the source code.
-}
fixModule : List Fix -> String -> Result FixProblem { source : String, ast : File }
fixModule fixes originalSourceCode =
    case tryToApplyFix fixes originalSourceCode of
        Ok fixedSourceCode ->
            case Elm.Parser.parseToFile fixedSourceCode of
                Ok ast ->
                    Ok { source = fixedSourceCode, ast = ast }

                Err _ ->
                    Err (AfterFixSourceParsingFailed fixedSourceCode)

        Err err ->
            Err err


{-| Apply the changes on the elm.json file.
-}
fixElmJson : List Fix -> String -> Result FixProblem { raw : String, project : Elm.Project.Project }
fixElmJson fixes originalSourceCode =
    case tryToApplyFix fixes originalSourceCode of
        Ok resultAfterFix ->
            case Json.Decode.decodeString Elm.Project.decoder resultAfterFix of
                Ok project ->
                    Ok { raw = resultAfterFix, project = project }

                Err _ ->
                    Err (AfterFixSourceParsingFailed resultAfterFix)

        Err err ->
            Err err


{-| Apply the changes on the README.md file.
-}
fixReadme : List Fix -> String -> Result FixProblem String
fixReadme fixes originalSourceCode =
    tryToApplyFix fixes originalSourceCode


type FixProblem
    = AfterFixIsUnchanged
    | AfterFixSourceParsingFailed String
    | FixHasCollisionsInRanges


tryToApplyFix : List Fix -> String -> Result FixProblem String
tryToApplyFix fixes sourceCode =
    if containRangeCollisions fixes then
        Err FixHasCollisionsInRanges

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
                    |> List.foldl applyFix (String.lines sourceCode)
                    |> String.join "\n"
        in
        if sourceCode == resultAfterFix then
            Err AfterFixIsUnchanged

        else
            Ok resultAfterFix


containRangeCollisions : List Fix -> Bool
containRangeCollisions fixes =
    fixes |> List.map fixRange |> listAnyPair rangesCollide


listAnyPair : (element -> element -> Bool) -> (List element -> Bool)
listAnyPair isFound =
    \list ->
        case list of
            [] ->
                False

            head :: tail ->
                if List.any (\tailElement -> isFound head tailElement) tail then
                    True

                else
                    listAnyPair isFound tail


fixRange : Fix -> Range
fixRange fix_ =
    case fix_ of
        FixRangeReplacement replace ->
            replace.range


rangesCollide : Range -> Range -> Bool
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


comparePosition : Location -> Location -> Order
comparePosition a b =
    case compare a.row b.row of
        EQ ->
            compare a.column b.column

        LT ->
            LT

        GT ->
            GT


{-| Apply the changes on the source code.
-}
fix : ErrorTarget -> List Fix -> String -> String
fix target fixes sourceCode =
    case target of
        ErrorTargetReadme _ ->
            tryToApplyFix fixes sourceCode
                |> resultValueOrOnError
                    (\_ -> sourceCode)

        ErrorTargetProjectModule _ ->
            case tryToApplyFix fixes sourceCode of
                Err _ ->
                    sourceCode

                Ok fixed ->
                    case fixed |> Elm.Parser.parseToFile of
                        Ok _ ->
                            fixed

                        Err _ ->
                            sourceCode

        ErrorTargetProjectElmJson _ ->
            case tryToApplyFix fixes sourceCode of
                Err _ ->
                    sourceCode

                Ok fixed ->
                    case Json.Decode.decodeString Elm.Project.decoder fixed of
                        Ok _ ->
                            fixed

                        Err _ ->
                            sourceCode


resultValueOrOnError : (error -> value) -> (Result error value -> value)
resultValueOrOnError errorToValue =
    \result ->
        case result of
            Ok value ->
                value

            Err error ->
                error |> errorToValue
