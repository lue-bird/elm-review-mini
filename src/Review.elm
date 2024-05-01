module Review exposing
    ( Review, ignoreErrorsForFilesWhere
    , create
    , Inspect, InspectSingleTargetToContext(..), inspectElmJson, inspectModule, inspectDirectDependencies, inspectExtraFile
    , inspectBatch, inspectMap
    , FileTarget(..)
    , Fix(..), FixError(..), fixInsertAt, fixRemoveRange, fixReplaceRangeBy
    , expressionSubs, moduleHeaderDocumentation, moduleHeaderNameNode
    , sourceExtractInRange
    , elmJsonToElmProjectFiles, run, Cache, cacheEmpty, fixFile
    , ContextGeneric(..)
    )

{-| `elm-review-mini` scans the modules, `elm.json`, dependencies and extra files like `README.md` from your project.
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

@docs create


## inspecting

Look at the global picture of an Elm project to collect context.

@docs Inspect, InspectSingleTargetToContext, inspectElmJson, inspectModule, inspectDirectDependencies, inspectExtraFile
@docs inspectBatch, inspectMap


## reporting

@docs FileTarget
@docs Fix, FixError, fixInsertAt, fixRemoveRange, fixReplaceRangeBy


## helpers

@docs expressionSubs, moduleHeaderDocumentation, moduleHeaderNameNode
@docs sourceExtractInRange


# safe internals

@docs elmJsonToElmProjectFiles, run, Cache, cacheEmpty, fixFile
@docs ContextGeneric

-}

import Codec
import Dict exposing (Dict)
import Elm.Constraint
import Elm.Docs
import Elm.Package
import Elm.Parser
import Elm.Project
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Infix
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Version
import Json.Decode
import Json.Encode
import ListExtra
import Rope exposing (Rope)
import Set
import Unicode


{-| A construct that can inspect your project files and report errors.

You can create one with [`Review.create`](Review#create)

-}
type alias Review =
    { name : String
    , ignoreErrorsForFiles : String -> Bool
    , run :
        { elmJson : { source : String, project : Elm.Project.Project }
        , directDependencies : List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
        , addedOrChangedModulesByPath : Dict String String
        , removedModulePaths : List String
        , addedOrChangedExtraFilesByPath : Dict String String
        , removedExtraFilePaths : List String
        , cache : Cache
        }
        ->
            { errorsByPath : Dict String (List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Fix })
            , cache : Cache
            }
    }


{-| The collected info from scanning a project in a generic format that can be used for caching.
See [`Inspect`](#Inspect)
-}
type ContextGeneric
    = ContextJsonString String


{-| How to collect info from scanning a project.
-}
type alias Inspect context =
    Rope (InspectSingleTargetToContext context)


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
inspectBatch : List (Inspect context) -> Inspect context
inspectBatch inspects =
    inspects |> Rope.fromList |> Rope.concat


{-| The smallest possible thing you can inspect and create context from
-}
type InspectSingleTargetToContext context
    = InspectDirectDependenciesToContext (List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> context)
    | InspectElmJsonToContext (Elm.Project.Project -> context)
    | InspectExtraFileToContext ({ path : String, content : String } -> context)
    | InspectProjectModuleToContext ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> context)


{-| Reference to a project file.
For modules and extra files, use `FileTarget` with the path from the inspect.
To refer to the elm.json, use `FileTargetElmJson`
-}
type FileTarget
    = FileTargetElmJson
    | FileTarget { path : String }


{-| Usually, when using an existing [`inspectComposable`](#inspectComposable)
your context type and its context type don't match up.

Use [`inspectMap`](#inspectMap) to use all that information to create your own context from it

-}
inspectMap : (context -> contextMapped) -> (Inspect context -> Inspect contextMapped)
inspectMap contextChange =
    \inspect ->
        inspect
            |> Rope.map
                (\inspectSingle ->
                    inspectSingle |> inspectSingleTargetToContextMap contextChange
                )


inspectSingleTargetToContextMap : (context -> contextMapped) -> (InspectSingleTargetToContext context -> InspectSingleTargetToContext contextMapped)
inspectSingleTargetToContextMap contextChange =
    \inspectSingle ->
        case inspectSingle of
            InspectDirectDependenciesToContext toContext ->
                (\data -> data |> toContext |> contextChange) |> InspectDirectDependenciesToContext

            InspectElmJsonToContext toContext ->
                (\data -> data |> toContext |> contextChange) |> InspectElmJsonToContext

            InspectExtraFileToContext toContext ->
                (\data -> data |> toContext |> contextChange) |> InspectExtraFileToContext

            InspectProjectModuleToContext toContext ->
                (\data -> data |> toContext |> contextChange) |> InspectProjectModuleToContext


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


### path

The file path, relative to the project's `elm.json`

-}
inspectModule : ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> context) -> Inspect context
inspectModule moduleDataToContext =
    InspectProjectModuleToContext moduleDataToContext |> Rope.singleton


{-| Collect context from the [elm.json project config](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Project#Project)
-}
inspectElmJson : (Elm.Project.Project -> context) -> Inspect context
inspectElmJson moduleDataToContext =
    InspectElmJsonToContext moduleDataToContext |> Rope.singleton


{-| Collect context from a provided non-elm or elm.json file like README.md or CHANGELOG.md.

The provided file path is relative to the project's `elm.json`

-}
inspectExtraFile : ({ path : String, content : String } -> context) -> Inspect context
inspectExtraFile moduleDataToContext =
    InspectExtraFileToContext moduleDataToContext |> Rope.singleton


{-| Collect context from all [project config and docs](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/) you directly depend upon
-}
inspectDirectDependencies : (List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> context) -> Inspect context
inspectDirectDependencies moduleDataToContext =
    InspectDirectDependenciesToContext moduleDataToContext |> Rope.singleton


{-| A new [`Review`](#Review) with

  - `name`: the name of the module this review is defined in including all the `.`s
  - `inspect`: the parts you want to analyze and collect context from. See [`Inspect`](#Inspect)
  - `contextMerge`: assemble contexts from multiple you've collected into one
  - `report`: the final evaluation, turning your collected and merged context into a list of [`Error`](#Error)s
  - `contextCodec`: For big files especially, re-running all the parsing and inspection every time we change a different file
    is costly, so we serialize it for caching using [MartinSStewart/elm-serialize](https://dark.elm.dmy.fr/packages/MartinSStewart/elm-serialize/latest/)

-}
create :
    { name : String
    , inspect : List (Inspect context)
    , contextMerge : context -> context -> context
    , report :
        context
        ->
            List
                { target : FileTarget
                , range : Elm.Syntax.Range.Range
                , message : String
                , details : List String
                , fixes : List Fix
                }
    , contextCodec : Codec.Codec context
    }
    -> Review
create review =
    let
        contextToCache : context -> ContextGeneric
        contextToCache =
            \context ->
                context
                    |> Codec.encodeToString 0 review.contextCodec
                    |> ContextJsonString

        deserialize : ContextGeneric -> Result Json.Decode.Error context
        deserialize =
            \(ContextJsonString serializedBase64) ->
                serializedBase64 |> Codec.decodeString review.contextCodec

        toContexts :
            { directDependenciesToContext : List (List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> context)
            , elmJsonToContext : List (Elm.Project.Project -> context)
            , extraFileToContext : List ({ path : String, content : String } -> context)
            , moduleToContext : List ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> context)
            }
        toContexts =
            review.inspect
                |> Rope.fromList
                |> Rope.concat
                |> inspectToToContexts
    in
    { name = review.name
    , ignoreErrorsForFiles = \_ -> False
    , run =
        \project ->
            let
                contextsFoldToMaybe : List context -> Maybe context
                contextsFoldToMaybe =
                    \contexts ->
                        case contexts of
                            [] ->
                                Nothing

                            one :: others ->
                                others |> List.foldl review.contextMerge one |> Just

                elmJsonContextAndCache : Maybe { context : context, cache : ContextGeneric }
                elmJsonContextAndCache =
                    case project.cache.elmJsonContext of
                        Nothing ->
                            toContexts.elmJsonToContext
                                |> List.map (\f -> f project.elmJson.project)
                                |> contextsFoldToMaybe
                                |> Maybe.map (\context -> { context = context, cache = context |> contextToCache })

                        Just cache ->
                            case cache |> deserialize of
                                Err _ ->
                                    -- corrupt cache
                                    Nothing

                                Ok context ->
                                    { context = context, cache = cache } |> Just

                directDependenciesContextAndCache : Maybe { context : context, cache : ContextGeneric }
                directDependenciesContextAndCache =
                    case project.cache.directDependenciesContext of
                        Nothing ->
                            toContexts.directDependenciesToContext
                                |> List.map (\f -> f project.directDependencies)
                                |> contextsFoldToMaybe
                                |> Maybe.map (\context -> { context = context, cache = context |> contextToCache })

                        Just cache ->
                            case cache |> deserialize of
                                Err _ ->
                                    -- corrupt cache
                                    Nothing

                                Ok context ->
                                    { context = context, cache = cache } |> Just

                moduleToMaybeContext : { path : String, source : String } -> Maybe context
                moduleToMaybeContext moduleFile =
                    case moduleFile.source |> Elm.Parser.parseToFile of
                        Err _ ->
                            Nothing

                        Ok syntax ->
                            let
                                moduleData : { path : String, source : String, syntax : Elm.Syntax.File.File }
                                moduleData =
                                    { path = moduleFile.path, source = moduleFile.source, syntax = syntax }
                            in
                            toContexts.moduleToContext |> List.map (\f -> f moduleData) |> contextsFoldToMaybe

                moduleContexts : List context
                moduleContexts =
                    Dict.merge
                        (\path source soFar -> soFar |> listConsJust ({ path = path, source = source } |> moduleToMaybeContext))
                        (\path new _ soFar -> soFar |> listConsJust ({ path = path, source = new } |> moduleToMaybeContext))
                        (\path contextCache soFar ->
                            case contextCache |> deserialize of
                                Err _ ->
                                    -- corrupt cache
                                    soFar

                                Ok context ->
                                    soFar |> (::) context
                        )
                        project.addedOrChangedModulesByPath
                        (project.cache.moduleContextsByPath
                            |> dictRemoveKeys project.removedModulePaths
                        )
                        []

                extraFileToMaybeContext : { path : String, content : String } -> Maybe context
                extraFileToMaybeContext fileInfo =
                    toContexts.extraFileToContext
                        |> List.map (\f -> f fileInfo)
                        |> contextsFoldToMaybe

                extraFilesContexts : List context
                extraFilesContexts =
                    Dict.merge
                        (\path content soFar ->
                            soFar
                                |> listConsJust
                                    ({ path = path, content = content } |> extraFileToMaybeContext)
                        )
                        (\path new _ soFar ->
                            soFar
                                |> listConsJust
                                    ({ path = path, content = new } |> extraFileToMaybeContext)
                        )
                        (\path contextCache soFar ->
                            case contextCache |> deserialize of
                                Err _ ->
                                    -- corrupt cache
                                    soFar

                                Ok context ->
                                    soFar |> (::) context
                        )
                        project.addedOrChangedExtraFilesByPath
                        (project.cache.extraFileContextsByPath
                            |> dictRemoveKeys project.removedExtraFilePaths
                        )
                        []

                allContexts =
                    (moduleContexts ++ extraFilesContexts)
                        |> listConsJust (directDependenciesContextAndCache |> Maybe.map .context)
                        |> listConsJust (elmJsonContextAndCache |> Maybe.map .context)
            in
            case allContexts |> contextsFoldToMaybe of
                Nothing ->
                    { errorsByPath = Dict.empty, cache = project.cache }

                Just completeContext ->
                    { errorsByPath =
                        (completeContext |> review.report)
                            |> Debug.todo ""
                            |> Dict.map
                                (\_ errors ->
                                    errors |> List.sortWith (\a b -> rangeCompare a.range b.range)
                                )
                    , cache =
                        { elmJsonContext = elmJsonContextAndCache |> Maybe.map .cache
                        , directDependenciesContext = directDependenciesContextAndCache |> Maybe.map .cache
                        , moduleContextsByPath = Debug.todo ""
                        , extraFileContextsByPath = Debug.todo ""
                        }
                    }
    }


listConsJust : Maybe a -> (List a -> List a)
listConsJust maybeNewHead =
    \list ->
        case maybeNewHead of
            Nothing ->
                list

            Just newHead ->
                list |> (::) newHead


inspectToToContexts :
    Inspect context
    ->
        { directDependenciesToContext : List (List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module } -> context)
        , elmJsonToContext : List (Elm.Project.Project -> context)
        , extraFileToContext : List ({ path : String, content : String } -> context)
        , moduleToContext : List ({ syntax : Elm.Syntax.File.File, source : String, path : String } -> context)
        }
inspectToToContexts =
    \inspect ->
        inspect
            |> Rope.foldl
                (\inspectSingle soFar ->
                    case inspectSingle of
                        InspectDirectDependenciesToContext toContext ->
                            { soFar
                                | directDependenciesToContext =
                                    soFar.directDependenciesToContext |> (::) toContext
                            }

                        InspectElmJsonToContext toContext ->
                            { soFar
                                | elmJsonToContext =
                                    soFar.elmJsonToContext |> (::) toContext
                            }

                        InspectExtraFileToContext toContext ->
                            { soFar
                                | extraFileToContext =
                                    soFar.extraFileToContext |> (::) toContext
                            }

                        InspectProjectModuleToContext toContext ->
                            { soFar
                                | moduleToContext =
                                    soFar.moduleToContext |> (::) toContext
                            }
                )
                { directDependenciesToContext = []
                , elmJsonToContext = []
                , extraFileToContext = []
                , moduleToContext = []
                }


{-| A fresh [`Cache`](#Cache) for a fresh session (or single [`run`](#run))
-}
cacheEmpty : Cache
cacheEmpty =
    { elmJsonContext = Nothing
    , directDependenciesContext = Nothing
    , moduleContextsByPath = Dict.empty
    , extraFileContextsByPath = Dict.empty
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


{-| Given the [project's elm.json config](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Project#Project),
find the elm module source directories and direct dependency directory paths from the global elm home.
-}
elmJsonToElmProjectFiles : Elm.Project.Project -> { sourceDirectories : List String, directDependenciesFromElmHome : List String }
elmJsonToElmProjectFiles =
    \elmJson ->
        { sourceDirectories = elmJson |> elmJsonSourceDirectories
        , directDependenciesFromElmHome = elmJson |> elmJsonDirectDependencyNameVersionStrings
        }


elmJsonDirectDependencyNameVersionStrings : Elm.Project.Project -> List String
elmJsonDirectDependencyNameVersionStrings elmJson =
    case elmJson of
        Elm.Project.Application applicationElmJson ->
            List.map
                (\( name, version ) ->
                    (name |> Elm.Package.toString) ++ (version |> Elm.Version.toString)
                )
                (applicationElmJson.depsDirect ++ applicationElmJson.testDepsDirect)

        Elm.Project.Package packageElmJson ->
            List.map
                (\( name, constraint ) ->
                    (name |> Elm.Package.toString) ++ (constraint |> versionConstraintLowerBound)
                )
                (packageElmJson.deps ++ packageElmJson.testDeps)


versionConstraintLowerBound : Elm.Constraint.Constraint -> String
versionConstraintLowerBound =
    \constraint ->
        case constraint |> Elm.Constraint.toString |> String.split " " of
            [] ->
                "1.0.0"

            lowerBoundAsString :: _ ->
                lowerBoundAsString


elmJsonSourceDirectories : Elm.Project.Project -> List String
elmJsonSourceDirectories elmJson =
    case elmJson of
        Elm.Project.Application application ->
            application.dirs |> List.map (\dir -> dir |> removeDotSlashAtBeginning |> pathMakeOSAgnostic |> endWithSlash)

        Elm.Project.Package _ ->
            [ "src/" ]


pathMakeOSAgnostic : String -> String
pathMakeOSAgnostic =
    \path -> path |> String.replace "\\" "/"


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


{-| Review a project and return the errors raised by the given rules.

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

updated internal cache to make it faster to re-run the rules on the same project.
If you plan on re-reviewing with the same rules and project, for instance to
review the project after a file has changed, you may want to store the rules in
your `Model`.

-}
run :
    Review
    ->
        ({ elmJson : { source : String, project : Elm.Project.Project }
         , directDependencies : List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
         , addedOrChangedModulesByPath : Dict String String
         , removedModulePaths : List String
         , addedOrChangedExtraFilesByPath : Dict String String
         , removedExtraFilePaths : List String
         , cache : Cache
         }
         ->
            { errorsByPath : Dict String (List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Fix })
            , cache : Cache
            }
        )
run review =
    \project ->
        let
            runResult :
                { errorsByPath : Dict String (List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Fix })
                , cache : Cache
                }
            runResult =
                project |> review.run
        in
        { cache = runResult.cache
        , errorsByPath =
            runResult.errorsByPath
                |> Dict.foldl
                    (\path errors soFar ->
                        if path |> review.ignoreErrorsForFiles then
                            soFar

                        else
                            soFar |> Dict.insert path errors
                    )
                    Dict.empty
        }


dictRemoveKeys : List comparableKey -> (Dict comparableKey v -> Dict comparableKey v)
dictRemoveKeys listOfKeysToRemove =
    \dict ->
        listOfKeysToRemove
            |> List.foldl (\key soFar -> soFar |> Dict.remove key) dict


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


runOnProjectModules :
    { projectModules : List { path : String, source : String }
    , cache : Dict String ContextGeneric
    }
    ->
        Result
            { fileAtPathFailedToParse : String }
            { contexts : List ContextGeneric, cache : Dict String ContextGeneric }
runOnProjectModules projectModuleInfo =
    projectModuleInfo.projectModules
        |> List.foldl
            (\moduleInfo soFar ->
                case soFar of
                    Err error ->
                        Err error

                    Ok modulesSoFar ->
                        case projectModuleInfo.cache |> Dict.get moduleInfo.path of
                            Just moduleCache ->
                                { modulesSoFar
                                    | contexts =
                                        modulesSoFar.contexts |> (::) (moduleCache |> Debug.todo "")
                                }
                                    |> Ok

                            Nothing ->
                                case moduleInfo.source |> Elm.Parser.parseToFile of
                                    Ok syntaxFile ->
                                        let
                                            moduleData =
                                                { path = moduleInfo.path
                                                , syntax = syntaxFile |> syntaxFileSanitize
                                                , source = moduleInfo.source
                                                }
                                        in
                                        { contexts = modulesSoFar.contexts |> (::) (moduleData |> Debug.todo "to context")
                                        , cache =
                                            modulesSoFar.cache
                                                |> Dict.insert moduleInfo.path
                                                    (moduleData |> Debug.todo "to context and serialize")
                                        }
                                            |> Ok

                                    Err _ ->
                                        Err { fileAtPathFailedToParse = moduleInfo.path }
            )
            ({ contexts = [], cache = projectModuleInfo.cache } |> Ok)


syntaxFileSanitize : Elm.Syntax.File.File -> Elm.Syntax.File.File
syntaxFileSanitize =
    \syntaxFile ->
        { syntaxFile
            | comments =
                syntaxFile.comments
                    |> List.sortBy (\(Elm.Syntax.Node.Node range _) -> ( range.start.row, range.start.column ))
        }


runOnElmJson =
    \elmJson ->
        case elmJson |> .source |> Json.Decode.decodeString Elm.Project.decoder of
            Err _ ->
                Err { fileAtPathFailedToParse = elmJson.path |> Set.singleton }

            Ok project ->
                { path = elmJson.path
                , source = elmJson.source
                , project = project
                }
                    |> Debug.todo ""
                    |> Ok


{-| Cache for the result of the analysis of an inspected project part (modules, elm.json, extra files, direct and indirect dependencies).
-}
type alias Cache =
    { elmJsonContext : Maybe ContextGeneric
    , directDependenciesContext : Maybe ContextGeneric
    , moduleContextsByPath : Dict String ContextGeneric
    , extraFileContextsByPath : Dict String ContextGeneric
    }


{-| Feels simple enough to implement on the fly. TODO remove?
-}
expressionFold : (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> (folded -> folded)) -> folded -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> folded
expressionFold reduce initialFolded node =
    (node :: expressionSubs node)
        |> List.foldl reduce initialFolded


{-| In case you need the read the source in a range verbatim.
This can be nice to keep the user's formatting if you just move code around
-}
sourceExtractInRange : Elm.Syntax.Range.Range -> (String -> String)
sourceExtractInRange range =
    -- TODO implement more performantly using String.foldl
    \string ->
        string
            |> String.lines
            |> List.drop (range.start.row - 1)
            |> List.take (range.end.row - range.start.row + 1)
            |> ListExtra.lastMap (String.slice 0 (range.end.column - 1))
            |> String.join "\n"
            |> String.dropLeft (range.start.column - 1)


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

        Elm.Syntax.Expression.LetExpression { expression, declarations } ->
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
                [ expression ]
                declarations

        Elm.Syntax.Expression.CaseExpression { expression, cases } ->
            expression
                :: List.map (\( _, caseExpression ) -> caseExpression) cases

        Elm.Syntax.Expression.LambdaExpression { expression } ->
            [ expression ]

        Elm.Syntax.Expression.TupledExpression expressions ->
            expressions

        Elm.Syntax.Expression.Negation expr ->
            [ expr ]

        Elm.Syntax.Expression.RecordAccess expr _ ->
            [ expr ]

        _ ->
            []


{-| The module header name + range
-}
moduleHeaderNameNode : Elm.Syntax.Node.Node Elm.Syntax.Module.Module -> Elm.Syntax.Node.Node Elm.Syntax.ModuleName.ModuleName
moduleHeaderNameNode node =
    case Elm.Syntax.Node.value node of
        Elm.Syntax.Module.NormalModule data ->
            data.moduleName

        Elm.Syntax.Module.PortModule data ->
            data.moduleName

        Elm.Syntax.Module.EffectModule data ->
            data.moduleName


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
            case String.trim line of
                "" ->
                    ""

                _ ->
                    line

        Nothing ->
            ""


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
