module Review.Test exposing
    ( ReviewResult, run
    , ExpectedError, error, atExactly, whenFixed, expectErrorsForModules, expectErrorsForElmJson, expectErrorsForReadme
    , expect, ReviewExpectation
    , moduleErrors, elmJsonErrors, readmeErrors
    , elmCore, elmHtml, elmParser, elmUrl
    )

{-| Module that helps you test your rules, using [`elm-test`](https://package.elm-lang.org/packages/elm-explorations/test/latest/).

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Review.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Review.You.Want.To.Test"
            [ test "should not report anything when <condition>" <|
                \() ->
                    """module A exposing (..)
    a = foo n"""
                        |> Review.Test.run rule
                        |> Review.Test.expectNoErrors
            , test "should report Debug.log use" <|
                \() ->
                    """module A exposing (..)
    a = Debug.log "some" "message" """
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                            ]
            ]


# Strategies for effective testing


## Use Test-Driven Development

Writing a rule is a process that works really well with the Test-Driven
Development process loop, which is:

  - Before writing any code, write a failing test.
  - Run the test and make sure that it is failing, otherwise you can't be
    sure that the test is well-written.
  - Write the simplest (almost stupid) code to make the test pass
  - Run the tests again and make sure that the test is passing, and that you
    didn't break any previous tests
  - Optionally, refactor your code but be sure not to change the behavior of the
    implementation. You should not add support for new patterns, as you will
    want to write tests for those first.

Then repeat for every pattern you wish to handle.


## Have a good title

A good test title explains

  - what is tested - Probably the rule, but making it explicit
    in a [`describe`](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test#describe)
    might improve your test report. Or maybe you are testing a sub-part of the rule,
    and you can name it explicitly.
  - what should happen: (not) reporting an error, fix <something> by <doing something>, ...
  - when: what is the situation that this test sets up?

Ideally, by only reading through the test titles, someone else should be able to
rewrite the rule you are testing.


## What should you test?

You should test the scenarios where you expect the rule to report something. At
the same time, you should also test when it shouldn't. I encourage writing tests
to make sure that things that are similar to what you want to report are not
reported.

For instance, if you wish to report uses of variables named `foo`, write a test
that ensures that the use of variables named differently does not get reported.

Tests are pretty cheap, and in the case of rules, it is probably better to have
too many tests rather than too few, since the behavior of a rule rarely changes
drastically.


# Design goals

If you are interested, you can read
[the design goals](https://github.com/jfmengels/elm-review/blob/master/documentation/design/test-module.md)
for this module.


# Running tests

@docs ReviewResult, run, runWithProjectData, runOnModules, runOnModulesWithProjectData


# Making assertions

@docs ExpectedError, expectNoErrors, expectErrors, error, atExactly, whenFixed, expectErrorsForModules, expectErrorsForElmJson, expectErrorsForReadme
@docs expectGlobalErrors
@docs expectConfigurationError
@docs expectDataExtract
@docs ignoredFilesImpactResults


## Composite assertions

@docs expect, ReviewExpectation
@docs moduleErrors, globalErrors, elmJsonErrors, readmeErrors, dataExtract


# Deprecated

@docs expectGlobalAndLocalErrors, expectGlobalAndModuleErrors

-}

import Array exposing (Array)
import Dict
import Elm.Docs
import Elm.Project
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node
import Elm.Syntax.Range exposing (Range)
import Expect exposing (Expectation)
import Json.Decode
import Json.Encode
import Review
import Review.Test.Dependencies.ElmCore
import Review.Test.Dependencies.ElmHtml
import Review.Test.Dependencies.ElmParser
import Review.Test.Dependencies.ElmUrl
import Review.Test.FailureMessage as FailureMessage
import Set exposing (Set)
import Unicode


{-| Dependency for `elm/core`. It contains operators.

It is present by default in `elm-review` tests when you use [`Review.Test.run`](./Review-Test#run) or
[`Review.Test.runOnModules`](./Review-Test#runOnModules), or when you create a new project starting with [`projectWithElmCore`](#projectWithElmCore).

Note that if you create a new project using [`Review.Project.new`](./Review-Project#new), you'll have to manually add it
again with [`Review.Project.addDependency`](./Review-Project#addDependency) if you so wish to.

-}
elmCore : { name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
elmCore =
    Review.Test.Dependencies.ElmCore.dependency


{-| Dependency for `elm/html`.
-}
elmHtml : { name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
elmHtml =
    Review.Test.Dependencies.ElmHtml.dependency


{-| Dependency for `elm/parser`. It contains operators.
-}
elmParser : { name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
elmParser =
    Review.Test.Dependencies.ElmParser.dependency


{-| Dependency for `elm/url`. It contains operators.
-}
elmUrl : { name : String, elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
elmUrl =
    Review.Test.Dependencies.ElmUrl.dependency



-- REVIEW RESULT


{-| The result of running a rule on a `String` containing source code.
-}
type ReviewResult
    = FailedRun String
    | SuccessfulRun SuccessfulRunData ReRun


type alias SuccessfulRunData =
    { ruleCanProvideFixes : RuleCanProvideFixes
    , runResults : List SuccessfulRunResult
    , allErrors : List Review.Error
    }


type alias Project =
    { modules : List { path : String, source : String }
    }


type ReRun
    = AttemptReRun Review.Review Project
    | DontAttemptReRun


type RuleCanProvideFixes
    = RuleCanProvideFixes Bool


type alias GlobalError =
    { message : String
    , details : List String
    }


type alias SuccessfulRunResult =
    { moduleName : String
    , inspector : CodeInspector
    , errors : List Review.Error
    }


type alias CodeInspector =
    { isModule : Bool
    , source : String
    , getCodeAtLocation : Range -> Maybe String
    , checkIfLocationIsAmbiguous : Review.Error -> String -> Expectation
    }


{-| An expectation for an error. Use [`error`](#error) to create one.
-}
type ExpectedError
    = ExpectedError ExpectedErrorDetails


type alias ExpectedErrorDetails =
    { message : String
    , details : List String
    , under : Under
    , fixedSource : Maybe String
    }


type Under
    = Under String
    | UnderExactly String Range


{-| Run a `Rule` on several modules. You can then use
[`expectNoErrors`](#expectNoErrors) or [`expectErrorsForModules`](#expectErrorsForModules) to assert
the errors reported by the rule.

This is basically the same as [`run`](#run), but you can pass several modules.
This is especially useful to test rules created with
[`Review.newProjectRuleSchema`](./Review-Rule#newProjectRuleSchema), that look at
several modules, and where the context of the project is important.

    import My.Rule exposing (rule)
    import Review.Test
    import Test exposing (Test, test)

    someTest : Test
    someTest =
        test "test title" <|
            \() ->
                let
                    project : Project
                    project =
                        Project.new
                            |> Project.addElmJson elmJsonToConstructManually
                in
                [ """
    module A exposing (a)
    a = 1""", """
    module B exposing (a)
    a = 1""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectNoErrors

The source codes need to be syntactically valid Elm code. If the code
can't be parsed, the test will fail regardless of the expectations you set on it.

Note that to be syntactically valid, you need at least a module declaration at the
top of each file (like `module A exposing (..)`) and one declaration (like `a = 1`).
You can't just have an expression like `1 + 2`.

Note: This is a more complex version of [`runOnModules`](#runOnModules). If your rule is not
interested in project related details, then you should use [`runOnModules`](#runOnModules) instead.

-}
run : Project -> Review.Review -> ReviewResult
run project review =
    let
        ( projectWithModules, modulesThatFailedToParse ) =
            List.foldl
                (\source ( accProject, accModulesThatFailedToParse ) ->
                    case FileParser.parse source of
                        Ok ast ->
                            ( Project.addParsedModule
                                { path = "src/" ++ String.join "/" (Module.moduleName (Node.value ast.moduleDefinition)) ++ ".elm"
                                , source = source
                                , ast = ast
                                }
                                accProject
                            , accModulesThatFailedToParse
                            )

                        Err _ ->
                            ( accProject
                            , { source = source, path = "test/DummyFilePath.elm" } :: accModulesThatFailedToParse
                            )
                )
                ( project, [] )
                sources
    in
    case modulesThatFailedToParse ++ Project.modulesThatFailedToParse projectWithModules of
        { source } :: _ ->
            let
                fileAndIndex : { source : String, index : Int }
                fileAndIndex =
                    { source = source
                    , index = indexOf source sources |> Maybe.withDefault -1
                    }
            in
            FailedRun <| FailureMessage.parsingFailure (hasExactlyOneElement sources) fileAndIndex

        [] ->
            let
                modules : List ProjectModule
                modules =
                    Project.modules projectWithModules
            in
            if List.isEmpty modules then
                FailedRun FailureMessage.missingSources

            else
                case findDuplicateModuleNames Set.empty modules of
                    Just moduleName ->
                        FailedRun <| FailureMessage.duplicateModuleName moduleName

                    Nothing ->
                        let
                            { errors } =
                                Review.run ReviewOptions.defaults [ review ] projectWithModules
                        in
                        case ListExtra.find (\err -> Review.errorTarget err == Error.Global) errors of
                            Just globalError_ ->
                                FailedRun <| FailureMessage.globalErrorInTest globalError_

                            Nothing ->
                                let
                                    fileErrors : List SuccessfulRunResult
                                    fileErrors =
                                        List.concat
                                            [ List.map (\module_ -> moduleToRunResult errors module_) modules
                                            , elmJsonRunResult errors projectWithModules
                                            , readmeRunResult errors projectWithModules
                                            ]
                                in
                                SuccessfulRun
                                    { ruleCanProvideFixes = RuleCanProvideFixes (Review.ruleProvidesFixes review)
                                    , runResults = fileErrors
                                    , allErrors = errors
                                    }
                                    (AttemptReRun review projectWithModules)


hasExactlyOneElement : List a -> Bool
hasExactlyOneElement =
    \list ->
        case list of
            [ _ ] ->
                True

            _ ->
                False


moduleToRunResult : List Review.Error -> ProjectModule -> SuccessfulRunResult
moduleToRunResult errors projectModule =
    { moduleName = String.join "." projectModule.moduleName
    , inspector = codeInspectorForSource True projectModule.source
    , errors =
        errors
            |> List.filter (\error_ -> Review.errorFilePath error_ == projectModule.path)
            |> List.sortWith compareErrorPositions
    }


elmJsonRunResult : List Review.Error -> Project -> List SuccessfulRunResult
elmJsonRunResult errors project =
    case project.elmJson of
        Just elmJsonData ->
            case List.filter (\error_ -> Review.errorFilePath error_ == elmJsonData.path) errors of
                [] ->
                    []

                errorsForElmJson ->
                    [ { moduleName = elmJsonData.path
                      , inspector = codeInspectorForSource False elmJsonData.raw
                      , errors = errorsForElmJson
                      }
                    ]

        Nothing ->
            []


readmeRunResult : List Review.Error -> Project -> List SuccessfulRunResult
readmeRunResult errors project =
    case project.readme of
        Just projectReadme ->
            case List.filter (\error_ -> Review.errorFilePath error_ == projectReadme.path) errors of
                [] ->
                    []

                errorsForReadme ->
                    [ { moduleName = projectReadme.path
                      , inspector = codeInspectorForSource False projectReadme.content
                      , errors = errorsForReadme
                      }
                    ]

        Nothing ->
            []


indexOf : a -> List a -> Maybe Int
indexOf elementToFind aList =
    indexOfHelp elementToFind aList 0


indexOfHelp : a -> List a -> Int -> Maybe Int
indexOfHelp elementToFind aList offset =
    case aList of
        [] ->
            Nothing

        a :: rest ->
            if a == elementToFind then
                Just offset

            else
                indexOfHelp elementToFind rest (offset + 1)


codeInspectorForSource : Bool -> String -> CodeInspector
codeInspectorForSource isModule source =
    { isModule = isModule
    , source = source
    , getCodeAtLocation = getCodeAtLocationInSourceCode source
    , checkIfLocationIsAmbiguous = checkIfLocationIsAmbiguousInSourceCode source
    }


compareErrorPositions : Review.Error -> Review.Error -> Order
compareErrorPositions a b =
    compareRange a.range b.range


compareRange : Range -> Range -> Order
compareRange a b =
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


{-| Assert that the rule reported some errors, by specifying which ones.

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Review.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Review.You.Want.To.Test"
            [ test "should report Debug.log use" <|
                \() ->
                    """module A exposing (..)
    a = Debug.log "some" "message"
    """
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                            ]
            ]

-}
expectErrors : List ExpectedError -> ReviewResult -> Expectation
expectErrors expectedErrors reviewResult =
    expectLocalErrors
        { global = []
        , local = expectedErrors
        }
        reviewResult


{-| Assert that the rule reported some errors, by specifying which ones and the
module for which they were reported.

This is the same as [`expectErrors`](#expectErrors), but for when you used
[`runOnModules`](#runOnModules) or [`runOnModulesWithProjectData`](#runOnModulesWithProjectData).
to create the test. When using those, the errors you expect need to be associated
with a module. If we don't specify this, your tests might pass because you
expected the right errors, but they may be reported for the wrong module!

If you expect the rule to report other kinds of errors or extract data, then you should use the [`Review.Test.expect`](#expect) and [`moduleErrors`](#moduleErrors) functions.

The expected errors are tupled: the first element is the module name
(for example: `List` or `My.Module.Name`) and the second element is the list of
errors you expect to be reported.

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

    import Review.Test
    import Test exposing (Test, test)
    import The.Review.You.Want.To.Test exposing (rule)

    someTest : Test
    someTest =
        test "should report an error when a module uses `Debug.log`" <|
            \() ->
                [ """
    module ModuleA exposing (a)
    a = 1""", """
    module ModuleB exposing (a)
    a = Debug.log "log" 1""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "ModuleB"
                          , [ Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                            ]
                          )
                        ]

-}
expectErrorsForModules : List ( String, List ExpectedError ) -> ReviewResult -> Expectation
expectErrorsForModules expectedErrorsList reviewResult =
    expectGlobalAndModuleErrors
        { global = []
        , modules = expectedErrorsList
        }
        reviewResult


{-| **@deprecated** Use [`Review.Test.expect`](#expect) instead.

Assert that the rule reported some [global errors](./Review-Rule#globalError) and [local](./Review-Test#ExpectedError) errors, by specifying which ones.

Use this function when you expect both local and global errors for a particular test, and when you are using [`run`](#run) or [`runWithProjectData`](#runWithProjectData).
When using [`runOnModules`](#runOnModules) or [`runOnModulesWithProjectData`](#runOnModulesWithProjectData), use [`expectGlobalAndModuleErrors`](#expectGlobalAndModuleErrors) instead.

If you only have local or global errors, you should instead use [`expectErrors`](#expectErrors) or [`expectGlobalErrors`](#expectGlobalErrors) respectively.

This function works in the same way as [`expectErrors`](#expectErrors) and [`expectGlobalErrors`](#expectGlobalErrors).

-}
expectLocalErrors : List ExpectedError -> ReviewResult -> Expectation
expectLocalErrors local reviewResult =
    case reviewResult of
        FailedRun errorMessage ->
            Expect.fail errorMessage

        SuccessfulRun { ruleCanProvideFixes, runResults, allErrors } reRun ->
            Expect.all
                [ \() ->
                    if List.isEmpty local then
                        expectNoModuleErrors runResults

                    else
                        case runResults of
                            runResult :: [] ->
                                checkAllErrorsMatch ruleCanProvideFixes runResult local

                            _ ->
                                Expect.fail FailureMessage.needToUsedExpectErrorsForModules
                , \() -> checkResultsAreTheSameWhenIgnoringFiles allErrors reRun
                ]
                ()


{-| **@deprecated** Use [`Review.Test.expect`](#expect) instead.

Assert that the rule reported some errors for modules and global errors, by specifying which ones.

Use this function when you expect both local and global errors for a particular test, and when you are using [`runOnModules`](#runOnModules) or [`runOnModulesWithProjectData`](#runOnModulesWithProjectData).
When using[`run`](#run) or [`runWithProjectData`](#runWithProjectData), use [`expectGlobalAndLocalErrors`](#expectGlobalAndLocalErrors) instead.

If you only have local or global errors, you should instead use [`expectErrorsForModules`](#expectErrorsForModules) or [`expectGlobalErrors`](#expectGlobalErrors) respectively.

This function works in the same way as [`expectErrorsForModules`](#expectErrorsForModules) and [`expectGlobalErrors`](#expectGlobalErrors).

-}
expectGlobalAndModuleErrors : { global : List { message : String, details : List String }, modules : List ( String, List ExpectedError ) } -> ReviewResult -> Expectation
expectGlobalAndModuleErrors { global, modules } reviewResult =
    case reviewResult of
        FailedRun errorMessage ->
            Expect.fail errorMessage

        SuccessfulRun { ruleCanProvideFixes, runResults, allErrors } reRun ->
            Expect.all
                [ \() -> expectErrorsForModulesHelp ruleCanProvideFixes modules runResults
                , \() -> checkResultsAreTheSameWhenIgnoringFiles allErrors reRun
                ]
                ()


checkResultsAreTheSameWhenIgnoringFiles : List Review.Error -> ReRun -> Expectation
checkResultsAreTheSameWhenIgnoringFiles allErrors reRun =
    case reRun of
        AttemptReRun rule project ->
            doCheckResultsAreTheSameWhenIgnoringFiles allErrors rule project

        DontAttemptReRun ->
            Expect.pass


doCheckResultsAreTheSameWhenIgnoringFiles : List Review.Error -> Review.Review -> Project -> Expectation
doCheckResultsAreTheSameWhenIgnoringFiles allErrors rule project =
    let
        filePaths : List String
        filePaths =
            Project.modules project
                |> List.map .path
                |> maybeCons .path (Project.elmJson project)
                |> maybeCons .path (Project.readme project)

        combinationsOfFilesToIgnore : List (List String)
        combinationsOfFilesToIgnore =
            allCombinations filePaths
    in
    if List.isEmpty combinationsOfFilesToIgnore then
        Expect.pass

    else
        Expect.all
            (List.map (\filesToIgnore () -> checkResultsAreTheSameWhenIgnoringFilesHelp rule project allErrors filesToIgnore) combinationsOfFilesToIgnore)
            ()


checkResultsAreTheSameWhenIgnoringFilesHelp : Review.Review -> Project -> List Review.Error -> List String -> Expectation
checkResultsAreTheSameWhenIgnoringFilesHelp rule project allErrors filesToIgnore =
    let
        ( missing, unexpected ) =
            Review.review
                (ReviewOptions.withDataExtraction False ReviewOptions.defaults)
                [ Review.ignoreErrorsForFilesWhere filesToIgnore rule ]
                project
                |> .errors
                |> removeInCommon (removeErrorsForIgnoredFiles filesToIgnore allErrors) []
    in
    if List.isEmpty missing && List.isEmpty unexpected then
        Expect.pass

    else
        Expect.fail (FailureMessage.resultsAreDifferentWhenFilesAreIgnored { ignoredFiles = filesToIgnore, missing = missing, unexpected = unexpected })


removeInCommon : List a -> List a -> List a -> ( List a, List a )
removeInCommon expected excessFromActual actual =
    case actual of
        [] ->
            ( expected, excessFromActual )

        first :: restOfActual ->
            case removeFirst first expected [] of
                Just newExpected ->
                    removeInCommon newExpected excessFromActual restOfActual

                Nothing ->
                    removeInCommon expected (first :: excessFromActual) restOfActual


removeFirst : a -> List a -> List a -> Maybe (List a)
removeFirst a list prev =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if x == a then
                Just (prev ++ xs)

            else
                removeFirst a xs (x :: prev)


removeErrorsForIgnoredFiles : List String -> List Review.Error -> List Review.Error
removeErrorsForIgnoredFiles filesToIgnore errors =
    List.filter
        (\err -> not (List.member (Review.errorFilePath err) filesToIgnore))
        errors


allCombinations : List a -> List (List a)
allCombinations list =
    case list of
        [] ->
            []

        [ _ ] ->
            []

        _ ->
            allCombinationsHelp list


allCombinationsHelp : List a -> List (List a)
allCombinationsHelp list =
    case list of
        [] ->
            []

        first :: rest ->
            let
                addFirst : List a -> List (List a) -> List (List a)
                addFirst subList acc =
                    subList :: (first :: subList) :: acc
            in
            [ first ] :: List.foldr addFirst [] (allCombinationsHelp rest)


maybeCons : (b -> a) -> Maybe b -> List a -> List a
maybeCons mapper maybe list =
    case maybe of
        Just b ->
            mapper b :: list

        Nothing ->
            list


expectErrorsForModulesHelp : RuleCanProvideFixes -> List ( String, List ExpectedError ) -> List SuccessfulRunResult -> Expectation
expectErrorsForModulesHelp ruleCanProvideFixes expectedErrorsList runResults =
    let
        maybeUnknownModule : Maybe String
        maybeUnknownModule =
            Set.diff
                (expectedErrorsList |> List.map Tuple.first |> Set.fromList)
                (Set.fromList (List.map .moduleName runResults))
                |> Set.toList
                |> List.head
    in
    case maybeUnknownModule of
        Just unknownModule ->
            FailureMessage.unknownModulesInExpectedErrors unknownModule
                |> Expect.fail

        Nothing ->
            Expect.all
                (expectErrorsForModuleFiles ruleCanProvideFixes expectedErrorsList runResults)
                ()


expectErrorsForModuleFiles : RuleCanProvideFixes -> List ( String, List ExpectedError ) -> List SuccessfulRunResult -> List (() -> Expectation)
expectErrorsForModuleFiles ruleCanProvideFixes expectedErrorsList runResults =
    List.map
        (\runResult () ->
            let
                expectedErrors : List ExpectedError
                expectedErrors =
                    expectedErrorsList
                        |> ListExtra.find (\( moduleName, _ ) -> moduleName == runResult.moduleName)
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault []
            in
            if List.isEmpty expectedErrors then
                expectNoErrorForModuleRunResult runResult

            else
                checkAllErrorsMatch ruleCanProvideFixes runResult expectedErrors
        )
        runResults


{-| Assert that the rule reported some errors for the `elm.json` file, by specifying which ones.

If you expect the rule to report other kinds of errors or extract data, then you should use the [`Review.Test.expect`](#expect) and [`elmJsonErrors`](#elmJsonErrors) functions.

    test "report an error when a module is unused" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson elmJsonToConstructManually
            in
            """
    module ModuleA exposing (a)
    a = 1"""
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expectErrorsForElmJson
                    [ Review.Test.error
                        { message = "Unused dependency `author/package`"
                        , details = [ "Dependency should be removed" ]
                        , under = "author/package"
                        }
                    ]

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

-}
expectErrorsForElmJson : List ExpectedError -> ReviewResult -> Expectation
expectErrorsForElmJson expectedErrors reviewResult =
    expectErrorsForModules [ ( "elm.json", expectedErrors ) ] reviewResult


{-| Assert that the rule reported some errors for the `README.md` file, by specifying which ones.

If you expect the rule to report other kinds of errors or extract data, then you should use the [`Review.Test.expect`](#expect) and [`readmeErrors`](#readmeErrors) functions.

    test "report an error when a module is unused" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addReadme { path = "README.md", context = "# Project\n..." }
            in
            """
    module ModuleA exposing (a)
    a = 1"""
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expectErrorsForReadme
                    [ Review.Test.error
                        { message = "Invalid link"
                        , details = [ "README contains an invalid link" ]
                        , under = "htt://example.com"
                        }
                    ]

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

-}
expectErrorsForReadme : List ExpectedError -> ReviewResult -> Expectation
expectErrorsForReadme expectedErrors reviewResult =
    expectErrorsForModules [ ( "README.md", expectedErrors ) ] reviewResult


{-| Create an expectation for an error.

`message` should be the message you're expecting to be shown to the user.

`under` is the part of the code where you are expecting the error to be shown to
the user. If it helps, imagine `under` to be the text under which the squiggly
lines will appear if the error appeared in an editor.

    tests : Test
    tests =
        describe "The.Review.You.Want.To.Test"
            [ test "should report Debug.log use" <|
                \() ->
                    """module A exposing (..)
    a = Debug.log "some" "message\""""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                            ]
            ]

If there are multiple locations where the value of `under` appears, the test will
fail unless you use [`atExactly`](#atExactly) to remove any ambiguity of where the
error should be used.

-}
error : { message : String, details : List String, under : String } -> ExpectedError
error input =
    ExpectedError
        { message = input.message
        , details = input.details
        , under = Under input.under
        , fixedSource = Nothing
        }


{-| Precise the exact position where the error should be shown to the user. This
is only necessary when the `under` field is ambiguous.

`atExactly` takes a record with start and end positions.

    tests : Test
    tests =
        describe "The.Review.You.Want.To.Test"
            [ test "should report multiple Debug.log calls" <|
                \() ->
                    """module A exposing (..)
    a = Debug.log "foo" z
    b = Debug.log "foo" z
    """
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                                |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 14 } }
                            , Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                                |> Review.Test.atExactly { start = { row = 5, column = 5 }, end = { row = 5, column = 14 } }
                            ]
            ]

Tip: By default, do not use this function. If the test fails because there is some
ambiguity, the test error will give you a recommendation of what to use as a parameter
of `atExactly`, so you do not have to bother writing this hard-to-write argument yourself.

-}
atExactly : { start : { row : Int, column : Int }, end : { row : Int, column : Int } } -> ExpectedError -> ExpectedError
atExactly range ((ExpectedError expectedError_) as expectedError) =
    ExpectedError { expectedError_ | under = UnderExactly (getUnder expectedError) range }


{-| Create an expectation that the error provides an automatic fix, meaning that it used
functions like [`errorWithFix`](./Review-Rule#errorWithFix), and an expectation of what the source
code should be after the error's fix have been applied.

In the absence of `whenFixed`, the test will fail if the error provides a fix.
In other words, you only need to use this function if the error provides a fix.

    tests : Test
    tests =
        describe "The.Review.You.Want.To.Test"
            [ test "should report multiple Debug.log calls" <|
                \() ->
                    """module A exposing (..)
    a = 1
    b = Debug.log "foo" 2
    """
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "Remove the use of `Debug` before shipping to production"
                                , details = [ "Details about the error" ]
                                , under = "Debug.log"
                                }
                                |> Review.Test.whenFixed """module SomeModule exposing (b)
    a = 1
    b = 2
    """
                            ]
            ]

-}
whenFixed : String -> ExpectedError -> ExpectedError
whenFixed fixedSource (ExpectedError expectedError) =
    ExpectedError { expectedError | fixedSource = Just fixedSource }


getUnder : ExpectedError -> String
getUnder (ExpectedError expectedError) =
    case expectedError.under of
        Under str ->
            str

        UnderExactly str _ ->
            str


getCodeAtLocationInSourceCode : String -> Range -> Maybe String
getCodeAtLocationInSourceCode sourceCode =
    let
        lines : Array String
        lines =
            String.lines sourceCode
                |> Array.fromList
    in
    \{ start, end } ->
        if start.row == end.row then
            Array.get (start.row - 1) lines
                |> Maybe.map (Unicode.slice (start.column - 1) (end.column - 1))

        else
            let
                firstLine : String
                firstLine =
                    case Array.get (start.row - 1) lines of
                        Just str ->
                            Unicode.dropLeft (start.column - 1) str

                        Nothing ->
                            ""

                lastLine : String
                lastLine =
                    case Array.get (end.row - 1) lines of
                        Just str ->
                            Unicode.left end.column str

                        Nothing ->
                            ""

                resultingLines : List String
                resultingLines =
                    if start.row + 1 == end.row then
                        [ firstLine
                        , lastLine
                        ]

                    else
                        [ firstLine
                        , Array.slice start.row (end.row - 1) lines
                            |> Array.toList
                            |> String.join "\n"
                        , lastLine
                        ]
            in
            resultingLines
                |> String.join "\n"
                |> Just


checkIfLocationIsAmbiguousInSourceCode : String -> Review.Error -> String -> Expectation
checkIfLocationIsAmbiguousInSourceCode sourceCode =
    \error_ under ->
        let
            occurrencesInSourceCode : List Int
            occurrencesInSourceCode =
                String.indexes under sourceCode
        in
        hasExactlyOneElement occurrencesInSourceCode
            |> Expect.equal True
            |> Expect.onFail (FailureMessage.locationIsAmbiguousInSourceCode sourceCode error_ under occurrencesInSourceCode)



-- RUNNING THE CHECKS


type alias ReorderState =
    { expectedErrors : List ExpectedError
    , reviewErrors : List Review.Error
    , pairs : List ( ExpectedError, Review.Error )
    , expectedErrorsWithNoMatch : List ExpectedError
    }


reorderErrors : CodeInspector -> ReorderState -> ( List ExpectedError, List Review.Error )
reorderErrors codeInspector reorderState =
    case reorderState.expectedErrors of
        [] ->
            ( List.reverse <| reorderState.expectedErrorsWithNoMatch ++ List.map Tuple.first reorderState.pairs
            , List.reverse <| reorderState.reviewErrors ++ List.map Tuple.second reorderState.pairs
            )

        ((ExpectedError expectedErrorDetails) as expectedError) :: restOfExpectedErrors ->
            case findBestMatchingReviewError codeInspector expectedErrorDetails reorderState.reviewErrors { error = Nothing, confidenceLevel = 0 } of
                Just reviewError ->
                    reorderErrors codeInspector
                        { reorderState
                            | pairs = ( expectedError, reviewError ) :: reorderState.pairs
                            , reviewErrors = removeFirstOccurrence reviewError reorderState.reviewErrors
                            , expectedErrors = restOfExpectedErrors
                        }

                Nothing ->
                    reorderErrors codeInspector
                        { reorderState
                            | expectedErrorsWithNoMatch = expectedError :: reorderState.expectedErrorsWithNoMatch
                            , expectedErrors = restOfExpectedErrors
                        }


removeFirstOccurrence : a -> List a -> List a
removeFirstOccurrence elementToRemove list =
    case list of
        [] ->
            []

        x :: xs ->
            if x == elementToRemove then
                xs

            else
                x :: removeFirstOccurrence elementToRemove xs


findBestMatchingReviewError : CodeInspector -> ExpectedErrorDetails -> List Review.Error -> { error : Maybe Review.Error, confidenceLevel : Int } -> Maybe Review.Error
findBestMatchingReviewError codeInspector expectedErrorDetails reviewErrors bestMatch =
    case reviewErrors of
        [] ->
            bestMatch.error

        reviewError :: restOfReviewErrors ->
            let
                confidenceLevel : Int
                confidenceLevel =
                    matchingConfidenceLevel codeInspector expectedErrorDetails reviewError
            in
            if confidenceLevel > bestMatch.confidenceLevel then
                findBestMatchingReviewError
                    codeInspector
                    expectedErrorDetails
                    restOfReviewErrors
                    { error = Just reviewError, confidenceLevel = confidenceLevel }

            else
                findBestMatchingReviewError
                    codeInspector
                    expectedErrorDetails
                    restOfReviewErrors
                    bestMatch


matchingConfidenceLevel : CodeInspector -> ExpectedErrorDetails -> Review.Error -> Int
matchingConfidenceLevel codeInspector expectedErrorDetails reviewError =
    if expectedErrorDetails.message /= .message reviewError then
        0

    else
        case expectedErrorDetails.under of
            Under under ->
                if codeInspector.getCodeAtLocation (.range reviewError) /= Just under then
                    1

                else
                    2

            UnderExactly under range ->
                if codeInspector.getCodeAtLocation (.range reviewError) /= Just under then
                    1

                else if range /= .range reviewError then
                    2

                else
                    3


checkAllErrorsMatch : RuleCanProvideFixes -> SuccessfulRunResult -> List ExpectedError -> Expectation
checkAllErrorsMatch ruleCanProvideFixes runResult unorderedExpectedErrors =
    let
        ( expectedErrors, reviewErrors ) =
            reorderErrors
                runResult.inspector
                { expectedErrors = unorderedExpectedErrors
                , reviewErrors = runResult.errors
                , pairs = []
                , expectedErrorsWithNoMatch = []
                }
    in
    Expect.all
        (List.reverse (checkErrorsMatch ruleCanProvideFixes runResult expectedErrors (List.length expectedErrors) reviewErrors))
        ()


checkGlobalErrorsMatch : Int -> { expected : List GlobalError, actual : List GlobalError, needSecondPass : List GlobalError } -> Expectation
checkGlobalErrorsMatch originalNumberOfExpectedErrors params =
    case params.expected of
        head :: rest ->
            case findAndRemove head params.actual of
                Just newActual ->
                    if List.isEmpty head.details then
                        Expect.fail (FailureMessage.emptyDetails head.message)

                    else
                        checkGlobalErrorsMatch originalNumberOfExpectedErrors { expected = rest, actual = newActual, needSecondPass = params.needSecondPass }

                Nothing ->
                    checkGlobalErrorsMatch originalNumberOfExpectedErrors { expected = rest, actual = params.actual, needSecondPass = head :: params.needSecondPass }

        [] ->
            case params.actual of
                firstActual :: restOfActual ->
                    case List.head (List.reverse params.needSecondPass) of
                        Just notFoundError ->
                            failBecauseExpectedErrorCouldNotBeFound notFoundError ( firstActual, restOfActual )

                        Nothing ->
                            Expect.fail <| FailureMessage.tooManyGlobalErrors params.actual

                [] ->
                    if List.isEmpty params.needSecondPass then
                        -- All expected errors were found
                        Expect.pass

                    else
                        -- We expected more errors
                        Expect.fail <| FailureMessage.expectedMoreGlobalErrors originalNumberOfExpectedErrors params.needSecondPass


findAndRemove : a -> List a -> Maybe (List a)
findAndRemove element list =
    findAndRemoveHelp element [] list


findAndRemoveHelp : a -> List a -> List a -> Maybe (List a)
findAndRemoveHelp element previous list =
    case list of
        [] ->
            Nothing

        head :: rest ->
            if element == head then
                Just (List.reverse previous ++ rest)

            else
                findAndRemoveHelp element (head :: previous) rest


failBecauseExpectedErrorCouldNotBeFound : GlobalError -> ( GlobalError, List GlobalError ) -> Expectation
failBecauseExpectedErrorCouldNotBeFound expectedError ( firstActual, restOfActual ) =
    case ListExtra.find (\e -> e.message == expectedError.message) (firstActual :: restOfActual) of
        Just actualErrorWithTheSameMessage ->
            Expect.fail <| FailureMessage.unexpectedGlobalErrorDetails expectedError.details actualErrorWithTheSameMessage

        Nothing ->
            Expect.fail <| FailureMessage.messageMismatchForGlobalError expectedError firstActual


checkAllGlobalErrorsMatch : Int -> { expected : List GlobalError, actual : List GlobalError } -> Expectation
checkAllGlobalErrorsMatch expectedErrorToString params =
    checkGlobalErrorsMatch expectedErrorToString { expected = params.expected, actual = params.actual, needSecondPass = [] }


checkErrorsMatch : RuleCanProvideFixes -> SuccessfulRunResult -> List ExpectedError -> Int -> List Review.Error -> List (() -> Expectation)
checkErrorsMatch ruleCanProvideFixes runResult expectedErrors expectedNumberOfErrors errors =
    case ( expectedErrors, errors ) of
        ( [], [] ) ->
            [ always Expect.pass ]

        ( expected :: restOfExpectedErrors, error_ :: restOfErrors ) ->
            checkErrorMatch ruleCanProvideFixes runResult.inspector expected error_
                :: checkErrorsMatch ruleCanProvideFixes runResult restOfExpectedErrors expectedNumberOfErrors restOfErrors

        ( expected :: restOfExpectedErrors, [] ) ->
            [ \() ->
                (expected :: restOfExpectedErrors)
                    |> List.map extractExpectedErrorData
                    |> FailureMessage.expectedMoreErrors runResult.moduleName expectedNumberOfErrors
                    |> Expect.fail
            ]

        ( [], error_ :: restOfErrors ) ->
            [ \() ->
                FailureMessage.tooManyErrors runResult.moduleName (error_ :: restOfErrors)
                    |> Expect.fail
            ]


extractExpectedErrorData : ExpectedError -> FailureMessage.ExpectedErrorData
extractExpectedErrorData ((ExpectedError expectedErrorContent) as expectedError) =
    { message = expectedErrorContent.message
    , details = expectedErrorContent.details
    , under = getUnder expectedError
    }


checkErrorMatch : RuleCanProvideFixes -> CodeInspector -> ExpectedError -> Review.Error -> (() -> Expectation)
checkErrorMatch ruleCanProvideFixes codeInspector ((ExpectedError expectedError_) as expectedError) error_ =
    Expect.all
        [ \() ->
            .message error_
                |> Expect.equal expectedError_.message
                |> Expect.onFail
                    (FailureMessage.messageMismatch
                        (extractExpectedErrorData expectedError)
                        error_
                    )
        , checkMessageAppearsUnder codeInspector error_ expectedError
        , checkDetailsAreCorrect error_ expectedError
        , \() -> checkFixesAreCorrect ruleCanProvideFixes codeInspector error_ expectedError
        ]


checkMessageAppearsUnder : CodeInspector -> Review.Error -> ExpectedError -> (() -> Expectation)
checkMessageAppearsUnder codeInspector error_ (ExpectedError expectedError) =
    if .target error_ == Error.UserGlobal then
        \() -> Expect.pass

    else
        case codeInspector.getCodeAtLocation (.errorRange error_) of
            Just codeAtLocation ->
                case expectedError.under of
                    Under under ->
                        Expect.all
                            [ \() ->
                                case under of
                                    "" ->
                                        FailureMessage.underMayNotBeEmpty
                                            { message = expectedError.message
                                            , codeAtLocation = codeAtLocation
                                            }
                                            |> Expect.fail

                                    _ ->
                                        Expect.pass
                            , \() ->
                                codeAtLocation
                                    |> Expect.equal under
                                    |> Expect.onFail (FailureMessage.underMismatch error_ { under = under, codeAtLocation = codeAtLocation })
                            , \() ->
                                codeInspector.checkIfLocationIsAmbiguous error_ under
                            ]

                    UnderExactly under range ->
                        Expect.all
                            [ \() ->
                                codeAtLocation
                                    |> Expect.equal under
                                    |> Expect.onFail (FailureMessage.underMismatch error_ { under = under, codeAtLocation = codeAtLocation })
                            , \() ->
                                .errorRange error_
                                    |> Expect.equal range
                                    |> Expect.onFail (FailureMessage.wrongLocation error_ range under)
                            ]

            Nothing ->
                \() ->
                    FailureMessage.locationNotFound error_
                        |> Expect.fail


checkDetailsAreCorrect : Review.Error -> ExpectedError -> (() -> Expectation)
checkDetailsAreCorrect error_ (ExpectedError expectedError) =
    Expect.all
        [ \() ->
            (List.isEmpty <| .details error_)
                |> Expect.equal False
                |> Expect.onFail (FailureMessage.emptyDetails (.message error_))
        , \() ->
            .details error_
                |> Expect.equal expectedError.details
                |> Expect.onFail (FailureMessage.unexpectedDetails expectedError.details error_)
        ]


checkFixesAreCorrect : RuleCanProvideFixes -> CodeInspector -> Review.Error -> ExpectedError -> Expectation
checkFixesAreCorrect (RuleCanProvideFixes ruleCanProvideFixes) codeInspector ((Error.Review.Error err) as error_) ((ExpectedError expectedError_) as expectedError) =
    case ( expectedError_.fixedSource, err.fixes ) of
        ( Nothing, Error.NoFixes ) ->
            Expect.pass

        ( Just _, Error.NoFixes ) ->
            FailureMessage.missingFixes (extractExpectedErrorData expectedError)
                |> Expect.fail

        ( Nothing, Error.Available _ ) ->
            FailureMessage.unexpectedFixes error_
                |> Expect.fail

        ( Just expectedFixedSource, Error.Available fixes ) ->
            case Fix.fix err.target fixes codeInspector.source of
                Fix.Successful fixedSource ->
                    if fixedSource == expectedFixedSource then
                        if ruleCanProvideFixes then
                            Expect.pass

                        else
                            Expect.fail FailureMessage.ruleNotMarkedAsFixableError

                    else if removeWhitespace fixedSource == removeWhitespace expectedFixedSource then
                        Expect.fail <| FailureMessage.fixedCodeWhitespaceMismatch fixedSource expectedFixedSource error_

                    else
                        Expect.fail <| FailureMessage.fixedCodeMismatch fixedSource expectedFixedSource error_

                Fix.Errored Fix.Unchanged ->
                    Expect.fail <| FailureMessage.unchangedSourceAfterFix error_

                Fix.Errored (Fix.SourceCodeIsNotValid sourceCode) ->
                    Expect.fail <| FailureMessage.invalidSourceAfterFix error_ sourceCode

                Fix.Errored Fix.HasCollisionsInFixRanges ->
                    Expect.fail <| FailureMessage.hasCollisionsInFixRanges error_

        ( _, Error.FailedToApply _ problem ) ->
            case problem of
                FixProblem.Unchanged ->
                    Expect.fail <| FailureMessage.unchangedSourceAfterFix error_

                FixProblem.SourceCodeIsNotValid sourceCode ->
                    Expect.fail <| FailureMessage.invalidSourceAfterFix error_ sourceCode

                FixProblem.HasCollisionsInFixRanges ->
                    Expect.fail <| FailureMessage.hasCollisionsInFixRanges error_


removeWhitespace : String -> String
removeWhitespace =
    \string -> string |> String.replace " " "" |> String.replace "\n" ""


{-| Expectation of something that the rule will report or do.

Check out the functions below to create these, and then pass them to [`Review.Test.expect`](#expect).

-}
type ReviewExpectation
    = FileErrorExpectation { path : String, errors : List ExpectedError }


type CompiledDataExtract
    = NoDataExtractExpected
    | DataExtractExpected String
    | MultipleDataExtractExpected


{-| Expect multiple outputs for tests.

Functions such as [`expectErrors`](#expectErrors) and [`expectGlobalErrors`](#expectGlobalErrors) work well, but
in some situations a rule will report multiple things: module errors, global errors, errors for `elm.json` or the
README, or even extract data.

When you have multiple expectations to make for a module, use this function.

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Review.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Review.You.Want.To.Test"
            [ test "should ..." <|
                \() ->
                    [ """module A.B exposing (..)
    import B
    a = 1
    b = 2
    c = 3
    """
                    , """module B exposing (..)
    x = 1
    y = 2
    z = 3
    """
                    ]
                        |> Review.Test.runOnModules rule
                        |> Review.Test.expect
                            [ Review.Test.globalErrors [ { message = "message", details = [ "details" ] } ]
                            , Review.Test.moduleErrors "A.B" [ { message = "message", details = [ "details" ] } ]
                            , Review.Test.dataExtract """
    {
        "foo": "bar",
        "other": [ 1, 2, 3 ]
    }"""
                            ]
            ]

-}
expect : List ReviewExpectation -> ReviewResult -> Expectation
expect expectations reviewResult =
    case reviewResult of
        FailedRun errorMessage ->
            Expect.fail errorMessage

        SuccessfulRun { ruleCanProvideFixes, runResults, allErrors } reRun ->
            Expect.all
                [ \() ->
                    expectErrorsForModulesHelp ruleCanProvideFixes
                        (List.map
                            (\expectation ->
                                case expectation of
                                    FileErrorExpectation moduleName errors ->
                                        ( moduleName, errors )
                            )
                            expectations
                        )
                        runResults
                , \() -> checkResultsAreTheSameWhenIgnoringFiles allErrors reRun
                ]
                ()


{-| Assert that the rule reported some [global errors](./Review-Rule#globalError), by specifying which ones. To be used along with [`Review.Test.expect`](#expect).

If you expect only global errors, then you may want to use [`expectGlobalErrors`](#expectGlobalErrors) which is simpler.

Assert which errors are reported using records with the expected message and details. The test will fail if
a different number of errors than expected are reported, or if the message or details is incorrect.

    import Review.Test
    import Test exposing (Test, test)
    import The.Review.You.Want.To.Test exposing (rule)

    someTest : Test
    someTest =
        test "should report a global error when the specified module could not be found" <|
            \() ->
                """
    module ModuleA exposing (a)
    a = 1"""
                    |> Review.Test.run (rule "ModuleB")
                    |> Review.Test.expect
                        [ Review.Test.globalErrors
                            [ { message = "Could not find module ModuleB"
                              , details =
                                    [ "You mentioned the module ModuleB in the configuration of this rule, but it could not be found."
                                    , "This likely means you misconfigured the rule or the configuration has become out of date with recent changes in your project."
                                    ]
                              }
                            ]
                        ]

-}
globalErrors : List { message : String, details : List String } -> ReviewExpectation
globalErrors expected =
    GlobalErrorExpectation expected


{-| Assert that the rule reported some errors for modules, by specifying which ones. To be used along with [`Review.Test.expect`](#expect).

If you expect only module errors, then you may want to use [`expectErrorsForModules`](#expectErrorsForModules) which is simpler.

    test "report an error when a module is unused" <|
        \() ->
            [ """
    module ModuleA exposing (a)
    a = 1""", """
    module ModuleB exposing (a)
    a = Debug.log "log" 1""" ]
                |> Review.Test.runOnModules rule
                |> Review.Test.expect
                    [ Review.Test.moduleErrors "ModuleB"
                        [ Review.Test.error
                            { message = "Remove the use of `Debug` before shipping to production"
                            , details = [ "Details about the error" ]
                            , under = "Debug.log"
                            }
                        ]
                    ]

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

-}
moduleErrors : String -> List ExpectedError -> ReviewExpectation
moduleErrors moduleName expected =
    FileErrorExpectation moduleName expected


{-| Assert that the rule reported some errors for the `elm.json` file, by specifying which ones. To be used along with [`Review.Test.expect`](#expect).

If you expect only errors for `elm.json`, then you may want to use [`expectErrorsForElmJson`](#expectErrorsForElmJson) which is simpler.

    test "report an error when a module is unused" <|
        \() ->
            let
                project : Project
                project =
                    Project.new
                        |> Project.addElmJson elmJsonToConstructManually
            in
            """
    module ModuleA exposing (a)
    a = 1"""
                |> Review.Test.runWithProjectData project rule
                |> Review.Test.expect
                    [ Review.Test.elmJson
                        [ Review.Test.error
                            { message = "Unused dependency `author/package`"
                            , details = [ "Dependency should be removed" ]
                            , under = "author/package"
                            }
                        ]
                    ]

Assert which errors are reported using [`error`](#error). The test will fail if
a different number of errors than expected are reported, or if the message or the
location is incorrect.

-}
elmJsonErrors : List ExpectedError -> ReviewExpectation
elmJsonErrors expected =
    FileErrorExpectation "elm.json" expected


{-| Assert that the rule reported some errors for the `README.md` file. To be used along with [`Review.Test.expect`](#expect).

If you expect only errors for `README.md`, then you may want to use [`expectErrorsForReadme`](#expectErrorsForReadme) which is simpler.

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Review.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Review.You.Want.To.Test"
            [ test "should extract even if there are errors" <|
                \() ->
                    let
                        project : Project
                        project =
                            Project.new
                                |> Project.addReadme { path = "README.md", context = "# Project\n..." }
                    in
                    """module ModuleA exposing (a)
    a = 1"""
                        |> Review.Test.runWithProjectData project rule
                        |> Review.Test.expect
                            [ Review.Test.readme
                                [ Review.Test.error
                                    { message = "Invalid link"
                                    , details = [ "README contains an invalid link" ]
                                    , under = "htt://example.com"
                                    }
                                ]
                            ]
            ]

-}
readmeErrors : List ExpectedError -> ReviewExpectation
readmeErrors expected =
    FileErrorExpectation "README.md" expected


{-| Expect the rule to produce a specific data extract. To be used along with [`Review.Test.expect`](#expect).

If you expect the rule not to report any errors, then you may want to use [`expectDataExtract`](#expectDataExtract) which is simpler.

Note: You do not need to match the exact formatting of the JSON object, though the order of fields does need to match.

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Review.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Review.You.Want.To.Test"
            [ test "should extract even if there are errors" <|
                \() ->
                    [ """module A.B exposing (..)
    import B
    a = 1
    b = 2
    c = 3
    """
                    , """module B exposing (..)
    x = 1
    y = 2
    z = 3
    """
                    ]
                        |> Review.Test.runOnModules rule
                        |> Review.Test.expect
                            [ Review.Test.dataExtract """
    {
        "foo": "bar",
        "other": [ 1, 2, 3 ]
    }"""
                            ]
            ]

-}
dataExtract : String -> ReviewExpectation
dataExtract expectedDataExtract =
    DataExtractExpectation expectedDataExtract


{-| Indicates to the test that the knowledge of ignored files (through [`Review.withIsFileIgnored`](Review-Rule#withIsFileIgnored))
can impact results, and that that is done on purpose.

By default, `elm-review` assumes that the knowledge of which files are ignored will only be used to improve performance,
and not to impact the results of the rule.

Testing that your rule behaves as expected in all your scenarios and with or without some files being ignored can be
very hard. As such, the testing framework will automatically — if you've used `withIsFileIgnored` — run the rule again
but with some of the files being ignored (it will in practice test out all the combinations) and ensure that the results
stat the same with or without ignored files.

If your rule uses this information to change the results (report less or more errors, give different details in the error
message, ...), then you can use this function to tell the test not to attempt re-running and expecting the same results.
In this case, you should write tests where some of the files are ignored yourself.

    test "report an error when..." <|
        \() ->
            [ """
    module ModuleA exposing (a)
    a = 1""", """
    module ModuleB exposing (a)
    a = Debug.log "log" 1""" ]
                |> Review.Test.runOnModules rule
                |> Review.Test.ignoredFilesImpactResults
                |> Review.Test.expect whatYouExpect

-}
ignoredFilesImpactResults : ReviewResult -> ReviewResult
ignoredFilesImpactResults reviewResult =
    case reviewResult of
        SuccessfulRun data _ ->
            SuccessfulRun data DontAttemptReRun

        _ ->
            reviewResult


containsDifferences : List (Diff.Change a) -> Bool
containsDifferences changes =
    case changes of
        [] ->
            False

        (Diff.NoChange _) :: restOfChanges ->
            containsDifferences restOfChanges

        _ ->
            True
