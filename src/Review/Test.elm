module Review.Test exposing
    ( ReviewResult, run
    , ExpectedError, error, atExactly, whenFixed, expectErrorsForModules, expectErrorsForElmJson, expectErrorsForReadme
    , expect, ReviewExpectation
    , moduleErrors, elmJsonErrors, readmeErrors
    , elmCore, elmHtml, elmParser, elmUrl, moduleInSrc
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
import Diff
import Elm.Docs
import Elm.Parser
import Elm.Project
import Elm.Syntax.File
import Elm.Syntax.Module as Module
import Elm.Syntax.Node
import Elm.Syntax.Range exposing (Location, Range)
import Expect exposing (Expectation)
import Json.Decode
import Json.Encode
import ListExtra
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
    | SuccessfulRun (List SuccessfulRunResult) ReRun


type alias Project =
    { modules : List { path : String, source : String }
    , extraFiles : List { path : String, content : String }
    , elmJson : String
    , directDependencies : List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
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
    { path : String
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
type alias ExpectedError =
    { message : String
    , details : List String
    , under : Under
    , fixedSource : Maybe String
    }


type Under
    = Under String
    | UnderExactly String Location


moduleInSrc : String -> { path : String, source : String }
moduleInSrc moduleSource =
    case Elm.Parser.parseToFile moduleSource of
        Ok ast ->
            { path = "src/" ++ String.join "/" (Module.moduleName (Elm.Syntax.Node.value ast.moduleDefinition)) ++ ".elm"
            , source = moduleSource
            }

        Err _ ->
            { source = moduleSource, path = "tests/FileFailedToParse.elm" }


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
        ( parsedModules, modulesThatFailedToParse ) =
            List.foldl
                (\( index, moduleUnparsed ) ( parsedModulesSoFar, modulesThatFailedToParseSoFar ) ->
                    case moduleUnparsed.source |> Elm.Parser.parseToFile of
                        Err _ ->
                            ( parsedModulesSoFar
                            , modulesThatFailedToParseSoFar
                                |> (::)
                                    { index = index
                                    , source = moduleUnparsed.source
                                    }
                            )

                        Ok ast ->
                            ( parsedModulesSoFar
                                |> (::)
                                    { path = moduleUnparsed.path
                                    , source = moduleUnparsed.source
                                    , ast = ast
                                    }
                            , modulesThatFailedToParseSoFar
                            )
                )
                ( [], [] )
                (project.modules |> List.indexedMap Tuple.pair)
    in
    case modulesThatFailedToParse of
        moduleThatFailedToParse :: moduleThatFailedToParseOthers ->
            FailedRun
                (FailureMessage.parsingFailure
                    { source = moduleThatFailedToParse.source
                    , index = moduleThatFailedToParse.index
                    , isOnlyFile = moduleThatFailedToParseOthers == []
                    }
                )

        [] ->
            let
                runResult =
                    { modules = project.modules
                    , elmJson = project.elmJson |> Just
                    , extraFiles = project.extraFiles
                    , directDependencies = project.directDependencies
                    }
                        |> Review.run
                            { cache = Review.cacheEmpty
                            }
                            review
            in
            let
                fileErrors : List SuccessfulRunResult
                fileErrors =
                    List.concat
                        [ parsedModules
                            |> List.map
                                (\module_ ->
                                    moduleToRunResult
                                        (runResult.errorsByPath
                                            |> Dict.get module_.path
                                            |> Maybe.withDefault []
                                        )
                                        module_
                                )
                        , case runResult.errorsByPath |> Dict.get "elm.json" of
                            Just errorsForElmJson ->
                                [ { path = "elm.json"
                                  , inspector = codeInspectorForSource False project.elmJson
                                  , errors = errorsForElmJson
                                  }
                                ]

                            Nothing ->
                                []
                        , project.extraFiles
                            |> List.filterMap
                                (\extraFile ->
                                    runResult.errorsByPath
                                        |> Dict.get extraFile.path
                                        |> Maybe.map
                                            (\errorsForExtraFile ->
                                                { path = extraFile.path
                                                , inspector = codeInspectorForSource False extraFile.content
                                                , errors = errorsForExtraFile
                                                }
                                            )
                                )
                        ]
            in
            SuccessfulRun fileErrors
                (AttemptReRun review project)


hasExactlyOneElement : List a -> Bool
hasExactlyOneElement =
    \list ->
        case list of
            [ _ ] ->
                True

            _ ->
                False


moduleToRunResult :
    List Review.Error
    -> { path : String, source : String, ast : Elm.Syntax.File.File }
    -> SuccessfulRunResult
moduleToRunResult errorsForModule projectModule =
    { path =
        projectModule.ast.moduleDefinition
            |> Review.moduleHeaderNameNode
            |> Elm.Syntax.Node.value
            |> String.join "."
    , inspector = codeInspectorForSource True projectModule.source
    , errors =
        errorsForModule
    }


fileTargetFilePath : Review.FileTarget -> String
fileTargetFilePath =
    \errorTarget ->
        case errorTarget of
            Review.ErrorTargetModule moduleInfo ->
                moduleInfo.path

            Review.FileTargetElmJson ->
                "elm.json"

            Review.FileTargetExtra readmeInfo ->
                readmeInfo.path


codeInspectorForSource : Bool -> String -> CodeInspector
codeInspectorForSource isModule source =
    { isModule = isModule
    , source = source
    , getCodeAtLocation = getCodeAtLocationInSourceCode source
    , checkIfLocationIsAmbiguous = checkIfLocationIsAmbiguousInSourceCode source
    }


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
    case reviewResult of
        FailedRun errorMessage ->
            Expect.fail errorMessage

        SuccessfulRun runResults reRun ->
            if expectedErrors == [] then
                expectNoModuleErrors runResults

            else
                case runResults of
                    runResult :: [] ->
                        checkAllErrorsMatch runResult expectedErrors

                    _ ->
                        Expect.fail FailureMessage.needToUsedExpectErrorsForModules


expectNoModuleErrors : List SuccessfulRunResult -> Expectation
expectNoModuleErrors runResults =
    Expect.all
        (runResults
            |> List.map (\fileRunResult () -> fileRunResult |> expectNoErrorForModuleRunResult)
        )
        ()


expectNoErrorForModuleRunResult : SuccessfulRunResult -> Expectation
expectNoErrorForModuleRunResult { path, errors } =
    if List.isEmpty errors then
        Expect.pass

    else
        Expect.fail (FailureMessage.didNotExpectErrors path errors)


expectErrorsForModulesHelp : List ( String, List ExpectedError ) -> List SuccessfulRunResult -> Expectation
expectErrorsForModulesHelp expectedErrorsList runResults =
    let
        maybeUnknownModule : Maybe String
        maybeUnknownModule =
            Set.diff
                (expectedErrorsList |> List.map Tuple.first |> Set.fromList)
                (Set.fromList (List.map .path runResults))
                |> Set.toList
                |> List.head
    in
    case maybeUnknownModule of
        Just unknownModule ->
            FailureMessage.unknownModulesInExpectedErrors unknownModule
                |> Expect.fail

        Nothing ->
            Expect.all
                (expectErrorsForModuleFiles expectedErrorsList runResults)
                ()


expectErrorsForModuleFiles : List ( String, List ExpectedError ) -> List SuccessfulRunResult -> List (() -> Expectation)
expectErrorsForModuleFiles expectedErrorsList runResults =
    List.map
        (\runResult () ->
            let
                expectedErrors : List ExpectedError
                expectedErrors =
                    expectedErrorsList
                        |> ListExtra.firstElementWhere
                            (\( moduleName, _ ) -> moduleName == runResult.path)
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault []
            in
            if List.isEmpty expectedErrors then
                expectNoErrorForModuleRunResult runResult

            else
                checkAllErrorsMatch runResult expectedErrors
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
    expectErrorsForFiles [ ( "elm.json", expectedErrors ) ] reviewResult


expectErrorsForModules : List ( String, List ExpectedError ) -> ReviewResult -> Expectation
expectErrorsForModules modules reviewResult =
    expectErrorsForFiles modules reviewResult


expectErrorsForFiles : List ( String, List ExpectedError ) -> ReviewResult -> Expectation
expectErrorsForFiles files reviewResult =
    Debug.todo ""


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
    expectErrorsForFiles [ ( "README.md", expectedErrors ) ] reviewResult


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
atExactly : { row : Int, column : Int } -> ExpectedError -> ExpectedError
atExactly location expectedError =
    { expectedError | under = UnderExactly (getUnder expectedError) location }


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
whenFixed fixedSource expectedError =
    { expectedError | fixedSource = Just fixedSource }


getUnder : ExpectedError -> String
getUnder expectedError =
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

        expectedError :: restOfExpectedErrors ->
            case findBestMatchingReviewError codeInspector expectedError reorderState.reviewErrors { error = Nothing, confidenceLevel = 0 } of
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


findBestMatchingReviewError : CodeInspector -> ExpectedError -> List Review.Error -> { error : Maybe Review.Error, confidenceLevel : Int } -> Maybe Review.Error
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


matchingConfidenceLevel : CodeInspector -> ExpectedError -> Review.Error -> Int
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

            UnderExactly under start ->
                if codeInspector.getCodeAtLocation (.range reviewError) /= Just under then
                    1

                else if start /= reviewError.range.start then
                    2

                else
                    3


checkAllErrorsMatch : SuccessfulRunResult -> List ExpectedError -> Expectation
checkAllErrorsMatch runResult unorderedExpectedErrors =
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
        (List.reverse
            (checkErrorsMatch runResult
                expectedErrors
                (List.length expectedErrors)
                reviewErrors
            )
        )
        ()


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


checkErrorsMatch : SuccessfulRunResult -> List ExpectedError -> Int -> List Review.Error -> List (() -> Expectation)
checkErrorsMatch runResult expectedErrors expectedNumberOfErrors errors =
    case ( expectedErrors, errors ) of
        ( [], [] ) ->
            [ always Expect.pass ]

        ( expected :: restOfExpectedErrors, error_ :: restOfErrors ) ->
            checkErrorMatch runResult.inspector expected error_
                :: checkErrorsMatch runResult restOfExpectedErrors expectedNumberOfErrors restOfErrors

        ( expected :: restOfExpectedErrors, [] ) ->
            [ \() ->
                (expected :: restOfExpectedErrors)
                    |> List.map extractExpectedErrorData
                    |> FailureMessage.expectedMoreErrors runResult.path expectedNumberOfErrors
                    |> Expect.fail
            ]

        ( [], error_ :: restOfErrors ) ->
            [ \() ->
                FailureMessage.tooManyErrors runResult.path (error_ :: restOfErrors)
                    |> Expect.fail
            ]


extractExpectedErrorData : ExpectedError -> FailureMessage.ExpectedErrorData
extractExpectedErrorData expectedError =
    { message = expectedError.message
    , details = expectedError.details
    , under = getUnder expectedError
    }


checkErrorMatch : CodeInspector -> ExpectedError -> Review.Error -> (() -> Expectation)
checkErrorMatch codeInspector expectedError error_ =
    Expect.all
        [ \() ->
            .message error_
                |> Expect.equal expectedError.message
                |> Expect.onFail
                    (FailureMessage.messageMismatch
                        (extractExpectedErrorData expectedError)
                        error_
                    )
        , checkMessageAppearsUnder codeInspector error_ expectedError
        , checkDetailsAreCorrect error_ expectedError
        , \() -> checkFixesAreCorrect codeInspector error_ expectedError
        ]


checkMessageAppearsUnder : CodeInspector -> Review.Error -> ExpectedError -> (() -> Expectation)
checkMessageAppearsUnder codeInspector error_ expectedError =
    case codeInspector.getCodeAtLocation (.range error_) of
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

                UnderExactly under start ->
                    Expect.all
                        [ \() ->
                            codeAtLocation
                                |> Expect.equal under
                                |> Expect.onFail (FailureMessage.underMismatch error_ { under = under, codeAtLocation = codeAtLocation })
                        , \() ->
                            error_.range.start
                                |> Expect.equal start
                                |> Expect.onFail
                                    (FailureMessage.wrongLocation error_
                                        { start = start, end = { row = start.row, column = start.column + (under |> String.length) } }
                                        under
                                    )
                        ]

        Nothing ->
            \() ->
                FailureMessage.locationNotFound error_
                    |> Expect.fail


checkDetailsAreCorrect : Review.Error -> ExpectedError -> (() -> Expectation)
checkDetailsAreCorrect error_ expectedError =
    Expect.all
        [ \() ->
            List.isEmpty error_.details
                |> Expect.equal False
                |> Expect.onFail (FailureMessage.emptyDetails (.message error_))
        , \() ->
            error_.details
                |> Expect.equal expectedError.details
                |> Expect.onFail (FailureMessage.unexpectedDetails expectedError.details error_)
        ]


checkFixesAreCorrect : CodeInspector -> Review.Error -> ExpectedError -> Expectation
checkFixesAreCorrect codeInspector err expectedError =
    case ( expectedError.fixedSource, err.fixes ) of
        ( Nothing, [] ) ->
            Expect.pass

        ( Just _, [] ) ->
            FailureMessage.missingFixes (extractExpectedErrorData expectedError)
                |> Expect.fail

        ( Nothing, _ :: _ ) ->
            FailureMessage.unexpectedFixes err
                |> Expect.fail

        ( Just expectedFixedSource, fix0 :: fix1Up ) ->
            case Review.fix err.target (fix0 :: fix1Up) codeInspector.source of
                Ok fixedSource ->
                    if fixedSource == expectedFixedSource then
                        Expect.pass

                    else if removeWhitespace fixedSource == removeWhitespace expectedFixedSource then
                        Expect.fail <| FailureMessage.fixedCodeWhitespaceMismatch fixedSource expectedFixedSource err

                    else
                        Expect.fail <| FailureMessage.fixedCodeMismatch fixedSource expectedFixedSource err

                Err Review.AfterFixIsUnchanged ->
                    Expect.fail <| FailureMessage.unchangedSourceAfterFix err

                Err (Review.AfterFixSourceParsingFailed sourceCode) ->
                    Expect.fail <| FailureMessage.invalidSourceAfterFix err sourceCode

                Err Review.FixHasCollisionsInRanges ->
                    Expect.fail <| FailureMessage.hasCollisionsInFixRanges err


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

        SuccessfulRun runResults reRun ->
            expectErrorsForModulesHelp
                (List.map
                    (\expectation ->
                        case expectation of
                            FileErrorExpectation fileExpectation ->
                                ( fileExpectation.path, fileExpectation.errors )
                    )
                    expectations
                )
                runResults


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
moduleErrors filePath expected =
    FileErrorExpectation { path = filePath, errors = expected }


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
    FileErrorExpectation { path = "elm.json", errors = expected }


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
    FileErrorExpectation { path = "README.md", errors = expected }


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
