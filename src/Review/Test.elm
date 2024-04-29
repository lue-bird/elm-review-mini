module Review.Test exposing
    ( ReviewResult, run
    , ExpectedError, expectErrors
    , ExpectedErrorRange(..), elmCore, elmHtml, elmParser, elmUrl, moduleInSrc
    )

{-| Module that helps you test your rules, using [`elm-test`](https://package.elm-lang.org/packages/elm-explorations/test/latest/).

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Review.You.Want.To.Test

    tests : Test
    tests =
        Test.describe "The.Review.You.Want.To.Test"
            [ Test.test "should not report anything when <condition>"
                (\() ->
                    """module A exposing (..)
    a = foo n"""
                        |> Review.Test.run The.Review.You.Want.To.Test.review
                        |> Review.Test.expect []
                )
                , test "should report Debug.log use" <|
                    \() ->
                        """module A exposing (..)
    a = Debug.log "some" "message" """
                            |> Review.Test.run The.Review.You.Want.To.Test.review
                            |> Review.Test.expectErrors
                                [ Review.Test.error
                                    { message = "Remove the use of `Debug` before shipping to production"
                                    , details = [ "Details about the error" ]
                                    , under = "Debug.log"
                                    }
                                ]
                )
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
import Elm.Syntax.ModuleName
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
    , errors : List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix }
    }


type alias CodeInspector =
    { isModule : Bool
    , source : String
    , getCodeAtLocation : Range -> Maybe String
    , checkIfLocationIsAmbiguous : { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> String -> Expectation
    }


{-| An expectation that an error with this shape will be reported.
-}
type alias ExpectedError =
    { message : String
    , details : List String
    , range : ExpectedErrorRange
    , fixedSource : Maybe String
    }


{-| An expectation for the reported error range.
If the section to mark is unique in the file, use `Under "your section source"`.
If the section occurs multiple times in this file, use `UnderExactly { section = "your section source", startingAt = { row = ..., column = ... } }`
-}
type ExpectedErrorRange
    = Under String
    | UnderExactly { section : String, startingAt : Location }


moduleInSrc : String -> { path : String, source : String }
moduleInSrc moduleSource =
    case Elm.Parser.parseToFile moduleSource of
        Ok syntax ->
            { path = "src/" ++ String.join "/" (Module.moduleName (Elm.Syntax.Node.value syntax.moduleDefinition)) ++ ".elm"
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

                        Ok syntax ->
                            ( parsedModulesSoFar
                                |> (::)
                                    { path = moduleUnparsed.path
                                    , source = moduleUnparsed.source
                                    , syntax = syntax
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


moduleToRunResult :
    List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix }
    -> { path : String, source : String, syntax : Elm.Syntax.File.File }
    -> SuccessfulRunResult
moduleToRunResult errorsForModule projectModule =
    { path =
        projectModule.syntax.moduleDefinition
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
            Review.FileTargetElmJson ->
                "elm.json"

            Review.FileTarget readmeInfo ->
                readmeInfo.path


codeInspectorForSource : Bool -> String -> CodeInspector
codeInspectorForSource isModule source =
    { isModule = isModule
    , source = source
    , getCodeAtLocation = getCodeAtLocationInSourceCode source
    , checkIfLocationIsAmbiguous = checkIfLocationIsAmbiguousInSourceCode source
    }


{-| Expect multiple outputs: module errors, `elm.json` errors or extra file errors.

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
expectErrors : List { path : String, errors : List ExpectedError } -> (ReviewResult -> Expectation)
expectErrors expectations reviewResult =
    case reviewResult of
        FailedRun errorMessage ->
            Expect.fail errorMessage

        SuccessfulRun runResults reRun ->
            expectErrorsForModulesHelp
                (List.map
                    (\fileExpectation ->
                        ( fileExpectation.path, fileExpectation.errors )
                    )
                    expectations
                )
                runResults


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
    , range = Under input.under
    , fixedSource = Nothing
    }


expectedErrorRangeSection : ExpectedErrorRange -> String
expectedErrorRangeSection =
    \expectedErrorRange ->
        case expectedErrorRange of
            Under str ->
                str

            UnderExactly underExactly ->
                underExactly.section


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


checkIfLocationIsAmbiguousInSourceCode : String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> String -> Expectation
checkIfLocationIsAmbiguousInSourceCode sourceCode error_ under =
    let
        occurrencesInSourceCode : List Int
        occurrencesInSourceCode =
            String.indexes under sourceCode
    in
    case occurrencesInSourceCode of
        [ _ ] ->
            Expect.pass

        _ ->
            Expect.fail (FailureMessage.locationIsAmbiguousInSourceCode sourceCode error_ under occurrencesInSourceCode)


type alias ReorderState =
    { expectedErrors : List ExpectedError
    , reviewErrors : List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix }
    , pairs : List ( ExpectedError, { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } )
    , expectedErrorsWithNoMatch : List ExpectedError
    }


reorderErrors : CodeInspector -> ReorderState -> ( List ExpectedError, List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } )
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


findBestMatchingReviewError : CodeInspector -> ExpectedError -> List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> { error : Maybe { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix }, confidenceLevel : Int } -> Maybe { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix }
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


matchingConfidenceLevel : CodeInspector -> ExpectedError -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> Int
matchingConfidenceLevel codeInspector expectedErrorDetails reviewError =
    if expectedErrorDetails.message /= .message reviewError then
        0

    else
        case expectedErrorDetails.range of
            Under under ->
                if codeInspector.getCodeAtLocation reviewError.range /= Just under then
                    1

                else
                    2

            UnderExactly underExactly ->
                if codeInspector.getCodeAtLocation reviewError.range /= Just underExactly.section then
                    1

                else if underExactly.startingAt /= reviewError.range.start then
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


checkErrorsMatch : SuccessfulRunResult -> List ExpectedError -> Int -> List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> List (() -> Expectation)
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
extractExpectedErrorData =
    \expectedError ->
        { message = expectedError.message
        , details = expectedError.details
        , under = expectedError.range |> expectedErrorRangeSection
        }


checkErrorMatch : CodeInspector -> ExpectedError -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> (() -> Expectation)
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


checkMessageAppearsUnder : CodeInspector -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> ExpectedError -> (() -> Expectation)
checkMessageAppearsUnder codeInspector error_ expectedError =
    case codeInspector.getCodeAtLocation (.range error_) of
        Just codeAtLocation ->
            case expectedError.range of
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

                UnderExactly underExactly ->
                    Expect.all
                        [ \() ->
                            codeAtLocation
                                |> Expect.equal underExactly.section
                                |> Expect.onFail (FailureMessage.underMismatch error_ { under = underExactly.section, codeAtLocation = codeAtLocation })
                        , \() ->
                            error_.range.start
                                |> Expect.equal underExactly.startingAt
                                |> Expect.onFail
                                    (FailureMessage.wrongLocation error_
                                        { start = underExactly.startingAt
                                        , end =
                                            { row = underExactly.startingAt.row
                                            , column = underExactly.startingAt.column + (underExactly.section |> String.length)
                                            }
                                        }
                                        underExactly.section
                                    )
                        ]

        Nothing ->
            \() ->
                FailureMessage.locationNotFound error_
                    |> Expect.fail


checkDetailsAreCorrect : { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> ExpectedError -> (() -> Expectation)
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


checkFixesAreCorrect : CodeInspector -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> ExpectedError -> Expectation
checkFixesAreCorrect codeInspector reviewError expectedError =
    case ( expectedError.fixedSource, reviewError.fixes ) of
        ( Nothing, [] ) ->
            Expect.pass

        ( Just _, [] ) ->
            FailureMessage.missingFixes (extractExpectedErrorData expectedError)
                |> Expect.fail

        ( Nothing, _ :: _ ) ->
            FailureMessage.unexpectedFixes reviewError
                |> Expect.fail

        ( Just expectedFixedSource, fix0 :: fix1Up ) ->
            case Review.fixFile (fix0 :: fix1Up) codeInspector.source of
                Ok fixedSource ->
                    if fixedSource == expectedFixedSource then
                        Expect.pass

                    else if removeWhitespace fixedSource == removeWhitespace expectedFixedSource then
                        Expect.fail <| FailureMessage.fixedCodeWhitespaceMismatch fixedSource expectedFixedSource reviewError

                    else
                        Expect.fail <| FailureMessage.fixedCodeMismatch fixedSource expectedFixedSource reviewError

                Err Review.AfterFixIsUnchanged ->
                    Expect.fail <| FailureMessage.unchangedSourceAfterFix reviewError

                Err Review.FixHasCollisionsInRanges ->
                    Expect.fail <| FailureMessage.hasCollisionsInFixRanges reviewError


removeWhitespace : String -> String
removeWhitespace =
    \string -> string |> String.replace " " "" |> String.replace "\n" ""


type CompiledDataExtract
    = NoDataExtractExpected
    | DataExtractExpected String
    | MultipleDataExtractExpected


containsDifferences : List (Diff.Change a) -> Bool
containsDifferences changes =
    case changes of
        [] ->
            False

        (Diff.NoChange _) :: restOfChanges ->
            containsDifferences restOfChanges

        _ ->
            True
