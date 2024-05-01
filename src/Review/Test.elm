module Review.Test exposing
    ( moduleInSrc, elmHtml, elmParser, elmUrl
    , run, ReviewResult(..)
    , expectErrors, ExpectedError, ExpectedErrorRange(..)
    )

{-| Module that helps you test your rules, using [`elm-test`](https://package.elm-lang.org/packages/elm-explorations/test/latest/).

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Review.You.Want.To.Test

    tests : Test
    tests =
        Test.describe "The.Review.You.Want.To.Test"
            [ Test.test "report Debug.log use"
                (\() ->
                    { modules =
                        [ Review.Test.moduleInSrc """
                            module A exposing (a)
                            a =
                                Debug.log "some" "message"
                            """
                        ]
                    , elmJson = """
                        {
                            "type": "application",
                            "source-directories": [
                                "src"
                            ],
                            "elm-version": "0.19.1",
                            "dependencies": {
                                "direct": {
                                    "elm/core": "1.0.5",
                                    "elm/html": "1.0.0"
                                },
                                "indirect": {
                                }
                            },
                            "test-dependencies": {
                                "direct": {},
                                "indirect": {}
                            }
                        }
                        """
                    , extraFiles = []
                    , directDependencies = Review.Test.elmHtml
                    }
                        |> Review.Test.run The.Review.You.Want.To.Test.review
                        |> Review.Test.expectErrors
                            [ { path = "src/A.elm"
                              , errors =
                                  [ { message = "Remove the use of `Debug` before shipping to production"
                                    , details = [ "Compiling elm code in optimized mode does not allow these helpers." ]
                                    , under = "Debug.log"
                                    , fixedSource =
                                        Just """
                                            module A exposing (a)
                                            a =
                                                "message"
                                            """
                                      }
                                  ]
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
    in a [`Test.describe`](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test#describe)
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


## create a project

@docs moduleInSrc, elmHtml, elmParser, elmUrl


## run it

@docs run, ReviewResult


## expectation

@docs expectErrors, ExpectedError, ExpectedErrorRange

-}

import Array exposing (Array)
import Dict
import Elm.Docs
import Elm.Parser
import Elm.Project
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Range
import Expect
import Json.Decode
import ListExtra
import Review
import Review.Test.Dependencies.ElmCore
import Review.Test.Dependencies.ElmHtml
import Review.Test.Dependencies.ElmParser
import Review.Test.Dependencies.ElmUrl
import Review.Test.FailureMessage
import Set
import Unicode


{-| Dependency for `elm/core`. It contains operators.

It is present by default in `elm-review` tests when you use [`Review.Test.run`](#run)

-}
elmCore : { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
elmCore =
    Review.Test.Dependencies.ElmCore.dependency


{-| Dependency for `elm/html`
-}
elmHtml : { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
elmHtml =
    Review.Test.Dependencies.ElmHtml.dependency


{-| Dependency for `elm/parser`. It contains operators
-}
elmParser : { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
elmParser =
    Review.Test.Dependencies.ElmParser.dependency


{-| Dependency for `elm/url`. It contains operators
-}
elmUrl : { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
elmUrl =
    Review.Test.Dependencies.ElmUrl.dependency


{-| The result of running a review on a project
-}
type ReviewResult
    = FailedRun (List String)
    | SuccessfulRun
        (List
            { path : String
            , source : String
            , errors :
                List
                    { range : Elm.Syntax.Range.Range
                    , message : String
                    , details : List String
                    , fixes : List Review.Fix
                    }
            }
        )


type alias SuccessfulRunResult =
    { path : String
    , source : String
    , errors :
        List
            { range : Elm.Syntax.Range.Range
            , message : String
            , details : List String
            , fixes : List Review.Fix
            }
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
    | UnderExactly { section : String, startingAt : Elm.Syntax.Range.Location }


{-| Add an elm module under the src/ path
-}
moduleInSrc : String -> { path : String, source : String }
moduleInSrc moduleSource =
    case Elm.Parser.parseToFile moduleSource of
        Ok syntax ->
            { path = [ "src/", String.join "/" (Elm.Syntax.Module.moduleName (Elm.Syntax.Node.value syntax.moduleDefinition)), ".elm" ] |> String.concat
            , source = moduleSource
            }

        Err _ ->
            { source = moduleSource, path = "tests/FileFailedToParse.elm" }


getCodeAtLocationInSourceCode : String -> Elm.Syntax.Range.Range -> Maybe String
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


checkIfLocationIsAmbiguousInSourceCode : String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> String -> Expect.Expectation
checkIfLocationIsAmbiguousInSourceCode sourceCode error under =
    let
        occurrencesInSourceCode : List Int
        occurrencesInSourceCode =
            String.indexes under sourceCode
    in
    case occurrencesInSourceCode of
        [ _ ] ->
            Expect.pass

        _ ->
            Expect.fail (Review.Test.FailureMessage.locationIsAmbiguousInSourceCode sourceCode error under occurrencesInSourceCode)


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

(elm/core is automatically added to every tested project)

The source codes need to be syntactically valid Elm code. If the code
can't be parsed, the test will fail regardless of the expectations you set on it.

Note that to be syntactically valid, you need at least a module declaration at the
top of each file (like `module A exposing (..)`) and one declaration (like `a = 1`).
You can't just have an expression like `1 + 2`.

Note: This is a more complex version of [`runOnModules`](#runOnModules). If your rule is not
interested in project related details, then you should use [`runOnModules`](#runOnModules) instead.

-}
run :
    Review.Review
    ->
        { modules : List { path : String, source : String }
        , extraFiles : List { path : String, content : String }
        , elmJson : String
        , directDependencies : List { elmJson : Elm.Project.Project, modules : List Elm.Docs.Module }
        }
    -> ReviewResult
run review project =
    let
        moduleParseResults :
            Result
                (List { path : String })
                (List { path : String, source : String, syntax : Elm.Syntax.File.File })
        moduleParseResults =
            List.foldl
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
                project.modules

        elmJsonSourceNormal : String
        elmJsonSourceNormal =
            project.elmJson |> multiLineStringNormalize
    in
    case ( moduleParseResults, elmJsonSourceNormal |> Json.Decode.decodeString Elm.Project.decoder ) of
        ( Ok modulesParsed, Ok elmJsonProject ) ->
            let
                runResult =
                    { elmJson = { project = elmJsonProject, source = elmJsonSourceNormal }
                    , directDependencies = elmCore :: project.directDependencies
                    , addedOrChangedModulesByPath =
                        project.modules
                            |> List.map (\file -> ( file.path, file.source |> multiLineStringNormalize ))
                            |> Dict.fromList
                    , removedModulePaths = []
                    , addedOrChangedExtraFilesByPath =
                        project.extraFiles
                            |> List.map (\file -> ( file.path, file.content |> multiLineStringNormalize ))
                            |> Dict.fromList
                    , removedExtraFilePaths = []
                    , cache = Review.cacheEmpty
                    }
                        |> Review.run review

                fileErrors : List SuccessfulRunResult
                fileErrors =
                    List.concat
                        [ modulesParsed
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
                                  , source = project.elmJson
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
                                                , source = extraFile.content
                                                , errors = errorsForExtraFile
                                                }
                                            )
                                )
                        ]
            in
            SuccessfulRun fileErrors

        ( moduleParseErrors, elmJsonParsed ) ->
            FailedRun
                ((case moduleParseErrors of
                    Ok _ ->
                        []

                    Err modulesThatFailedToParse ->
                        modulesThatFailedToParse
                            |> List.map Review.Test.FailureMessage.moduleParsingFailure
                 )
                    |> (case elmJsonParsed of
                            Ok _ ->
                                identity

                            Err parseError ->
                                (::) (parseError |> Review.Test.FailureMessage.elmJsonParsingFailure)
                       )
                )


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
    , source = projectModule.source
    , errors =
        errorsForModule
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
expectErrors : List { path : String, errors : List ExpectedError } -> (ReviewResult -> Expect.Expectation)
expectErrors expectations reviewResult =
    case reviewResult of
        FailedRun errorMessage ->
            Expect.fail (errorMessage |> String.join "\n\n\n\n")

        SuccessfulRun runResults ->
            expectErrorsHelp
                (List.map
                    (\fileExpectation ->
                        ( fileExpectation.path, fileExpectation.errors )
                    )
                    expectations
                )
                runResults


expectErrorsHelp : List ( String, List ExpectedError ) -> List SuccessfulRunResult -> Expect.Expectation
expectErrorsHelp expectedErrorsList runResults =
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
            Review.Test.FailureMessage.unknownModulesInExpectedErrors unknownModule
                |> Expect.fail

        Nothing ->
            Expect.all
                (List.map
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
                            if List.isEmpty runResult.errors then
                                Expect.pass

                            else
                                Expect.fail (Review.Test.FailureMessage.didNotExpectErrors runResult.path runResult.errors)

                        else
                            checkAllErrorsMatch runResult expectedErrors
                    )
                    runResults
                )
                ()


checkAllErrorsMatch : SuccessfulRunResult -> List ExpectedError -> Expect.Expectation
checkAllErrorsMatch runResult unorderedExpectedErrors =
    let
        ( expectedErrors, reviewErrors ) =
            reorderErrors
                runResult.source
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


reorderErrors : String -> ReorderState -> ( List ExpectedError, List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } )
reorderErrors source reorderState =
    case reorderState.expectedErrors of
        [] ->
            ( List.reverse (reorderState.expectedErrorsWithNoMatch ++ List.map Tuple.first reorderState.pairs)
            , List.reverse (reorderState.reviewErrors ++ List.map Tuple.second reorderState.pairs)
            )

        expectedError :: restOfExpectedErrors ->
            case findBestMatchingReviewError source expectedError reorderState.reviewErrors { error = Nothing, confidenceLevel = 0 } of
                Just reviewError ->
                    reorderErrors source
                        { reorderState
                            | pairs = ( expectedError, reviewError ) :: reorderState.pairs
                            , reviewErrors = removeFirstOccurrence reviewError reorderState.reviewErrors
                            , expectedErrors = restOfExpectedErrors
                        }

                Nothing ->
                    reorderErrors source
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


findBestMatchingReviewError : String -> ExpectedError -> List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> { error : Maybe { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix }, confidenceLevel : Int } -> Maybe { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix }
findBestMatchingReviewError source expectedErrorDetails reviewErrors bestMatch =
    case reviewErrors of
        [] ->
            bestMatch.error

        reviewError :: restOfReviewErrors ->
            let
                confidenceLevel : Int
                confidenceLevel =
                    matchingConfidenceLevel source expectedErrorDetails reviewError
            in
            if confidenceLevel > bestMatch.confidenceLevel then
                findBestMatchingReviewError
                    source
                    expectedErrorDetails
                    restOfReviewErrors
                    { error = Just reviewError, confidenceLevel = confidenceLevel }

            else
                findBestMatchingReviewError
                    source
                    expectedErrorDetails
                    restOfReviewErrors
                    bestMatch


matchingConfidenceLevel : String -> ExpectedError -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> Int
matchingConfidenceLevel source expectedErrorDetails reviewError =
    if expectedErrorDetails.message /= .message reviewError then
        0

    else
        let
            sourceInErrorRange : String
            sourceInErrorRange =
                source |> Review.sourceExtractInRange reviewError.range
        in
        case expectedErrorDetails.range of
            Under under ->
                if sourceInErrorRange /= under then
                    1

                else
                    2

            UnderExactly underExactly ->
                if sourceInErrorRange /= underExactly.section then
                    1

                else if underExactly.startingAt /= reviewError.range.start then
                    2

                else
                    3


extractExpectedErrorData : ExpectedError -> Review.Test.FailureMessage.ExpectedErrorData
extractExpectedErrorData =
    \expectedError ->
        { message = expectedError.message
        , details = expectedError.details
        , under = expectedError.range |> expectedErrorRangeSection
        }


expectedErrorRangeSection : ExpectedErrorRange -> String
expectedErrorRangeSection =
    \expectedErrorRange ->
        case expectedErrorRange of
            Under str ->
                str

            UnderExactly underExactly ->
                underExactly.section


checkErrorsMatch : SuccessfulRunResult -> List ExpectedError -> Int -> List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> List (() -> Expect.Expectation)
checkErrorsMatch runResult expectedErrors expectedNumberOfErrors errors =
    case ( expectedErrors, errors ) of
        ( [], [] ) ->
            [ always Expect.pass ]

        ( [], error :: restOfErrors ) ->
            [ \() ->
                Review.Test.FailureMessage.tooManyErrors runResult.path (error :: restOfErrors)
                    |> Expect.fail
            ]

        ( expected :: restOfExpectedErrors, [] ) ->
            [ \() ->
                (expected :: restOfExpectedErrors)
                    |> List.map extractExpectedErrorData
                    |> Review.Test.FailureMessage.expectedMoreErrors runResult.path expectedNumberOfErrors
                    |> Expect.fail
            ]

        ( expected :: restOfExpectedErrors, error :: restOfErrors ) ->
            checkErrorMatch runResult.source expected error
                :: checkErrorsMatch runResult restOfExpectedErrors expectedNumberOfErrors restOfErrors


checkErrorMatch : String -> ExpectedError -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> (() -> Expect.Expectation)
checkErrorMatch source expectedError error =
    Expect.all
        [ \() ->
            .message error
                |> Expect.equal expectedError.message
                |> Expect.onFail
                    (Review.Test.FailureMessage.messageMismatch
                        (extractExpectedErrorData expectedError)
                        error
                    )
        , checkMessageAppearsUnder source error expectedError
        , checkDetailsAreCorrect error expectedError
        , \() -> checkFixesAreCorrect source error expectedError
        ]


checkMessageAppearsUnder : String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> ExpectedError -> (() -> Expect.Expectation)
checkMessageAppearsUnder source error expectedError =
    let
        codeAtLocation : String
        codeAtLocation =
            source |> Review.sourceExtractInRange error.range
    in
    case expectedError.range of
        Under under ->
            Expect.all
                [ \() ->
                    case under of
                        "" ->
                            Review.Test.FailureMessage.underMayNotBeEmpty
                                { message = expectedError.message
                                , codeAtLocation = codeAtLocation
                                }
                                |> Expect.fail

                        _ ->
                            Expect.pass
                , \() ->
                    codeAtLocation
                        |> Expect.equal under
                        |> Expect.onFail (Review.Test.FailureMessage.underMismatch error { under = under, codeAtLocation = codeAtLocation })
                , \() ->
                    checkIfLocationIsAmbiguousInSourceCode source error under
                ]

        UnderExactly underExactly ->
            Expect.all
                [ \() ->
                    codeAtLocation
                        |> Expect.equal underExactly.section
                        |> Expect.onFail (Review.Test.FailureMessage.underMismatch error { under = underExactly.section, codeAtLocation = codeAtLocation })
                , \() ->
                    error.range.start
                        |> Expect.equal underExactly.startingAt
                        |> Expect.onFail
                            (Review.Test.FailureMessage.wrongLocation error
                                { start = underExactly.startingAt
                                , end =
                                    { row = underExactly.startingAt.row
                                    , column = underExactly.startingAt.column + (underExactly.section |> String.length)
                                    }
                                }
                                underExactly.section
                            )
                ]


checkDetailsAreCorrect : { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> ExpectedError -> (() -> Expect.Expectation)
checkDetailsAreCorrect error expectedError =
    Expect.all
        [ \() ->
            List.isEmpty error.details
                |> Expect.equal False
                |> Expect.onFail (Review.Test.FailureMessage.emptyDetails (.message error))
        , \() ->
            error.details
                |> Expect.equal expectedError.details
                |> Expect.onFail (Review.Test.FailureMessage.unexpectedDetails expectedError.details error)
        ]


checkFixesAreCorrect : String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> ExpectedError -> Expect.Expectation
checkFixesAreCorrect source reviewError expectedError =
    case ( expectedError.fixedSource, reviewError.fixes ) of
        ( Nothing, [] ) ->
            Expect.pass

        ( Just _, [] ) ->
            Review.Test.FailureMessage.missingFixes (extractExpectedErrorData expectedError)
                |> Expect.fail

        ( Nothing, _ :: _ ) ->
            Review.Test.FailureMessage.unexpectedFixes reviewError
                |> Expect.fail

        ( Just expectedFixedSourceNotNormalized, fix0 :: fix1Up ) ->
            case Review.fixFile (fix0 :: fix1Up) source of
                Ok fixedSource ->
                    let
                        expectedFixedSource : String
                        expectedFixedSource =
                            expectedFixedSourceNotNormalized |> multiLineStringNormalize
                    in
                    if fixedSource == expectedFixedSource then
                        Expect.pass

                    else if removeWhitespace fixedSource == removeWhitespace expectedFixedSource then
                        Expect.fail (Review.Test.FailureMessage.fixedCodeWhitespaceMismatch fixedSource expectedFixedSource reviewError)

                    else
                        Expect.fail (Review.Test.FailureMessage.fixedCodeMismatch fixedSource expectedFixedSource reviewError)

                Err Review.AfterFixIsUnchanged ->
                    Expect.fail (Review.Test.FailureMessage.unchangedSourceAfterFix reviewError)

                Err Review.FixHasCollisionsInRanges ->
                    Expect.fail (Review.Test.FailureMessage.hasCollisionsInFixRanges reviewError)


removeWhitespace : String -> String
removeWhitespace =
    \string -> string |> String.replace " " "" |> String.replace "\n" ""


type alias ReorderState =
    { expectedErrors : List ExpectedError
    , reviewErrors : List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix }
    , pairs : List ( ExpectedError, { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } )
    , expectedErrorsWithNoMatch : List ExpectedError
    }
