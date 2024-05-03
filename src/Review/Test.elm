module Review.Test exposing (run, ExpectedErrorRange(..))

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

@docs run, ExpectedErrorRange

-}

import Elm.Parser
import Elm.Project
import Elm.Syntax.Range
import Expect
import FastDict
import Json.Decode
import Review
import Review.Test.FailureMessage
import Set exposing (Set)


{-| The result of running a review on a project
-}
type alias ReviewResult =
    Result
        (List String)
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


expectationsViolated : List String -> Expect.Expectation
expectationsViolated =
    \errors ->
        Expect.fail (errors |> String.join "\n\n\n\n")


{-| Run a `Review` on a project, matching all reported module errors, `elm.json` errors and extra file errors
with your provided expected errors.

(elm/core is automatically part of every project)

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Review.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Review.You.Want.To.Test"
            [ test "should ..."
                (\() ->
                    { modules =
                        [ { path = "src/A/B.elm"
                          , source = """
                                module A.B exposing (a, b, c)
                                import B
                                a = 1
                                b = 2
                                c = 3
                                """
                          }
                        , """
                            module B exposing (x, y, z)
                            x = 1
                            y = 2
                            z = 3
                            """
                        ]
                    }
                        |> Review.Test.run rule
                        |> Review.Test.expect
                            [ { path = "src/A/B.elm"
                              , error =
                                  [ { message = "message"
                                    , details = [ "details" ]
                                    , range = Review.Test.Under ...
                                    , fixedSource = Nothing
                                    }
                                  ]
                               }
                            ]
            ]

-}
run :
    { review : Review.Review
    , expectedErrors :
        List
            { path : String
            , errors :
                List
                    { message : String
                    , details : List String
                    , range : ExpectedErrorRange
                    , fixedSource : Maybe String
                    }
            }
    }
    ->
        { modules : List { path : String, source : String }
        , extraFiles : List { path : String, source : String }
        , elmJson : String
        , directDependencies : List { elmJson : String, docsJson : String }
        }
    -> Expect.Expectation
run config project =
    case config.expectedErrors |> invalidExpectedErrorsIn project of
        [] ->
            toReviewResult config.review project |> expectErrors config.expectedErrors

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
    Review.Review
    ->
        { modules : List { path : String, source : String }
        , extraFiles : List { path : String, source : String }
        , elmJson : String
        , directDependencies : List { elmJson : String, docsJson : String }
        }
    -> ReviewResult
toReviewResult review project =
    let
        moduleParseResults :
            Result
                (List { path : String })
                (List { path : String, source : String })
        moduleParseResults =
            modulesWithNormalSource
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

                            Ok _ ->
                                case soFar of
                                    Ok parsedSoFar ->
                                        parsedSoFar
                                            |> (::)
                                                { path = moduleUnparsed.path
                                                , source = moduleUnparsed.source
                                                }
                                            |> Ok

                                    Err failedToParseSoFar ->
                                        failedToParseSoFar |> Err
                    )
                    (Ok [])

        modulesWithNormalSource : List { path : String, source : String }
        modulesWithNormalSource =
            project.modules
                |> List.map
                    (\file -> { path = file.path, source = file.source |> multiLineStringNormalize })

        elmJsonSourceNormal : String
        elmJsonSourceNormal =
            project.elmJson |> multiLineStringNormalize
    in
    case ( moduleParseResults, elmJsonSourceNormal |> Json.Decode.decodeString Elm.Project.decoder ) of
        ( Ok modulesParsed, Ok elmJsonProject ) ->
            let
                runErrorsByPath : FastDict.Dict String (List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix })
                runErrorsByPath =
                    { elmJson = { project = elmJsonProject, source = elmJsonSourceNormal }
                    , directDependencies = project.directDependencies
                    , addedOrChangedModules = modulesWithNormalSource
                    , removedModulePaths = []
                    , addedOrChangedExtraFiles =
                        project.extraFiles
                            |> List.map
                                (\file -> { path = file.path, source = file.source |> multiLineStringNormalize })
                    , removedExtraFilePaths = []
                    , cache = Review.cacheEmpty
                    }
                        |> Review.run review
                        |> .errorsByPath

                fileErrors : List SuccessfulRunResult
                fileErrors =
                    List.concat
                        [ modulesParsed
                            |> List.map
                                (\module_ ->
                                    moduleToRunResult
                                        (runErrorsByPath
                                            |> FastDict.get module_.path
                                            |> Maybe.withDefault []
                                        )
                                        module_
                                )
                        , case runErrorsByPath |> FastDict.get "elm.json" of
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
                        ]
            in
            fileErrors |> Ok

        ( moduleParseErrors, elmJsonParsed ) ->
            Err
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


moduleToRunResult :
    List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix }
    -> { path : String, source : String }
    -> SuccessfulRunResult
moduleToRunResult errorsForModule projectModule =
    { path = projectModule.path
    , source = projectModule.source
    , errors = errorsForModule
    }


invalidExpectedErrorsIn :
    { modules : List { path : String, source : String }
    , extraFiles : List { path : String, source : String }
    , elmJson : String
    , directDependencies : List { elmJson : String, docsJson : String }
    }
    -> (List { path : String, errors : List ExpectedError } -> List String)
invalidExpectedErrorsIn project =
    \expectedErrors ->
        let
            modulePathsSet : Set String
            modulePathsSet =
                project.modules |> List.map .path |> Set.fromList

            projectPaths : Set String
            projectPaths =
                Set.union modulePathsSet (project.extraFiles |> List.map .path |> Set.fromList)
                    |> Set.insert "elm.json"
        in
        [ expectedErrors
            |> List.filterMap
                (\expectedErrorsAtPath ->
                    if projectPaths |> Set.member expectedErrorsAtPath.path then
                        Nothing

                    else
                        Review.Test.FailureMessage.unknownFilesInExpectedErrors expectedErrorsAtPath.path
                            |> Just
                )
        , expectedErrors
            |> List.concatMap
                (\expectedFileErrors ->
                    case expectedFileErrors.path of
                        "elm.json" ->
                            expectedFileErrors.errors

                        _ ->
                            []
                )
            |> List.filterMap
                (\expectedError ->
                    case expectedError.fixedSource of
                        Nothing ->
                            Nothing

                        Just fixedSource ->
                            case fixedSource |> Json.Decode.decodeString Elm.Project.decoder of
                                Ok _ ->
                                    Nothing

                                Err jsonDecodeError ->
                                    Review.Test.FailureMessage.elmJsonFixedSourceParsingFailure
                                        jsonDecodeError
                                        { message = expectedError.message, details = expectedError.details }
                                        |> Just
                )
        , expectedErrors
            |> List.concatMap
                (\expectedFileErrors ->
                    if modulePathsSet |> Set.member expectedFileErrors.path then
                        expectedFileErrors.errors
                            |> List.filterMap
                                (\expectedError ->
                                    case expectedError.fixedSource of
                                        Nothing ->
                                            Nothing

                                        Just fixedSource ->
                                            case fixedSource |> Elm.Parser.parseToFile of
                                                Ok _ ->
                                                    Nothing

                                                Err _ ->
                                                    Review.Test.FailureMessage.moduleFixedSourceParsingFailure
                                                        { path = expectedFileErrors.path
                                                        , errorInfo =
                                                            { message = expectedError.message
                                                            , details = expectedError.details
                                                            }
                                                        }
                                                        |> Just
                                )

                    else
                        []
                )
        ]
            |> List.concat


expectErrors : List { path : String, errors : List ExpectedError } -> (ReviewResult -> Expect.Expectation)
expectErrors expectationsList reviewResult =
    case reviewResult of
        Err errors ->
            expectationsViolated errors

        Ok runResults ->
            let
                expectations : FastDict.Dict String (List ExpectedError)
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
            case runResults of
                [] ->
                    if expectations |> FastDict.isEmpty then
                        Expect.pass

                    else
                        Expect.fail Review.Test.FailureMessage.expectedErrorsButFoundNone

                runResult0 :: runResult1Up ->
                    Expect.all
                        (List.map
                            (\runResult () ->
                                let
                                    expectedErrorsForFile : List ExpectedError
                                    expectedErrorsForFile =
                                        expectations
                                            |> FastDict.get runResult.path
                                            |> Maybe.withDefault []
                                in
                                case expectedErrorsForFile of
                                    [] ->
                                        case runResult.errors of
                                            [] ->
                                                Expect.pass

                                            actualError0 :: actualError1Up ->
                                                Expect.fail
                                                    (Review.Test.FailureMessage.didNotExpectErrors runResult.path
                                                        (actualError0 :: actualError1Up)
                                                    )

                                    expectedError0 :: expectedError1Up ->
                                        checkAllErrorsMatch runResult (expectedError0 :: expectedError1Up)
                            )
                            (runResult0 :: runResult1Up)
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
    checkErrorsMatch runResult
        expectedErrors
        (List.length expectedErrors)
        reviewErrors


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
                            , reviewErrors = listRemoveFirstMember reviewError reorderState.reviewErrors
                            , expectedErrors = restOfExpectedErrors
                        }

                Nothing ->
                    reorderErrors source
                        { reorderState
                            | expectedErrorsWithNoMatch = expectedError :: reorderState.expectedErrorsWithNoMatch
                            , expectedErrors = restOfExpectedErrors
                        }


listRemoveFirstMember : a -> List a -> List a
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


expectedErrorForMessage : ExpectedError -> { message : String, details : List String, under : String }
expectedErrorForMessage =
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


checkErrorsMatch : SuccessfulRunResult -> List ExpectedError -> Int -> List { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> Expect.Expectation
checkErrorsMatch runResult expectedErrors expectedNumberOfErrors errors =
    -- IGNORE TCO
    case ( expectedErrors, errors ) of
        ( [], [] ) ->
            Expect.pass

        ( [], error :: restOfErrors ) ->
            Review.Test.FailureMessage.tooManyErrors runResult.path (error :: restOfErrors)
                |> Expect.fail

        ( expected :: restOfExpectedErrors, [] ) ->
            (expected :: restOfExpectedErrors)
                |> List.map expectedErrorForMessage
                |> Review.Test.FailureMessage.tooFewErrors runResult.path expectedNumberOfErrors
                |> Expect.fail

        ( expected :: restOfExpectedErrors, error :: restOfErrors ) ->
            Expect.all
                [ \() -> checkErrorsMatch runResult restOfExpectedErrors expectedNumberOfErrors restOfErrors
                , \() -> checkErrorMatch runResult.source expected error
                ]
                ()


checkErrorMatch : String -> ExpectedError -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> Expect.Expectation
checkErrorMatch source expectedError error =
    Expect.all
        [ \() ->
            error.message
                |> Expect.equal expectedError.message
                |> Expect.onFail
                    (Review.Test.FailureMessage.messageMismatch
                        (expectedErrorForMessage expectedError)
                        error
                    )
        , checkMessageAppearsUnder source error expectedError
        , checkDetailsAreCorrect error expectedError
        , \() -> checkFixesAreCorrect source error expectedError
        ]
        ()


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
                            Review.Test.FailureMessage.underNotFound { message = expectedError.message }
                                |> Expect.fail

                        _ ->
                            Expect.pass
                , \() ->
                    codeAtLocation
                        |> Expect.equal under
                        |> Expect.onFail (Review.Test.FailureMessage.underMismatch error { under = under, codeAtLocation = codeAtLocation })
                , \() ->
                    expectUnambiguousLocationInSourceCode source error under
                ]

        UnderExactly underExactly ->
            Expect.all
                [ \() ->
                    codeAtLocation
                        |> Expect.equal underExactly.section
                        |> Expect.onFail
                            (Review.Test.FailureMessage.underMismatch error
                                { under = underExactly.section, codeAtLocation = codeAtLocation }
                            )
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


expectUnambiguousLocationInSourceCode : String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fixes : List Review.Fix } -> String -> Expect.Expectation
expectUnambiguousLocationInSourceCode sourceCode error under =
    case String.indexes under sourceCode of
        [ _ ] ->
            Expect.pass

        occurrencesInSourceCode ->
            Expect.fail (Review.Test.FailureMessage.locationIsAmbiguousInSourceCode sourceCode error under occurrencesInSourceCode)


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
            Review.Test.FailureMessage.missingFixes (expectedErrorForMessage expectedError)
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
