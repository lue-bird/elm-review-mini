module Review.Test exposing (run, applicationConfigAfterElmInit, ExpectedErrorRange(..))

{-| Test your review using [`elm-test`](https://package.elm-lang.org/packages/elm-explorations/test/latest/).

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

@docs run, applicationConfigAfterElmInit, ExpectedErrorRange

-}

import Elm.Docs
import Elm.Parser
import Elm.Project
import Elm.Syntax.File
import Elm.Syntax.Range
import ElmJson.LocalExtra
import Expect
import FastDict
import Json.Decode
import Review
import Review.Test.FailureMessage
import Set exposing (Set)


{-| Equivalent to (copy and adapt if necessary)

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

-}
applicationConfigAfterElmInit : { elmJson : String, directDependencies : List { elmJson : String, docsJson : String } }
applicationConfigAfterElmInit =
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
            , errors :
                List
                    { range : Elm.Syntax.Range.Range
                    , message : String
                    , details : List String
                    , fix : List Review.Fix
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
            , fix : List Review.Fix
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
If the section to mark is unique in the source, use `Under "your section source"`.
If the section occurs multiple times in the source, use `UnderExactly { section = "your section source", startingAt = { row = ..., column = ... } }`
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

    import Review.Test
    import Test exposing (Test, describe, test)
    import The.Review.You.Want.To.Test exposing (rule)

    tests : Test
    tests =
        describe "The.Review.You.Want.To.Test"
            [ test "should report ... when ..."
                (\() ->
                    { projectConfig = Review.Test.applicationConfigAfterElmInit
                    , files =
                        [ { path = "src/A/B.elm"
                          , source = """
                              module A.B exposing (a, b, c)
                              import B
                              a = 1
                              b = 2
                              c = 3
                              """
                          }
                        , { path = "src/B.elm"
                          , source = """
                              module B exposing (x, y, z)
                              x = 1
                              y = 2
                              z = 3
                              """
                          }
                        ]
                    , review = YourConvention.review
                    , expectedErrors =
                        [ { path = "src/A/B.elm"
                          , message = "message"
                          , details = [ "details" ]
                          , range = Review.Test.Under "where it's located"
                          , fixedSource = Nothing
                          }
                        ]
                    }
                        |> Review.Test.run
                )
            ]

The extra indentation specified by the first line will be stripped of all the provided source strings.

You can also specify a custom project config in case you need to test a package
or want to add dependencies (elm/core is automatically part of every tested project)

    import Review.Test
    import Test exposing (Test)
    import DebugForbid

    tests : Test
    tests =
        Test.describe "DebugForbid"
            [ Test.test "report Debug.log use"
                (\() ->
                    { files =
                        [ { path = "src/A.elm"
                          , source = """
                                module A exposing (a)
                                a =
                                    Debug.log "some" "message"
                                """
                          }
                        ]
                    , projectConfig =
                        { elmJson = """
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
                        , directDependencies =
                            [ ..elm html elm.json and docs.json copied from elm home.. ]
                        }
                    , review = DebugForbid.review
                    , expectedErrors =
                        [ { path = "src/A.elm"
                          , message = "Remove the use of `Debug` before shipping to production"
                          , details = [ "Compiling elm code in optimized mode does not allow these helpers." ]
                          , range = Review.Test.Under "Debug.log"
                          , fixedSource =
                              Just """
                                  module A exposing (a)
                                  a =
                                      "message"
                                  """
                          }
                        ]
                    }
                        |> Review.Test.run

                )
            ]

-}
run :
    { files : List { path : String, source : String }
    , projectConfig :
        { elmJson : String, directDependencies : List { elmJson : String, docsJson : String } }
    , review : Review.Review
    , expectedErrors :
        List
            { path : String
            , message : String
            , details : List String
            , range : ExpectedErrorRange
            , fixedSource : Maybe String
            }
    }
    -> Expect.Expectation
run config =
    let
        elmJsonSourceNormal : String
        elmJsonSourceNormal =
            config.projectConfig.elmJson |> multiLineStringNormalize
    in
    case elmJsonSourceNormal |> Json.Decode.decodeString Elm.Project.decoder of
        Err elmJsonParseError ->
            [ elmJsonParseError |> Review.Test.FailureMessage.elmJsonParsingFailure ] |> expectationsViolated

        Ok elmJsonProject ->
            let
                filesWithNormalSource : List { path : String, source : String }
                filesWithNormalSource =
                    config.files
                        |> List.map
                            (\file -> { path = file.path, source = file.source |> multiLineStringNormalize })

                expectedErrorsWithNormalizedFixedSource : List { path : String, errors : List { message : String, details : List String, range : ExpectedErrorRange, fixedSource : Maybe String } }
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
                                                    , fixedSource = error.fixedSource |> Maybe.map multiLineStringNormalize
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
                    toReviewResult config.review
                        { files = filesWithNormalSource
                        , directDependencies = config.projectConfig.directDependencies
                        , elmJson = { source = elmJsonSourceNormal, project = elmJsonProject }
                        }
                        |> expectErrors expectedErrorsWithNormalizedFixedSource

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
                |> List.map Review.Test.FailureMessage.moduleParsingFailure
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
                                        , errors = [ Review.Test.FailureMessage.dependencyDocsJsonParsingFailure docsJsonParseError ]
                                        }

                                    ( Err elmJsonParseError, Ok _ ) ->
                                        { parsed = Nothing
                                        , errors = [ Review.Test.FailureMessage.dependencyElmJsonParsingFailure elmJsonParseError ]
                                        }

                                    ( Err elmJsonParseError, Err docsJsonParseError ) ->
                                        { parsed = Nothing
                                        , errors =
                                            [ Review.Test.FailureMessage.dependencyDocsJsonParsingFailure docsJsonParseError
                                            , Review.Test.FailureMessage.dependencyElmJsonParsingFailure elmJsonParseError
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
                        runErrorsByPath : FastDict.Dict String (List { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix })
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
                                |> Review.run review
                                |> .errorsByPath

                        fileErrors : List SuccessfulRunResult
                        fileErrors =
                            [ modulesParsed
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
                            , case runErrorsByPath |> FastDict.get "elm.json" of
                                Just errorsForElmJson ->
                                    [ { path = "elm.json"
                                      , source = project.elmJson.source
                                      , errors = errorsForElmJson
                                      }
                                    ]

                                Nothing ->
                                    []
                            , extraFiles
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
                                |> List.concat
                    in
                    fileErrors |> Ok


invalidExpectedErrorsIn :
    { files : List { path : String, source : String }
    , project : Elm.Project.Project
    }
    -> (List { path : String, errors : List ExpectedError } -> List String)
invalidExpectedErrorsIn project =
    \expectedErrors ->
        let
            sourceDirectories : List String
            sourceDirectories =
                project.project |> ElmJson.LocalExtra.sourceDirectories

            pathIsModule : String -> Bool
            pathIsModule =
                \path ->
                    (path |> String.endsWith ".elm")
                        && (sourceDirectories |> List.any (\dir -> path |> String.startsWith dir))

            modulePaths : Set String
            modulePaths =
                project.files
                    |> List.map .path
                    |> List.filter (\path -> path |> pathIsModule)
                    |> Set.fromList

            projectPaths : Set String
            projectPaths =
                (project.files |> List.map .path |> Set.fromList)
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
                    if modulePaths |> Set.member expectedFileErrors.path then
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


reorderErrors : String -> ReorderState -> ( List ExpectedError, List { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } )
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


findBestMatchingReviewError : String -> ExpectedError -> List { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> { error : Maybe { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix }, confidenceLevel : Int } -> Maybe { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix }
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


matchingConfidenceLevel : String -> ExpectedError -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> Int
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


checkErrorsMatch : SuccessfulRunResult -> List ExpectedError -> Int -> List { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> Expect.Expectation
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


checkErrorMatch : String -> ExpectedError -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> Expect.Expectation
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


checkMessageAppearsUnder : String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> ExpectedError -> (() -> Expect.Expectation)
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


expectUnambiguousLocationInSourceCode : String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> String -> Expect.Expectation
expectUnambiguousLocationInSourceCode sourceCode error under =
    case String.indexes under sourceCode of
        [ _ ] ->
            Expect.pass

        occurrencesInSourceCode ->
            Expect.fail (Review.Test.FailureMessage.locationIsAmbiguousInSourceCode sourceCode error under occurrencesInSourceCode)


checkDetailsAreCorrect : { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> ExpectedError -> (() -> Expect.Expectation)
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


checkFixesAreCorrect : String -> { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } -> ExpectedError -> Expect.Expectation
checkFixesAreCorrect source reviewError expectedError =
    case ( expectedError.fixedSource, reviewError.fix ) of
        ( Nothing, [] ) ->
            Expect.pass

        ( Just _, [] ) ->
            Review.Test.FailureMessage.missingFixes (expectedErrorForMessage expectedError)
                |> Expect.fail

        ( Nothing, _ :: _ ) ->
            Review.Test.FailureMessage.unexpectedFixes reviewError
                |> Expect.fail

        ( Just expectedFixedSource, fix0 :: fix1Up ) ->
            case Review.applyFix (fix0 :: fix1Up) source of
                Ok fixedSource ->
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
    , reviewErrors : List { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix }
    , pairs : List ( ExpectedError, { range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix } )
    , expectedErrorsWithNoMatch : List ExpectedError
    }
