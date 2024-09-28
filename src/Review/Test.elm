module Review.Test exposing (run, applicationConfigurationMinimal, ExpectedErrorRange(..))

{-| Test your review using [`elm-explorations/test`](https://dark.elm.dmy.fr/packages/elm-explorations/test/latest/).

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

@docs run, applicationConfigurationMinimal, ExpectedErrorRange

-}

import Ansi
import Diff
import Elm.Docs
import Elm.Parser
import Elm.Project
import Elm.Syntax.File
import Elm.Syntax.Range
import ElmJson.LocalExtra
import Expect
import FastDict
import FastDictLocalExtra
import FastSet
import FastSetLocalExtra
import Json.Decode
import ListLocalExtra
import Review


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
    = Under String
    | UnderExactly { section : String, startingAt : Elm.Syntax.Range.Location }


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
                                import Html
                                a =
                                    Debug.log "some" "message"
                                """
                          }
                        ]
                    , projectConfiguration =
                    , review = DebugForbid.review
                    , expectedErrors =
                        [ { path = "src/A.elm"
                          , message = "Remove the use of `Debug` before shipping to production"
                          , details = [ "Compiling elm code in optimized mode does not allow these helpers." ]
                          , range = Review.Test.Under "Debug.log"
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
                        |> Review.Test.run
                )
            ]

The extra indentation specified by the first line will be stripped of all the provided source strings.

You can also specify a custom project config in case you need to test a package
or want to add dependencies (elm/core is automatically part of every tested project)
by supplying the raw sources for the `elm.json` and the direct dependency `docs.json`
and `elm.json` you can find in your elm root folder.

-}
run :
    { files : List { path : String, source : String }
    , projectConfiguration :
        { elmJson : String, directDependencies : List { elmJson : String, docsJson : String } }
    , review : Review.Review
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
run config =
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
                                |> Review.run review
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
            Under under ->
                if (source |> Review.sourceExtractInRange reviewError.range) /= under then
                    1

                else
                    2

            UnderExactly underExactly ->
                if (source |> Review.sourceExtractInRange reviewError.range) /= underExactly.section then
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
                Under section ->
                    section

                UnderExactly underExactly ->
                    underExactly.section
        }


checkMessageAppearsUnder :
    { source : String, reviewError : FileReviewError, expected : ExpectedFileError }
    -> Expect.Expectation
checkMessageAppearsUnder toCheck =
    let
        errorSourceInRange : String
        errorSourceInRange =
            toCheck.source |> Review.sourceExtractInRange toCheck.reviewError.range
    in
    case toCheck.expected.range of
        Under under ->
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

        UnderExactly underExactly ->
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
         , """ locations where that code appeared. Switch from Review.Test.Under to
Review.Test.UnderExactly to make the range you were targeting unambiguous.

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
    -> List Review.SourceEdit
    -> Maybe String
checkFileFixesAreCorrect expectedError source expectedFixedSource reviewErrorEdits =
    case Review.sourceApplyEdits reviewErrorEdits source of
        Ok fixedSource ->
            if fixedSource == expectedFixedSource then
                Nothing

            else if removeWhitespace fixedSource == removeWhitespace expectedFixedSource then
                Just (fixedSourceWhitespaceMismatch fixedSource expectedFixedSource expectedError)

            else
                Just (fixedSourceMismatch fixedSource expectedFixedSource expectedError)

        Err Review.AfterFixIsUnchanged ->
            Just (unchangedSourceAfterFix expectedError)

        Err Review.FixHasCollisionsInRanges ->
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
    , fixEditsByPath : FastDict.Dict String (List Review.SourceEdit)
    }
