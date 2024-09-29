module Review.Cli exposing
    ( program, Program
    , ProgramEvent(..), ProgramState(..)
    )

{-|

@docs program, Program

If you experience a high memory footprint in a large project,
please open an issue called "only ask for file sources to generate local error displays".

That's all you need to know.
Below are the internal types if you're interested:

@docs ProgramEvent, ProgramState

-}

import Ansi
import Array exposing (Array)
import Diff
import Elm.Docs
import Elm.Parser
import Elm.Project
import Elm.Syntax.File
import Elm.Syntax.Range
import FastDict
import FastDictLocalExtra
import Json.Decode
import Json.Encode
import ListLocalExtra
import Review exposing (Review)
import Unicode


{-| An elm worker produced by [`Review.Cli.Program`](Review-Cli#Program)
-}
type alias Program =
    Platform.Program () ProgramState ProgramEvent


{-| elm model of a [`Review.program`](#program)
-}
type ProgramState
    = WaitingForInitialFiles
    | HavingRunReviewsPreviously
        { nextRuns : List Review.Run
        , availableErrorsOnReject :
            FastDict.Dict
                String
                (List
                    { range : Elm.Syntax.Range.Range
                    , message : String
                    , details : List String
                    , fixEditsByPath : FastDict.Dict String (List Review.SourceEdit)
                    }
                )
        , elmJson : { source : String, project : Elm.Project.Project }
        , filesByPath : FastDict.Dict String String
        }


{-| elm msg of a [`Review.Cli.program`](#program)
-}
type ProgramEvent
    = InitialFilesReceived
        { elmJson : { source : String, project : Elm.Project.Project }
        , directDependencies : List { elmJson : Elm.Project.Project, docsJson : List Elm.Docs.Module }
        , extraFiles : List { path : String, source : String }
        , modules : List { path : String, source : String }
        }
    | ModuleAddedOrChanged { path : String, source : String }
    | ModuleRemoved { path : String }
    | ExtraFileAddedOrChanged { path : String, source : String }
    | ExtraFileRemoved { path : String }
    | ErrorFixRejected
    | JsEventJsonFailedToDecode Json.Decode.Error


type alias FileReviewError =
    { range : Elm.Syntax.Range.Range
    , message : String
    , details : List String
    , fixEditsByPath : FastDict.Dict String (List Review.SourceEdit)
    }


eventToSendToJsToJson : EventToSendToJs -> Json.Encode.Value
eventToSendToJsToJson eventToBeSentToJs =
    case eventToBeSentToJs of
        InitialFilesRequested initialFileRequest ->
            Json.Encode.object
                [ ( "tag", "InitialFilesRequested" |> Json.Encode.string )
                , ( "value"
                  , Json.Encode.object
                        [ ( "extraPaths"
                          , initialFileRequest.extraPaths |> Json.Encode.list Json.Encode.string
                          )
                        ]
                  )
                ]

        ErrorsReceived errorsToSend ->
            Json.Encode.object
                [ ( "tag", "ErrorsReceived" |> Json.Encode.string )
                , ( "value"
                  , case errorsToSend.errors of
                        AllUnfixable allUnfixable ->
                            Json.Encode.object
                                [ ( "display"
                                  , allUnfixable
                                        |> FastDictLocalExtra.concatToListMap
                                            (\path errors ->
                                                let
                                                    pathSource : String
                                                    pathSource =
                                                        errorsToSend.projectFilesByPath
                                                            |> FastDict.get path
                                                            |> Maybe.withDefault ""
                                                in
                                                errors
                                                    |> List.map
                                                        (\error ->
                                                            { path = path
                                                            , range = error.range
                                                            , message = error.message
                                                            , details = error.details
                                                            , fixedSources = []
                                                            }
                                                                |> errorDisplay pathSource
                                                        )
                                            )
                                        |> String.join "\n\n\n"
                                        |> Json.Encode.string
                                  )
                                , ( "fix", Json.Encode.null )
                                ]

                        Fixable fixable ->
                            Json.Encode.object
                                [ ( "display", fixable.fixable |> errorDisplay fixable.fixable.source |> Json.Encode.string )
                                , ( "fix"
                                  , Json.Encode.object
                                        [ ( "path", fixable.fixable.path |> Json.Encode.string )
                                        , ( "fixedSources"
                                          , fixable.fixable.fixedSources
                                                |> Json.Encode.list
                                                    (\fileFix ->
                                                        Json.Encode.object
                                                            [ ( "path", fileFix.path |> Json.Encode.string )
                                                            , ( "source", fileFix.fixedSource |> Json.Encode.string )
                                                            ]
                                                    )
                                          )
                                        ]
                                  )
                                ]
                  )
                ]

        ProblemEncountered problem ->
            Json.Encode.object
                [ ( "tag", "ProblemEncountered" |> Json.Encode.string )
                , ( "value", problem |> Json.Encode.string )
                ]


errorDisplay :
    String
    ->
        { error_
            | path : String
            , range : Elm.Syntax.Range.Range
            , message : String
            , details : List String
            , fixedSources :
                List
                    { path : String
                    , fixedSource : String
                    , originalSource : String
                    }
        }
    -> String
errorDisplay source error =
    (error.message |> Ansi.cyan |> Ansi.bold)
        ++ "  in "
        ++ error.path
        ++ ":"
        ++ (error.range.start.row |> String.fromInt)
        ++ ":"
        ++ (error.range.start.column |> String.fromInt)
        ++ "\n\n"
        ++ (codeExtract source error.range
                |> String.split "\n"
                |> List.map (\line -> "|   " ++ line)
                |> String.join "\n"
           )
        ++ "\n\n"
        ++ (error.details |> String.join "\n\n")
        ++ (case error.fixedSources of
                [] ->
                    ""

                fixedSource0 :: fixedSource1Up ->
                    "\n\nI can fix this for you by changing\n"
                        ++ ((fixedSource0 :: fixedSource1Up)
                                |> List.map
                                    (\fileFixedSource ->
                                        "in "
                                            ++ fileFixedSource.path
                                            ++ "\n\n"
                                            ++ sourceHighlightDifferentLines
                                                fileFixedSource.originalSource
                                                fileFixedSource.fixedSource
                                    )
                                |> String.join "\n\n"
                           )
           )


sourceHighlightDifferentLines : String -> String -> String
sourceHighlightDifferentLines aString bString =
    let
        highlightedAndFinalNoChangeLines :
            { continuousNoChangeLinesReversed : List String
            , highlighted : String
            }
        highlightedAndFinalNoChangeLines =
            Diff.diff (aString |> String.lines) (bString |> String.lines)
                |> List.foldl
                    (\change soFar ->
                        case change of
                            Diff.NoChange line ->
                                { soFar
                                    | continuousNoChangeLinesReversed =
                                        line :: soFar.continuousNoChangeLinesReversed
                                }

                            Diff.Added line ->
                                { continuousNoChangeLinesReversed = []
                                , highlighted =
                                    soFar.highlighted
                                        ++ (soFar.continuousNoChangeLinesReversed
                                                |> List.reverse
                                                |> continuousNoChangeLinesCollapse
                                           )
                                        ++ "\n"
                                        ++ Ansi.green ("|   " ++ line)
                                }

                            Diff.Removed line ->
                                { continuousNoChangeLinesReversed = []
                                , highlighted =
                                    soFar.highlighted
                                        ++ (soFar.continuousNoChangeLinesReversed
                                                |> List.reverse
                                                |> continuousNoChangeLinesCollapse
                                           )
                                        ++ "\n"
                                        ++ Ansi.red ("|   " ++ line)
                                }
                    )
                    { continuousNoChangeLinesReversed = [], highlighted = "" }
    in
    case highlightedAndFinalNoChangeLines.continuousNoChangeLinesReversed |> ListLocalExtra.last of
        Nothing ->
            highlightedAndFinalNoChangeLines.highlighted

        Just nextNoChangeLine ->
            highlightedAndFinalNoChangeLines.highlighted
                ++ "\n|   "
                ++ nextNoChangeLine


continuousNoChangeLinesCollapse : List String -> String
continuousNoChangeLinesCollapse noChangeLines =
    case noChangeLines of
        [] ->
            ""

        [ onlyNoChangeLine ] ->
            "|   " ++ onlyNoChangeLine

        [ noChangeLine0, noChangeLine1 ] ->
            "|   "
                ++ noChangeLine0
                ++ "\n"
                ++ "|   "
                ++ noChangeLine1

        noChangeLine0 :: _ :: noChangeLine2 :: noChangeLine3Up ->
            "|   "
                ++ noChangeLine0
                ++ "\n⋮\n"
                ++ "|   "
                ++ (noChangeLine3Up |> ListLocalExtra.last |> Maybe.withDefault noChangeLine2)


codeExtract : String -> Elm.Syntax.Range.Range -> String
codeExtract source range =
    let
        sourceLines : Array String
        sourceLines =
            source |> String.lines |> Array.fromList

        sourceLineAtRow : Int -> String
        sourceLineAtRow rowIndex =
            case sourceLines |> Array.get rowIndex of
                Just line ->
                    line |> String.trimRight

                Nothing ->
                    ""
    in
    if range.start.row == range.end.row then
        if range.start.column == range.end.column then
            ""

        else
            [ sourceLineAtRow (range.start.row - 2)
            , sourceLineAtRow (range.start.row - 1)
                |> withErrorHighlightedRange
                    { start = range.start.column, end = range.end.column }
            , sourceLineAtRow range.end.row
            ]
                |> List.filter (\l -> not (l |> String.isEmpty))
                |> String.join "\n"

    else
        [ [ sourceLineAtRow (range.start.row - 2)
          , let
                startLineContent : String
                startLineContent =
                    sourceLineAtRow (range.start.row - 1)
            in
            startLineContent
                |> withErrorHighlightedRange
                    { start = range.start.column
                    , end = (startLineContent |> Unicode.length) + 1
                    }
          ]
            |> List.filter (\l -> not (l |> String.isEmpty))
            |> String.join "\n"
        , List.range range.start.row (range.end.row - 2)
            |> List.map
                (\middleLine ->
                    sourceLineAtRow middleLine |> Ansi.backgroundRed
                )
            |> String.join "\n"
        , [ let
                endLineContent : String
                endLineContent =
                    sourceLineAtRow (range.end.row - 1)
            in
            endLineContent
                |> withErrorHighlightedRange
                    { start = getIndexOfFirstNonSpace endLineContent + 1
                    , end = range.end.column
                    }
          , sourceLineAtRow range.end.row
          ]
            |> List.filter (\l -> not (l |> String.isEmpty))
            |> String.join "\n"
        ]
            |> String.join "\n"


getIndexOfFirstNonSpace : String -> Int
getIndexOfFirstNonSpace string =
    (string |> String.length) - (string |> String.trimLeft |> String.length)


withErrorHighlightedRange : { start : Int, end : Int } -> (String -> String)
withErrorHighlightedRange lineRange lineContent =
    (lineContent |> Unicode.left (lineRange.start - 1))
        ++ (lineContent
                |> Unicode.dropLeft (lineRange.start - 1)
                |> Unicode.left (lineRange.end - lineRange.start)
                |> Ansi.backgroundRed
           )
        ++ (lineContent
                |> Unicode.dropLeft
                    (lineRange.start - 1 + lineRange.end - lineRange.start)
           )


{-| Run reviews, re-running on file changes.
To be used from a node.js CLI program
as showcased in [elm-review-mini-cli-starter](https://github.com/lue-bird/elm-review-mini-cli-starter)
-}
program :
    { configuration : { reviews : List Review.Review, extraPaths : List String }
    , toJs : Json.Encode.Value -> Cmd Never
    , fromJs : (Json.Encode.Value -> ProgramEvent) -> Sub ProgramEvent
    }
    -> Program
program config =
    Platform.worker
        { init = \() -> initialStateAndCommand config
        , update =
            \event state ->
                let
                    ( newState, elmEventsToSendToJs ) =
                        state |> reactToEvent config event
                in
                ( newState
                , elmEventsToSendToJs
                    |> List.map
                        (\elmEventToSendToJs ->
                            elmEventToSendToJs
                                |> eventToSendToJsToJson
                                |> config.toJs
                                |> Cmd.map Basics.never
                        )
                    |> Cmd.batch
                )
        , subscriptions = \state -> state |> listen config
        }


initialStateAndCommand :
    { configuration : { reviews : List Review.Review, extraPaths : List String }
    , toJs : Json.Encode.Value -> Cmd Never
    , fromJs : (Json.Encode.Value -> ProgramEvent) -> Sub ProgramEvent
    }
    -> ( ProgramState, Cmd never_ )
initialStateAndCommand config =
    ( WaitingForInitialFiles
    , InitialFilesRequested { extraPaths = config.configuration.extraPaths }
        |> eventToSendToJsToJson
        |> config.toJs
        |> Cmd.map Basics.never
    )


failedToParsePathsMessage : List String -> String
failedToParsePathsMessage pathsThatFailedToParse =
    case pathsThatFailedToParse of
        [ onlyPathThatFailedToParse ] ->
            "module at path "
                ++ onlyPathThatFailedToParse
                ++ " failed to parse"

        nonSinglePathsThatFailedToParse ->
            "modules at paths "
                ++ (nonSinglePathsThatFailedToParse |> String.join " and ")
                ++ " failed to parse"


reactToEvent :
    { configuration : { reviews : List Review.Review, extraPaths : List String }
    , toJs : Json.Encode.Value -> Cmd Never
    , fromJs : (Json.Encode.Value -> ProgramEvent) -> Sub ProgramEvent
    }
    -> ProgramEvent
    -> (ProgramState -> ( ProgramState, List EventToSendToJs ))
reactToEvent config event state =
    case event of
        InitialFilesReceived initialFiles ->
            case initialFiles.modules |> allModuleFilesToWithParsedSyntax of
                Err pathsThatFailedToParse ->
                    ( state
                    , [ ProblemEncountered
                            (failedToParsePathsMessage pathsThatFailedToParse)
                      ]
                    )

                Ok modules ->
                    let
                        initialFilesByPath : FastDict.Dict String String
                        initialFilesByPath =
                            FastDict.union
                                (initialFiles.extraFiles
                                    |> FastDictLocalExtra.fromListMap
                                        (\file -> { key = file.path, value = file.source })
                                )
                                (initialFiles.modules
                                    |> FastDictLocalExtra.fromListMap
                                        (\file -> { key = file.path, value = file.source })
                                )

                        runResult :
                            { errorsByPath : FastDict.Dict String (List FileReviewError)
                            , nextRuns : List Review.Run
                            }
                        runResult =
                            { addedOrChangedModules = modules
                            , addedOrChangedExtraFiles = initialFiles.extraFiles
                            , directDependencies = initialFiles.directDependencies
                            , elmJson = initialFiles.elmJson
                            , removedExtraFilePaths = []
                            , removedModulePaths = []
                            }
                                |> reviewRunList config.configuration.reviews

                        maybeNextFixableErrorOrAllUnfixable : Maybe NextFixableOrAllUnfixable
                        maybeNextFixableErrorOrAllUnfixable =
                            runResult.errorsByPath
                                |> errorsByPathToNextFixableErrorOrAll
                                    { elmJsonSource = initialFiles.elmJson.source
                                    , filesByPath = initialFilesByPath
                                    }
                    in
                    ( HavingRunReviewsPreviously
                        { nextRuns = runResult.nextRuns
                        , availableErrorsOnReject =
                            case maybeNextFixableErrorOrAllUnfixable of
                                Nothing ->
                                    FastDict.empty

                                Just (AllUnfixable _) ->
                                    FastDict.empty

                                Just (Fixable nextFixable) ->
                                    nextFixable.otherErrors
                        , elmJson = initialFiles.elmJson
                        , filesByPath = initialFilesByPath
                        }
                    , case maybeNextFixableErrorOrAllUnfixable of
                        Nothing ->
                            []

                        Just nextFixableErrorOrAllUnfixable ->
                            [ ErrorsReceived
                                { projectFilesByPath =
                                    initialFilesByPath
                                        |> FastDict.insert "elm.json" initialFiles.elmJson.source
                                , errors = nextFixableErrorOrAllUnfixable
                                }
                            ]
                    )

        ErrorFixRejected ->
            case state of
                HavingRunReviewsPreviously havingRunReviewsPreviously ->
                    let
                        maybeNextFixableErrorOrAllUnfixable : Maybe NextFixableOrAllUnfixable
                        maybeNextFixableErrorOrAllUnfixable =
                            havingRunReviewsPreviously.availableErrorsOnReject
                                |> errorsByPathToNextFixableErrorOrAll
                                    { elmJsonSource = havingRunReviewsPreviously.elmJson.source
                                    , filesByPath = havingRunReviewsPreviously.filesByPath
                                    }
                    in
                    ( HavingRunReviewsPreviously
                        { havingRunReviewsPreviously
                            | availableErrorsOnReject =
                                case maybeNextFixableErrorOrAllUnfixable of
                                    Nothing ->
                                        FastDict.empty

                                    Just (AllUnfixable _) ->
                                        FastDict.empty

                                    Just (Fixable nextFixable) ->
                                        nextFixable.otherErrors
                        }
                    , case maybeNextFixableErrorOrAllUnfixable of
                        Nothing ->
                            []

                        Just nextFixableErrorOrAllUnfixable ->
                            [ ErrorsReceived
                                { projectFilesByPath =
                                    havingRunReviewsPreviously.filesByPath
                                        |> FastDict.insert "elm.json" havingRunReviewsPreviously.elmJson.source
                                , errors = nextFixableErrorOrAllUnfixable
                                }
                            ]
                    )

                unexpectedState ->
                    ( unexpectedState, [] )

        ModuleAddedOrChanged moduleAddedOrChangedPathAndSource ->
            case state of
                HavingRunReviewsPreviously havingRunReviewsPreviously ->
                    case moduleAddedOrChangedPathAndSource |> moduleFileToWithParsedSyntax of
                        Err () ->
                            ( HavingRunReviewsPreviously havingRunReviewsPreviously
                            , [ ProblemEncountered
                                    (failedToParsePathsMessage [ moduleAddedOrChangedPathAndSource.path ])
                              ]
                            )

                        Ok moduleAddedOrChanged ->
                            let
                                filesByPathWithAddedOrChanged : FastDict.Dict String String
                                filesByPathWithAddedOrChanged =
                                    havingRunReviewsPreviously.filesByPath
                                        |> FastDict.insert moduleAddedOrChanged.path moduleAddedOrChanged.source

                                runResult :
                                    { errorsByPath : FastDict.Dict String (List FileReviewError)
                                    , nextRuns : List Review.Run
                                    }
                                runResult =
                                    { addedOrChangedExtraFiles = []
                                    , addedOrChangedModules = [ moduleAddedOrChanged ]
                                    , directDependencies = []
                                    , elmJson = havingRunReviewsPreviously.elmJson
                                    , removedExtraFilePaths = []
                                    , removedModulePaths = []
                                    }
                                        |> reviewRunList
                                            (List.map2 reviewWithRun
                                                havingRunReviewsPreviously.nextRuns
                                                config.configuration.reviews
                                            )

                                maybeNextFixableErrorOrAllUnfixable : Maybe NextFixableOrAllUnfixable
                                maybeNextFixableErrorOrAllUnfixable =
                                    runResult.errorsByPath
                                        |> errorsByPathToNextFixableErrorOrAll
                                            { elmJsonSource = havingRunReviewsPreviously.elmJson.source
                                            , filesByPath = filesByPathWithAddedOrChanged
                                            }
                            in
                            ( HavingRunReviewsPreviously
                                { nextRuns = runResult.nextRuns
                                , availableErrorsOnReject =
                                    case maybeNextFixableErrorOrAllUnfixable of
                                        Nothing ->
                                            FastDict.empty

                                        Just (AllUnfixable _) ->
                                            FastDict.empty

                                        Just (Fixable nextFixable) ->
                                            nextFixable.otherErrors
                                , elmJson = havingRunReviewsPreviously.elmJson
                                , filesByPath = filesByPathWithAddedOrChanged
                                }
                            , case maybeNextFixableErrorOrAllUnfixable of
                                Nothing ->
                                    []

                                Just nextFixableErrorOrAllUnfixable ->
                                    [ ErrorsReceived
                                        { projectFilesByPath =
                                            filesByPathWithAddedOrChanged
                                                |> FastDict.insert "elm.json" havingRunReviewsPreviously.elmJson.source
                                        , errors = nextFixableErrorOrAllUnfixable
                                        }
                                    ]
                            )

                unexpectedState ->
                    ( unexpectedState, [] )

        ModuleRemoved moduleRemoved ->
            case state of
                HavingRunReviewsPreviously havingRunReviewsPreviously ->
                    let
                        filesByPathWithRemoved : FastDict.Dict String String
                        filesByPathWithRemoved =
                            havingRunReviewsPreviously.filesByPath
                                |> FastDict.remove moduleRemoved.path

                        runResult :
                            { errorsByPath : FastDict.Dict String (List FileReviewError)
                            , nextRuns : List Review.Run
                            }
                        runResult =
                            { addedOrChangedExtraFiles = []
                            , addedOrChangedModules = []
                            , directDependencies = []
                            , elmJson = havingRunReviewsPreviously.elmJson
                            , removedExtraFilePaths = []
                            , removedModulePaths = [ moduleRemoved.path ]
                            }
                                |> reviewRunList
                                    (List.map2 reviewWithRun
                                        havingRunReviewsPreviously.nextRuns
                                        config.configuration.reviews
                                    )

                        maybeNextFixableErrorOrAllUnfixable : Maybe NextFixableOrAllUnfixable
                        maybeNextFixableErrorOrAllUnfixable =
                            runResult.errorsByPath
                                |> errorsByPathToNextFixableErrorOrAll
                                    { elmJsonSource = havingRunReviewsPreviously.elmJson.source
                                    , filesByPath = filesByPathWithRemoved
                                    }
                    in
                    ( HavingRunReviewsPreviously
                        { nextRuns = runResult.nextRuns
                        , availableErrorsOnReject =
                            case maybeNextFixableErrorOrAllUnfixable of
                                Nothing ->
                                    FastDict.empty

                                Just (AllUnfixable _) ->
                                    FastDict.empty

                                Just (Fixable nextFixable) ->
                                    nextFixable.otherErrors
                        , elmJson = havingRunReviewsPreviously.elmJson
                        , filesByPath = filesByPathWithRemoved
                        }
                    , case maybeNextFixableErrorOrAllUnfixable of
                        Nothing ->
                            []

                        Just nextFixableErrorOrAllUnfixable ->
                            [ ErrorsReceived
                                { projectFilesByPath =
                                    filesByPathWithRemoved
                                        |> FastDict.insert "elm.json" havingRunReviewsPreviously.elmJson.source
                                , errors = nextFixableErrorOrAllUnfixable
                                }
                            ]
                    )

                unexpectedState ->
                    ( unexpectedState, [] )

        ExtraFileAddedOrChanged fileAddedOrChanged ->
            case state of
                HavingRunReviewsPreviously havingRunReviewsPreviously ->
                    let
                        filesByPathWithAddedOrChanged : FastDict.Dict String String
                        filesByPathWithAddedOrChanged =
                            havingRunReviewsPreviously.filesByPath
                                |> FastDict.insert fileAddedOrChanged.path fileAddedOrChanged.source

                        runResult :
                            { errorsByPath : FastDict.Dict String (List FileReviewError)
                            , nextRuns : List Review.Run
                            }
                        runResult =
                            { addedOrChangedExtraFiles = [ fileAddedOrChanged ]
                            , addedOrChangedModules = []
                            , directDependencies = []
                            , elmJson = havingRunReviewsPreviously.elmJson
                            , removedExtraFilePaths = []
                            , removedModulePaths = []
                            }
                                |> reviewRunList
                                    (List.map2 reviewWithRun
                                        havingRunReviewsPreviously.nextRuns
                                        config.configuration.reviews
                                    )

                        maybeNextFixableErrorOrAllUnfixable : Maybe NextFixableOrAllUnfixable
                        maybeNextFixableErrorOrAllUnfixable =
                            runResult.errorsByPath
                                |> errorsByPathToNextFixableErrorOrAll
                                    { elmJsonSource = havingRunReviewsPreviously.elmJson.source
                                    , filesByPath = filesByPathWithAddedOrChanged
                                    }
                    in
                    ( HavingRunReviewsPreviously
                        { nextRuns = runResult.nextRuns
                        , availableErrorsOnReject =
                            case maybeNextFixableErrorOrAllUnfixable of
                                Nothing ->
                                    FastDict.empty

                                Just (AllUnfixable _) ->
                                    FastDict.empty

                                Just (Fixable nextFixable) ->
                                    nextFixable.otherErrors
                        , elmJson = havingRunReviewsPreviously.elmJson
                        , filesByPath = filesByPathWithAddedOrChanged
                        }
                    , case maybeNextFixableErrorOrAllUnfixable of
                        Nothing ->
                            []

                        Just nextFixableErrorOrAllUnfixable ->
                            [ ErrorsReceived
                                { projectFilesByPath =
                                    filesByPathWithAddedOrChanged
                                        |> FastDict.insert "elm.json" havingRunReviewsPreviously.elmJson.source
                                , errors = nextFixableErrorOrAllUnfixable
                                }
                            ]
                    )

                unexpectedState ->
                    ( unexpectedState, [] )

        ExtraFileRemoved fileRemoved ->
            case state of
                HavingRunReviewsPreviously havingRunReviewsPreviously ->
                    let
                        filesByPathWithRemoved : FastDict.Dict String String
                        filesByPathWithRemoved =
                            havingRunReviewsPreviously.filesByPath
                                |> FastDict.remove fileRemoved.path

                        runResult :
                            { errorsByPath : FastDict.Dict String (List FileReviewError)
                            , nextRuns : List Review.Run
                            }
                        runResult =
                            { addedOrChangedExtraFiles = []
                            , addedOrChangedModules = []
                            , directDependencies = []
                            , elmJson = havingRunReviewsPreviously.elmJson
                            , removedExtraFilePaths = [ fileRemoved.path ]
                            , removedModulePaths = []
                            }
                                |> reviewRunList
                                    (List.map2 reviewWithRun
                                        havingRunReviewsPreviously.nextRuns
                                        config.configuration.reviews
                                    )

                        maybeNextFixableErrorOrAllUnfixable : Maybe NextFixableOrAllUnfixable
                        maybeNextFixableErrorOrAllUnfixable =
                            runResult.errorsByPath
                                |> errorsByPathToNextFixableErrorOrAll
                                    { elmJsonSource = havingRunReviewsPreviously.elmJson.source
                                    , filesByPath = filesByPathWithRemoved
                                    }
                    in
                    ( HavingRunReviewsPreviously
                        { nextRuns = runResult.nextRuns
                        , availableErrorsOnReject =
                            case maybeNextFixableErrorOrAllUnfixable of
                                Nothing ->
                                    FastDict.empty

                                Just (AllUnfixable _) ->
                                    FastDict.empty

                                Just (Fixable nextFixable) ->
                                    nextFixable.otherErrors
                        , elmJson = havingRunReviewsPreviously.elmJson
                        , filesByPath = filesByPathWithRemoved
                        }
                    , case maybeNextFixableErrorOrAllUnfixable of
                        Nothing ->
                            []

                        Just nextFixableErrorOrAllUnfixable ->
                            [ ErrorsReceived
                                { projectFilesByPath =
                                    filesByPathWithRemoved
                                        |> FastDict.insert "elm.json" havingRunReviewsPreviously.elmJson.source
                                , errors = nextFixableErrorOrAllUnfixable
                                }
                            ]
                    )

                unexpectedState ->
                    ( unexpectedState, [] )

        JsEventJsonFailedToDecode jsonDecodeError ->
            ( state
            , [ ProblemEncountered
                    ("bug: failed to decode json event from the js CLI"
                        ++ (jsonDecodeError |> Json.Decode.errorToString)
                    )
              ]
            )


reviewWithRun : Review.Run -> (Review -> Review)
reviewWithRun nextRun review =
    { ignoreErrorsForPathsWhere = review.ignoreErrorsForPathsWhere
    , run = nextRun
    }


reviewRunList :
    List Review
    ->
        { elmJson : { source : String, project : Elm.Project.Project }
        , directDependencies : List { elmJson : Elm.Project.Project, docsJson : List Elm.Docs.Module }
        , addedOrChangedExtraFiles : List { path : String, source : String }
        , addedOrChangedModules : List { path : String, source : String, syntax : Elm.Syntax.File.File }
        , removedExtraFilePaths : List String
        , removedModulePaths : List String
        }
    ->
        { errorsByPath : FastDict.Dict String (List FileReviewError)
        , nextRuns : List Review.Run
        }
reviewRunList reviews project =
    let
        runResultsForReviews :
            List
                { errorsByPath : FastDict.Dict String (List FileReviewError)
                , nextRun : Review.Run
                }
        runResultsForReviews =
            reviews |> List.map (\review -> project |> Review.run review)
    in
    { errorsByPath =
        runResultsForReviews
            |> List.foldl
                (\runResultForReview soFar ->
                    FastDictLocalExtra.unionWith (\new already -> new ++ already)
                        runResultForReview.errorsByPath
                        soFar
                )
                FastDict.empty
            |> FastDict.map
                (\_ errors ->
                    errors |> List.sortWith (\a b -> Elm.Syntax.Range.compare a.range b.range)
                )
    , nextRuns = runResultsForReviews |> List.map .nextRun
    }


errorsByPathToNextFixableErrorOrAll :
    { elmJsonSource : String, filesByPath : FastDict.Dict String String }
    -> FastDict.Dict String (List FileReviewError)
    -> Maybe NextFixableOrAllUnfixable
errorsByPathToNextFixableErrorOrAll project errorsByPath =
    let
        sourceAtPath : String -> Maybe String
        sourceAtPath path =
            case path of
                "elm.json" ->
                    project.elmJsonSource |> Just

                filePath ->
                    project.filesByPath |> FastDict.get filePath

        nextFixableErrorOrAll :
            { fixable :
                Maybe
                    { source : String
                    , fixedSources : List { path : String, fixedSource : String, originalSource : String }
                    , message : String
                    , details : List String
                    , range : Elm.Syntax.Range.Range
                    , path : String
                    }
            , otherErrors : FastDict.Dict String (List FileReviewError)
            }
        nextFixableErrorOrAll =
            errorsByPath
                |> FastDict.foldl
                    (\path errors soFar ->
                        let
                            errorsFixableErrorOrAll :
                                { fixable :
                                    Maybe
                                        { source : String
                                        , fixedSources : List { path : String, fixedSource : String, originalSource : String }
                                        , message : String
                                        , details : List String
                                        , range : Elm.Syntax.Range.Range
                                        , path : String
                                        }
                                , otherErrors : List FileReviewError
                                }
                            errorsFixableErrorOrAll =
                                case soFar.fixable of
                                    Just soFarFix ->
                                        { fixable = soFarFix |> Just, otherErrors = errors }

                                    Nothing ->
                                        case sourceAtPath path of
                                            Nothing ->
                                                { fixable = Nothing, otherErrors = [] }

                                            Just pathSource ->
                                                errors
                                                    |> List.foldl
                                                        (\error errorsResultSoFar ->
                                                            let
                                                                fixedSources : List { path : String, fixedSource : String, originalSource : String }
                                                                fixedSources =
                                                                    error.fixEditsByPath
                                                                        |> FastDict.foldl
                                                                            (\fixPath fileEdits soFarFixedSources ->
                                                                                case sourceAtPath fixPath of
                                                                                    Nothing ->
                                                                                        soFarFixedSources

                                                                                    Just sourceToFix ->
                                                                                        case sourceToFix |> Review.sourceApplyEdits fileEdits of
                                                                                            Ok fixedSource ->
                                                                                                { path = fixPath
                                                                                                , fixedSource = fixedSource
                                                                                                , originalSource = sourceToFix
                                                                                                }
                                                                                                    :: soFarFixedSources

                                                                                            Err _ ->
                                                                                                soFarFixedSources
                                                                            )
                                                                            []
                                                            in
                                                            case fixedSources of
                                                                [] ->
                                                                    { fixable = Nothing
                                                                    , otherErrors = error :: errorsResultSoFar.otherErrors
                                                                    }

                                                                fixedSource0 :: fixedSource1Up ->
                                                                    { fixable =
                                                                        { source = pathSource
                                                                        , fixedSources = fixedSource0 :: fixedSource1Up
                                                                        , message = error.message
                                                                        , details = error.details
                                                                        , range = error.range
                                                                        , path = path
                                                                        }
                                                                            |> Just
                                                                    , otherErrors = errorsResultSoFar.otherErrors
                                                                    }
                                                        )
                                                        { fixable = Nothing
                                                        , otherErrors = []
                                                        }
                        in
                        { fixable = errorsFixableErrorOrAll.fixable
                        , otherErrors = soFar.otherErrors |> FastDict.insert path errorsFixableErrorOrAll.otherErrors
                        }
                    )
                    { fixable = Nothing
                    , otherErrors = FastDict.empty
                    }
    in
    case ( nextFixableErrorOrAll.fixable, nextFixableErrorOrAll.otherErrors |> FastDictLocalExtra.all (\_ errors -> errors |> List.isEmpty) ) of
        ( Nothing, True ) ->
            Nothing

        ( Just fixable, _ ) ->
            Fixable { fixable = fixable, otherErrors = nextFixableErrorOrAll.otherErrors }
                |> Just

        ( Nothing, False ) ->
            AllUnfixable nextFixableErrorOrAll.otherErrors
                |> Just


moduleFileToWithParsedSyntax : { source : String, path : String } -> Result () { path : String, source : String, syntax : Elm.Syntax.File.File }
moduleFileToWithParsedSyntax moduleFile =
    case moduleFile.source |> Elm.Parser.parseToFile of
        Err _ ->
            Err ()

        Ok syntax ->
            { path = moduleFile.path, source = moduleFile.source, syntax = syntax }
                |> Ok


allModuleFilesToWithParsedSyntax :
    List { source : String, path : String }
    -> Result (List String) (List { path : String, source : String, syntax : Elm.Syntax.File.File })
allModuleFilesToWithParsedSyntax moduleFiles =
    moduleFiles
        |> List.foldl
            (\moduleFile soFar ->
                case moduleFile.source |> Elm.Parser.parseToFile of
                    Err _ ->
                        case soFar of
                            Ok _ ->
                                Err [ moduleFile.path ]

                            Err soFarErrorPaths ->
                                Err (moduleFile.path :: soFarErrorPaths)

                    Ok syntax ->
                        case soFar of
                            Err soFarErrorPaths ->
                                Err soFarErrorPaths

                            Ok soFarWithParsedSyntax ->
                                { path = moduleFile.path, source = moduleFile.source, syntax = syntax }
                                    :: soFarWithParsedSyntax
                                    |> Ok
            )
            ([] |> Ok)


listen :
    { configuration : { reviews : List Review.Review, extraPaths : List String }
    , toJs : Json.Encode.Value -> Cmd Never
    , fromJs : (Json.Encode.Value -> ProgramEvent) -> Sub ProgramEvent
    }
    -> (ProgramState -> Sub ProgramEvent)
listen config _ =
    config.fromJs
        (\fromJs ->
            case fromJs |> Json.Decode.decodeValue eventJsonDecoder of
                Err decodeError ->
                    JsEventJsonFailedToDecode decodeError

                Ok event ->
                    event
        )


eventJsonDecoder : Json.Decode.Decoder ProgramEvent
eventJsonDecoder =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\tag ->
                Json.Decode.field "value"
                    (case tag of
                        "InitialFilesReceived" ->
                            Json.Decode.map4
                                (\elmJson directDependencies modules extraFiles ->
                                    InitialFilesReceived
                                        { elmJson = elmJson
                                        , directDependencies = directDependencies
                                        , modules = modules
                                        , extraFiles = extraFiles
                                        }
                                )
                                (Json.Decode.field "elmJson"
                                    (Json.Decode.map2 (\source project -> { source = source, project = project })
                                        (Json.Decode.field "source" Json.Decode.string)
                                        (Json.Decode.field "project" Elm.Project.decoder)
                                    )
                                )
                                (Json.Decode.field "directDependencies"
                                    (Json.Decode.list
                                        (Json.Decode.map2
                                            (\elmJson docsJson -> { elmJson = elmJson, docsJson = docsJson })
                                            (Json.Decode.field "elmJson" Elm.Project.decoder)
                                            (Json.Decode.field "docsJson" (Json.Decode.list Elm.Docs.decoder))
                                        )
                                    )
                                )
                                (Json.Decode.field "modules"
                                    (Json.Decode.list
                                        (Json.Decode.map2
                                            (\path source -> { path = path, source = source })
                                            (Json.Decode.field "path" Json.Decode.string)
                                            (Json.Decode.field "source" Json.Decode.string)
                                        )
                                    )
                                )
                                (Json.Decode.field "extraFiles"
                                    (Json.Decode.list
                                        (Json.Decode.map2
                                            (\path source -> { path = path, source = source })
                                            (Json.Decode.field "path" Json.Decode.string)
                                            (Json.Decode.field "source" Json.Decode.string)
                                        )
                                    )
                                )

                        "ModuleAddedOrChanged" ->
                            Json.Decode.map ModuleAddedOrChanged
                                (Json.Decode.map2
                                    (\path source -> { path = path, source = source })
                                    (Json.Decode.field "path" Json.Decode.string)
                                    (Json.Decode.field "source" Json.Decode.string)
                                )

                        "ModuleRemoved" ->
                            Json.Decode.map
                                (\path -> ModuleRemoved { path = path })
                                (Json.Decode.field "path" Json.Decode.string)

                        "ExtraFileAddedOrChanged" ->
                            Json.Decode.map2
                                (\path source -> ExtraFileAddedOrChanged { path = path, source = source })
                                (Json.Decode.field "path" Json.Decode.string)
                                (Json.Decode.field "source" Json.Decode.string)

                        "ExtraFileRemoved" ->
                            Json.Decode.map
                                (\path -> ExtraFileRemoved { path = path })
                                (Json.Decode.field "path" Json.Decode.string)

                        "ErrorFixRejected" ->
                            Json.Decode.map (\() -> ErrorFixRejected)
                                (Json.Decode.null ())

                        unknownTag ->
                            Json.Decode.fail ("unknown js message tag " ++ unknownTag)
                    )
            )


type EventToSendToJs
    = InitialFilesRequested { extraPaths : List String }
    | ErrorsReceived
        { projectFilesByPath : FastDict.Dict String String
        , errors : NextFixableOrAllUnfixable
        }
    | ProblemEncountered String


type NextFixableOrAllUnfixable
    = AllUnfixable (FastDict.Dict String (List FileReviewError))
    | Fixable
        { fixable :
            { source : String
            , fixedSources : List { path : String, fixedSource : String, originalSource : String }
            , message : String
            , details : List String
            , range : Elm.Syntax.Range.Range
            , path : String
            }
        , otherErrors : FastDict.Dict String (List FileReviewError)
        }
