module Review.Cli exposing (program, Program, ProgramEvent(..), ProgramState(..))

{-| Run a review in the terminal

@docs program, Program, ProgramEvent, ProgramState

-}

import Array exposing (Array)
import Elm.Project
import Elm.Syntax.Range
import FastDict
import FastDictExtra
import Json.Decode
import Json.Encode
import Review exposing (Review)


{-| An elm worker produced by [`Review.Cli.Program`](Review-Cli#Program)
-}
type alias Program =
    Platform.Program () ProgramState ProgramEvent


{-| elm model of a [`Review.program`](#program)
-}
type ProgramState
    = WaitingForInitialFiles
    | HavingRunReviewsPreviously
        { cache : Review.Cache
        , availableErrorsOnReject :
            List
                { path : String
                , range : Elm.Syntax.Range.Range
                , message : String
                , details : List String
                , fix : List Review.Fix
                }
        , elmJsonSource : String
        , filesByPath : FastDict.Dict String String
        }


{-| elm msg of a [`Review.Cli.program`](#program)
-}
type ProgramEvent
    = InitialFilesReceived
        { elmJson : { source : String, project : Elm.Project.Project }
        , directDependencies : List { elmJson : String, docsJson : String }
        , files : List { path : String, source : String }
        }
    | FileAddedOrChanged { path : String, source : String }
    | FileRemoved { path : String }
    | ErrorFixRejected
    | JsonDecodingFailed Json.Decode.Error


eventJsonDecoder : Json.Decode.Decoder ProgramEvent
eventJsonDecoder =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\tag ->
                Json.Decode.field "value"
                    (case tag of
                        "InitialFilesReceived" ->
                            Json.Decode.map3
                                (\elmJson directDependencies files ->
                                    InitialFilesReceived
                                        { elmJson = elmJson
                                        , directDependencies = directDependencies
                                        , files = files
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
                                            (Json.Decode.field "elmJson" Json.Decode.string)
                                            (Json.Decode.field "docsJson" Json.Decode.string)
                                        )
                                    )
                                )
                                (Json.Decode.field "files"
                                    (Json.Decode.list
                                        (Json.Decode.map2
                                            (\path source -> { path = path, source = source })
                                            (Json.Decode.field "path" Json.Decode.string)
                                            (Json.Decode.field "source" Json.Decode.string)
                                        )
                                    )
                                )

                        "FileAddedOrChanged" ->
                            Json.Decode.map2
                                (\path source -> FileAddedOrChanged { path = path, source = source })
                                (Json.Decode.field "path" Json.Decode.string)
                                (Json.Decode.field "source" Json.Decode.string)

                        "FileRemoved" ->
                            Json.Decode.map
                                (\path -> FileRemoved { path = path })
                                (Json.Decode.field "path" Json.Decode.string)

                        "ErrorFixRejected" ->
                            Json.Decode.map (\() -> ErrorFixRejected)
                                (Json.Decode.null ())

                        unknownTag ->
                            Json.Decode.fail ("unknown js message tag " ++ unknownTag)
                    )
            )


{-| A node.js CLI program.
Start initializing one as showcased in [elm-review-mini-cli-starter](https://github.com/lue-bird/elm-review-mini-cli-starter)
-}
program :
    { toJs : Json.Encode.Value -> Cmd Never
    , fromJs : (Json.Encode.Value -> ProgramEvent) -> Sub ProgramEvent
    , configuration : { reviews : List Review.Review, extraPaths : List String }
    }
    -> Program
program config =
    Platform.worker
        { init =
            \() ->
                ( WaitingForInitialFiles
                , Json.Encode.object
                    [ ( "tag", "InitialFilesRequested" |> Json.Encode.string )
                    , ( "value"
                      , Json.Encode.object
                            [ ( "extraPaths", config.configuration.extraPaths |> Json.Encode.list Json.Encode.string ) ]
                      )
                    ]
                    |> config.toJs
                    |> Cmd.map Basics.never
                )
        , update =
            \event state ->
                case event of
                    InitialFilesReceived initialFiles ->
                        let
                            initialFilesByPath : FastDict.Dict String String
                            initialFilesByPath =
                                initialFiles.files
                                    |> FastDictExtra.fromListMap (\file -> { key = file.path, value = file.source })

                            run :
                                Review
                                -> Review.Cache
                                ->
                                    { errorsByPath :
                                        List
                                            { path : String
                                            , range : Elm.Syntax.Range.Range
                                            , message : String
                                            , details : List String
                                            , fix : List Review.Fix
                                            }
                                    , cache : Review.Cache
                                    }
                            run review cache =
                                let
                                    runResultForReview :
                                        { errorsByPath :
                                            FastDict.Dict
                                                String
                                                (List
                                                    { range : Elm.Syntax.Range.Range
                                                    , message : String
                                                    , details : List String
                                                    , fix : List Review.Fix
                                                    }
                                                )
                                        , cache : Review.Cache
                                        }
                                    runResultForReview =
                                        { addedOrChangedFiles = initialFiles.files
                                        , directDependencies = initialFiles.directDependencies
                                        , elmJson = initialFiles.elmJson
                                        , removedFilePaths = []
                                        , cache = cache
                                        }
                                            |> Review.run review
                                in
                                { cache = runResultForReview.cache
                                , errorsByPath =
                                    runResultForReview.errorsByPath
                                        |> FastDictExtra.concatToListMap
                                            (\path errors ->
                                                errors
                                                    |> List.map
                                                        (\error ->
                                                            { path = path
                                                            , range = error.range
                                                            , message = error.message
                                                            , details = error.details
                                                            , fix = error.fix
                                                            }
                                                        )
                                            )
                                }

                            runResult :
                                { errorsByPath : List { path : String, range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix }
                                , cache : Review.Cache
                                }
                            runResult =
                                config.configuration.reviews
                                    |> List.foldl
                                        (\review soFar ->
                                            let
                                                runResultForReview : { errorsByPath : List { path : String, range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix }, cache : Review.Cache }
                                                runResultForReview =
                                                    run review soFar.cache
                                            in
                                            { cache = runResultForReview.cache
                                            , errorsByPath =
                                                runResultForReview.errorsByPath ++ soFar.errorsByPath
                                            }
                                        )
                                        { errorsByPath = [], cache = Review.cacheEmpty }

                            errorsToDisplay :
                                { fixable : Maybe { path : String, range : Elm.Syntax.Range.Range, message : String, details : List String, source : String, fixedSource : String }
                                , otherErrors : List { path : String, range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix }
                                }
                            errorsToDisplay =
                                runResult.errorsByPath
                                    |> List.foldl
                                        (\error soFar ->
                                            case soFar.fixable of
                                                Nothing ->
                                                    case error.fix of
                                                        [] ->
                                                            { fixable = Nothing
                                                            , otherErrors = soFar.otherErrors |> (::) error
                                                            }

                                                        fix0 :: fix1Up ->
                                                            let
                                                                maybeSourceToFix : Maybe String
                                                                maybeSourceToFix =
                                                                    case error.path of
                                                                        "elmJson" ->
                                                                            initialFiles.elmJson.source |> Just

                                                                        filePath ->
                                                                            initialFilesByPath |> FastDict.get filePath
                                                            in
                                                            case maybeSourceToFix of
                                                                Nothing ->
                                                                    { fixable = Nothing
                                                                    , otherErrors = soFar.otherErrors |> (::) error
                                                                    }

                                                                Just sourceToFix ->
                                                                    case sourceToFix |> Review.fixFile (fix0 :: fix1Up) of
                                                                        Ok fixedSource ->
                                                                            { fixable = { source = sourceToFix, fixedSource = fixedSource, message = error.message, details = error.details, range = error.range, path = error.path } |> Just
                                                                            , otherErrors = soFar.otherErrors |> (::) error
                                                                            }

                                                                        Err _ ->
                                                                            { fixable = Nothing
                                                                            , otherErrors = soFar.otherErrors |> (::) error
                                                                            }

                                                Just soFarFix ->
                                                    { fixable = soFarFix |> Just
                                                    , otherErrors = soFar.otherErrors |> (::) error
                                                    }
                                        )
                                        { fixable = Nothing
                                        , otherErrors = []
                                        }
                        in
                        ( HavingRunReviewsPreviously
                            { cache = runResult.cache
                            , availableErrorsOnReject = errorsToDisplay.otherErrors
                            , elmJsonSource = initialFiles.elmJson.source
                            , filesByPath = initialFilesByPath
                            }
                        , errorsToDisplay
                            |> errorReceivedToJson
                                { elmJsonSource = initialFiles.elmJson.source
                                , filesByPath = initialFilesByPath
                                }
                            |> config.toJs
                            |> Cmd.map Basics.never
                        )

                    ErrorFixRejected ->
                        case state of
                            HavingRunReviewsPreviously havingRunReviewsPreviously ->
                                -- TODO provide new errors and fixes
                                ( HavingRunReviewsPreviously havingRunReviewsPreviously
                                , Cmd.none
                                )

                            unexpectedState ->
                                ( unexpectedState, Cmd.none )

                    FileAddedOrChanged fileAddedOrChanged ->
                        case state of
                            HavingRunReviewsPreviously havingRunReviewsPreviously ->
                                -- TODO rerun with cache, overwriting existing errors
                                ( HavingRunReviewsPreviously havingRunReviewsPreviously
                                , Cmd.none
                                )

                            unexpectedState ->
                                ( unexpectedState, Cmd.none )

                    FileRemoved fileRemoved ->
                        case state of
                            HavingRunReviewsPreviously havingRunReviewsPreviously ->
                                -- TODO rerun with cache, overwriting existing errors
                                ( HavingRunReviewsPreviously havingRunReviewsPreviously
                                , Cmd.none
                                )

                            unexpectedState ->
                                ( unexpectedState, Cmd.none )

                    JsonDecodingFailed decodeError ->
                        ( state
                        , Json.Encode.object
                            [ ( "tag", "JsonDecodingFailed" |> Json.Encode.string )
                            , ( "value", decodeError |> Json.Decode.errorToString |> Json.Encode.string )
                            ]
                            |> config.toJs
                            |> Cmd.map Basics.never
                        )
        , subscriptions =
            \_ ->
                config.fromJs
                    (\fromJs ->
                        case fromJs |> Json.Decode.decodeValue eventJsonDecoder of
                            Err decodeError ->
                                JsonDecodingFailed decodeError

                            Ok event ->
                                event
                    )
        }


errorReceivedToJson :
    { elmJsonSource : String, filesByPath : FastDict.Dict String String }
    ->
        { fixable : Maybe { path : String, range : Elm.Syntax.Range.Range, message : String, details : List String, source : String, fixedSource : String }
        , otherErrors : List { path : String, range : Elm.Syntax.Range.Range, message : String, details : List String, fix : List Review.Fix }
        }
    -> Json.Encode.Value
errorReceivedToJson projectSources =
    \errorReceived ->
        Json.Encode.object
            [ ( "tag", "ErrorsReceived" |> Json.Encode.string )
            , ( "value"
              , case errorReceived.fixable of
                    Nothing ->
                        Json.Encode.object
                            [ ( "display"
                              , errorReceived.otherErrors
                                    |> List.map
                                        (\error ->
                                            error
                                                |> errorDisplay
                                                    (case error.path of
                                                        "elm.json" ->
                                                            projectSources.elmJsonSource

                                                        filePath ->
                                                            projectSources.filesByPath |> FastDict.get filePath |> Maybe.withDefault ""
                                                    )
                                        )
                                    |> String.join "\n\n\n"
                                    |> Json.Encode.string
                              )
                            , ( "fix", Json.Encode.null )
                            ]

                    Just fixable ->
                        Json.Encode.object
                            [ ( "display", fixable |> errorDisplay fixable.source |> Json.Encode.string )
                            , ( "fix"
                              , Json.Encode.object
                                    [ ( "path", fixable.path |> Json.Encode.string )
                                    , ( "fixedSource", fixable.fixedSource |> Json.Encode.string )
                                    ]
                              )
                            ]
              )
            ]


errorDisplay : String -> { error_ | path : String, range : Elm.Syntax.Range.Range, message : String, details : List String } -> String
errorDisplay source =
    \error ->
        [ error.path
        , ":"
        , error.range.start.row |> String.fromInt
        , ":"
        , error.range.start.column |> String.fromInt
        , "\n\n    "
        , error.message
        , "\n\n"
        , codeExtract source error.range
            |> String.split "\n"
            |> List.map (\line -> "|   " ++ line)
            |> String.join "\n"
        , "\n\n"
        , error.details |> String.join "\n\n"
        ]
            |> String.concat


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
            let
                lineContent : String
                lineContent =
                    sourceLineAtRow (range.start.row - 1)
            in
            [ sourceLineAtRow (range.start.row - 2)
            , lineContent
            , underline { start = range.start.column, end = range.end.column, lineContent = lineContent }
            , sourceLineAtRow range.end.row
            ]
                |> List.filter (\l -> not (l |> String.isEmpty))
                |> String.join "\n"

    else
        let
            startLineNumber : Int
            startLineNumber =
                range.start.row - 1

            startLineContent : String
            startLineContent =
                sourceLineAtRow startLineNumber

            linesBetweenStartAndEnd : List Int
            linesBetweenStartAndEnd =
                List.range range.start.row (range.end.row - 2)

            endLineContent : String
            endLineContent =
                sourceLineAtRow (range.end.row - 1)
        in
        [ [ sourceLineAtRow (startLineNumber - 1)
          , startLineContent
          , underline
                { start = range.start.column
                , end = List.length (String.toList startLineContent) + 1
                , lineContent = startLineContent
                }
          ]
            |> List.filter (\l -> not (l |> String.isEmpty))
            |> String.join "\n"
        , linesBetweenStartAndEnd
            |> List.map
                (\middleLine ->
                    let
                        line : String
                        line =
                            sourceLineAtRow middleLine
                    in
                    if String.isEmpty line then
                        sourceLineAtRow middleLine

                    else
                        sourceLineAtRow middleLine
                            ++ "\n"
                            ++ underlineWholeLine line
                )
            |> String.join "\n"
        , [ endLineContent
          , underline
                { start = getIndexOfFirstNonSpace endLineContent + 1
                , end = range.end.column
                , lineContent = endLineContent
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


underlineWholeLine : String -> String
underlineWholeLine line =
    let
        start : Int
        start =
            getIndexOfFirstNonSpace line

        end : Int
        end =
            line |> String.length
    in
    String.repeat start " "
        ++ String.repeat (end - start) "^"


underline : { start : Int, end : Int, lineContent : String } -> String
underline toUnderline =
    let
        lineChars : List Char
        lineChars =
            String.toList toUnderline.lineContent

        preText : List Char
        preText =
            List.take (toUnderline.start - 1) lineChars

        unicodePreOffset : Int
        unicodePreOffset =
            String.length (String.fromList preText) - List.length preText

        inText : List Char
        inText =
            lineChars
                |> List.drop (toUnderline.start - 1)
                |> List.take (toUnderline.end - toUnderline.start)

        unicodeInOffset : Int
        unicodeInOffset =
            -- We want to show enough ^ characters to cover the whole underlined zone,
            -- and for unicode characters that sometimes means 2 ^
            String.length (String.fromList inText) - List.length inText
    in
    String.repeat (unicodePreOffset + toUnderline.start - 1) " "
        ++ String.repeat (unicodeInOffset + toUnderline.end - toUnderline.start) "^"
