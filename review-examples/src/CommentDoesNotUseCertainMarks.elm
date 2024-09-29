module CommentDoesNotUseCertainMarks exposing (review)

{-|

@docs review

-}

import Elm.Syntax.Declaration
import Elm.Syntax.File
import Elm.Syntax.Node
import Elm.Syntax.Range
import Review


{-| Sometimes, you have code that compiles and should be able to run but still requires
attention in certain parts in the future.

For these, elm comments (single-line `--`, multi-line `{- -}`, documentation `{-| -}`)
can include words like `TODO`, `REPLACEME` or syntax like `- [ ]`.

The review does not inspect the `elm.json` summary, the change log or readme.
Feel free to fork if these seem useful to you!


### not reported with `review [ "TODO" ]`

    error =
        { ...
        , fix = [] -- consider removing the problematic call
        }


### reported with `review [ "TODO" ]`

    error =
        { ...
        , fix = [] -- TODO consider removing the problematic call
        }

-}
review : List String -> Review.Review
review specificMarksToFindInComments =
    Review.create
        { inspect =
            [ Review.inspectModule
                (moduleDataToKnowledge
                    { specificMarksToFindInComments = specificMarksToFindInComments }
                )
            ]
        , knowledgeMerge = knowledgeMerge
        , report = report
        }


type alias Knowledge =
    { specificMarksInComments :
        List
            { path : String
            , mark : String
            , range : Elm.Syntax.Range.Range
            }
    }


moduleDataToKnowledge :
    { specificMarksToFindInComments : List String }
    -> { info_ | path : String, syntax : Elm.Syntax.File.File }
    -> Knowledge
moduleDataToKnowledge reviewConfiguration moduleData =
    -- TODO declaration documentation comments
    let
        allComments : List (Elm.Syntax.Node.Node String)
        allComments =
            moduleData.syntax.comments
                ++ (moduleData.syntax.declarations
                        |> List.filterMap
                            (\(Elm.Syntax.Node.Node _ declaration) ->
                                case declaration of
                                    Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                                        valueOrFunctionDeclaration.documentation

                                    Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                                        choiceTypeDeclaration.documentation

                                    Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                                        typeAliasDeclaration.documentation

                                    Elm.Syntax.Declaration.PortDeclaration _ ->
                                        Nothing

                                    Elm.Syntax.Declaration.InfixDeclaration _ ->
                                        Nothing

                                    -- invalid elm
                                    Elm.Syntax.Declaration.Destructuring _ _ ->
                                        Nothing
                            )
                   )
    in
    { specificMarksInComments =
        allComments
            |> List.concatMap
                (\(Elm.Syntax.Node.Node commentRange commentContent) ->
                    reviewConfiguration.specificMarksToFindInComments
                        |> List.concatMap
                            (\specificMarkInComment ->
                                commentContent
                                    |> stringRowColumnOffsetsOfMark specificMarkInComment
                                    |> List.map
                                        (\specificMarkOffset ->
                                            { path = moduleData.path
                                            , mark = specificMarkInComment
                                            , range =
                                                rangeInSingleRow
                                                    { row = commentRange.start.row + specificMarkOffset.row0Based
                                                    , startColumn =
                                                        case specificMarkOffset.row0Based of
                                                            0 ->
                                                                commentRange.start.column + specificMarkOffset.startColumn0Based

                                                            _ ->
                                                                specificMarkOffset.startColumn0Based + 1
                                                    , length = specificMarkInComment |> String.length
                                                    }
                                            }
                                        )
                            )
                )
    }


{-| The given mark to find should not wrap multiple lines
-}
stringRowColumnOffsetsOfMark : String -> (String -> List { row0Based : Int, startColumn0Based : Int })
stringRowColumnOffsetsOfMark toFind string =
    string
        |> String.lines
        |> List.indexedMap
            (\row0Based commentLine ->
                commentLine
                    |> String.indexes toFind
                    |> List.map
                        (\startColumn0Based ->
                            { row0Based = row0Based
                            , startColumn0Based = startColumn0Based
                            }
                        )
            )
        |> List.concat


rangeInSingleRow : { row : Int, startColumn : Int, length : Int } -> Elm.Syntax.Range.Range
rangeInSingleRow rowStartColumnAndLength =
    { start = { row = rowStartColumnAndLength.row, column = rowStartColumnAndLength.startColumn }
    , end =
        { row = rowStartColumnAndLength.row
        , column = rowStartColumnAndLength.startColumn + rowStartColumnAndLength.length
        }
    }


knowledgeMerge : Knowledge -> Knowledge -> Knowledge
knowledgeMerge a b =
    { specificMarksInComments =
        a.specificMarksInComments ++ b.specificMarksInComments
    }


report : Knowledge -> List Review.Error
report knowledge =
    knowledge.specificMarksInComments
        |> List.map
            (\specificMarkInComment ->
                { path = specificMarkInComment.path
                , message = "comment uses " ++ specificMarkInComment.mark ++ " mark"
                , details = [ "This mark has been placed in a comment as a reminder. Read the comment and analyze the surrounding context to decide what needs to be done. Once the issue is resolved, remove the notice." ]
                , range = specificMarkInComment.range
                , fix = []
                }
            )
