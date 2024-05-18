module StringSpellsCompanyNameCorrectly exposing (review)

import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.Node
import Elm.Syntax.Range
import Review


review : Review.Review
review =
    Review.create
        { inspect =
            [ Review.inspectModule
                (\moduleData ->
                    { stringsWithTypos =
                        moduleData.syntax.declarations
                            |> List.concatMap declarationToTyposInStrings
                            |> List.map
                                (\typoRange ->
                                    { range = typoRange
                                    , modulePath = moduleData.path
                                    }
                                )
                    }
                )
            ]
        , knowledgeMerge = knowledgeMerge
        , report = report
        }


declarationToTyposInStrings :
    Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration
    -> List Elm.Syntax.Range.Range
declarationToTyposInStrings =
    \(Elm.Syntax.Node.Node _ declaration) ->
        case declaration of
            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                functionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .expression
                    |> expressionToTyposInStrings

            _ ->
                []


expressionToTyposInStrings :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> List Elm.Syntax.Range.Range
expressionToTyposInStrings =
    \expressionNode ->
        case expressionNode of
            Elm.Syntax.Node.Node range (Elm.Syntax.Expression.Literal string) ->
                string
                    |> Review.sourceRangesOf "frits.com"
                    |> List.map (rangeRelativeTo range.start)

            nonStringExpressionNode ->
                nonStringExpressionNode
                    |> Review.expressionSubs
                    |> List.concatMap expressionToTyposInStrings


rangeRelativeTo :
    Elm.Syntax.Range.Location
    -> (Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range)
rangeRelativeTo baseStart =
    \offsetRange ->
        { start = offsetRange.start |> locationRelativeTo baseStart
        , end = offsetRange.end |> locationRelativeTo baseStart
        }


locationRelativeTo :
    Elm.Syntax.Range.Location
    -> (Elm.Syntax.Range.Location -> Elm.Syntax.Range.Location)
locationRelativeTo baseStart =
    \offsetLocation ->
        case offsetLocation.row of
            1 ->
                { row = baseStart.row
                , column = baseStart.column + offsetLocation.column
                }

            offsetRowAtLeast2 ->
                { row = baseStart.row + (offsetRowAtLeast2 - 1)
                , column = offsetLocation.column
                }


knowledgeMerge : Knowledge -> Knowledge -> Knowledge
knowledgeMerge a b =
    { stringsWithTypos =
        a.stringsWithTypos ++ b.stringsWithTypos
    }


report : Knowledge -> List Review.Error
report knowledge =
    knowledge.stringsWithTypos
        |> List.map
            (\stringWithTypos ->
                { path = stringWithTypos.modulePath
                , message = "misspelled fruits.com"
                , details = [ "The typo of using frits.com instead of fruits.com has been made and noticed by users too many times. Our company is `fruits.com`, not `frits.com`." ]
                , range = stringWithTypos.range
                , fix =
                    [ { path = stringWithTypos.modulePath
                      , edits = [ Review.replaceRange stringWithTypos.range "fruits.com" ]
                      }
                    ]
                }
            )


type alias Knowledge =
    { stringsWithTypos :
        List { modulePath : String, range : Elm.Syntax.Range.Range }
    }
