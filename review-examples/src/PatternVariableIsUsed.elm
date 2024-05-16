module PatternVariableIsUsed exposing (review)

{-|

@docs review

-}

import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Expression.LocalExtra
import FastDict
import FastDict.LocalExtra
import Pattern.LocalExtra
import Review
import Set exposing (Set)


{-| Report variables in patterns that are never referenced.

Unused stray variables might be a sign that someone wanted to use it for something but didn't do so, yet.
If that's not the case, explicitly say so (usually by replacing the variable with `_`)


### not reported

    a =
        \used ->
            used
    a used =
        used
    a (Variant ( _, { used } )) =
        used


### reported

    a =
        \unused ->
            ""
    a unused =
        ""
    a (Variant ( _, { unused } )) =
        used

-}
review : Review.Review
review =
    Review.create
        { inspect =
            [ Review.inspectModule
                (\moduleData ->
                    moduleData |> moduleToKnowledge
                )
            ]
        , report = report
        , knowledgeMerge = knowledgeMerge
        }


type alias Knowledge =
    { unusedPatternVariables :
        List
            { modulePath : String
            , name : String
            , variableRange : Elm.Syntax.Range.Range
            , fixRange : Elm.Syntax.Range.Range
            , fixReplacement : String
            }
    }


moduleToKnowledge : { info_ | path : String, syntax : Elm.Syntax.File.File } -> Knowledge
moduleToKnowledge =
    \moduleData ->
        { unusedPatternVariables =
            moduleData.syntax.declarations
                |> List.concatMap
                    (\(Elm.Syntax.Node.Node _ declaration) ->
                        case declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                                valueOrFunctionDeclaration.declaration
                                    |> Elm.Syntax.Node.value
                                    |> .expression
                                    |> expressionKnowledge
                                    |> expressionKnowledgeAddVariablesUnusedIn
                                        (valueOrFunctionDeclaration.declaration
                                            |> Elm.Syntax.Node.value
                                            |> .arguments
                                            |> List.concatMap Pattern.LocalExtra.variablesAndRanges
                                        )
                                    |> .unusedVariablePatterns

                            _ ->
                                []
                    )
                |> List.map
                    (\unusedVariable ->
                        { modulePath = moduleData.path
                        , name = unusedVariable.variableName
                        , variableRange = unusedVariable.variableRange
                        , fixRange = unusedVariable.fixRange
                        , fixReplacement = unusedVariable.fixReplacement
                        }
                    )
        }


expressionKnowledgeAddVariablesUnusedIn :
    List
        { variableName : String
        , variableRange : Elm.Syntax.Range.Range
        , fixRange : Elm.Syntax.Range.Range
        , fixReplacement : String
        }
    ->
        { unqualifiedReferences : Set String
        , unusedVariablePatterns :
            List
                { variableName : String
                , variableRange : Elm.Syntax.Range.Range
                , fixRange : Elm.Syntax.Range.Range
                , fixReplacement : String
                }
        }
    ->
        { unqualifiedReferences : Set String
        , unusedVariablePatterns :
            List
                { variableName : String
                , variableRange : Elm.Syntax.Range.Range
                , fixRange : Elm.Syntax.Range.Range
                , fixReplacement : String
                }
        }
expressionKnowledgeAddVariablesUnusedIn outerVariables =
    \expressionInnerKnowledge ->
        { unqualifiedReferences = expressionInnerKnowledge.unqualifiedReferences
        , unusedVariablePatterns =
            (outerVariables
                |> List.filter
                    (\variable ->
                        not (expressionInnerKnowledge.unqualifiedReferences |> Set.member variable.variableName)
                    )
            )
                ++ expressionInnerKnowledge.unusedVariablePatterns
        }


flatExpressionKnowledgeMap :
    (element
     ->
        { unqualifiedReferences : Set String
        , unusedVariablePatterns :
            List
                { variableName : String
                , variableRange : Elm.Syntax.Range.Range
                , fixRange : Elm.Syntax.Range.Range
                , fixReplacement : String
                }
        }
    )
    -> List element
    ->
        { unqualifiedReferences : Set String
        , unusedVariablePatterns :
            List
                { variableName : String
                , variableRange : Elm.Syntax.Range.Range
                , fixRange : Elm.Syntax.Range.Range
                , fixReplacement : String
                }
        }
flatExpressionKnowledgeMap elementToExpressionKnowledge =
    \expressionNodeList ->
        expressionNodeList
            |> List.foldl
                (\expressionNode soFar ->
                    expressionKnowledgeMerge (expressionNode |> elementToExpressionKnowledge) soFar
                )
                { unqualifiedReferences = Set.empty
                , unusedVariablePatterns = []
                }


expressionKnowledgeMerge :
    { unqualifiedReferences : Set String
    , unusedVariablePatterns :
        List
            { variableName : String
            , variableRange : Elm.Syntax.Range.Range
            , fixRange : Elm.Syntax.Range.Range
            , fixReplacement : String
            }
    }
    ->
        { unqualifiedReferences : Set String
        , unusedVariablePatterns :
            List
                { variableName : String
                , variableRange : Elm.Syntax.Range.Range
                , fixRange : Elm.Syntax.Range.Range
                , fixReplacement : String
                }
        }
    ->
        { unqualifiedReferences : Set String
        , unusedVariablePatterns :
            List
                { variableName : String
                , variableRange : Elm.Syntax.Range.Range
                , fixRange : Elm.Syntax.Range.Range
                , fixReplacement : String
                }
        }
expressionKnowledgeMerge knowledge soFar =
    { unqualifiedReferences = Set.union knowledge.unqualifiedReferences soFar.unqualifiedReferences
    , unusedVariablePatterns = knowledge.unusedVariablePatterns ++ soFar.unusedVariablePatterns
    }


expressionKnowledge :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    ->
        { unqualifiedReferences : Set String
        , unusedVariablePatterns :
            List
                { variableName : String
                , variableRange : Elm.Syntax.Range.Range
                , fixRange : Elm.Syntax.Range.Range
                , fixReplacement : String
                }
        }
expressionKnowledge =
    \(Elm.Syntax.Node.Node _ expression) ->
        case expression of
            Elm.Syntax.Expression.UnitExpr ->
                { unqualifiedReferences = Set.empty, unusedVariablePatterns = [] }

            Elm.Syntax.Expression.Application subs ->
                subs |> flatExpressionKnowledgeMap expressionKnowledge

            Elm.Syntax.Expression.OperatorApplication _ _ left right ->
                expressionKnowledgeMerge (left |> expressionKnowledge) (right |> expressionKnowledge)

            Elm.Syntax.Expression.FunctionOrValue qualification unqualified ->
                case qualification of
                    _ :: _ ->
                        { unqualifiedReferences = Set.empty, unusedVariablePatterns = [] }

                    [] ->
                        { unqualifiedReferences = unqualified |> Set.singleton, unusedVariablePatterns = [] }

            Elm.Syntax.Expression.IfBlock condition onTrue onFalse ->
                expressionKnowledgeMerge (condition |> expressionKnowledge)
                    (expressionKnowledgeMerge (onTrue |> expressionKnowledge)
                        (onFalse |> expressionKnowledge)
                    )

            Elm.Syntax.Expression.PrefixOperator _ ->
                { unqualifiedReferences = Set.empty, unusedVariablePatterns = [] }

            Elm.Syntax.Expression.Operator _ ->
                { unqualifiedReferences = Set.empty, unusedVariablePatterns = [] }

            Elm.Syntax.Expression.Integer _ ->
                { unqualifiedReferences = Set.empty, unusedVariablePatterns = [] }

            Elm.Syntax.Expression.Hex _ ->
                { unqualifiedReferences = Set.empty, unusedVariablePatterns = [] }

            Elm.Syntax.Expression.Floatable _ ->
                { unqualifiedReferences = Set.empty, unusedVariablePatterns = [] }

            Elm.Syntax.Expression.Negation negated ->
                negated |> expressionKnowledge

            Elm.Syntax.Expression.Literal _ ->
                { unqualifiedReferences = Set.empty, unusedVariablePatterns = [] }

            Elm.Syntax.Expression.CharLiteral _ ->
                { unqualifiedReferences = Set.empty, unusedVariablePatterns = [] }

            Elm.Syntax.Expression.TupledExpression subs ->
                subs |> flatExpressionKnowledgeMap expressionKnowledge

            Elm.Syntax.Expression.ParenthesizedExpression inParens ->
                inParens |> expressionKnowledge

            Elm.Syntax.Expression.LetExpression letIn ->
                let
                    destructuredVariables : List { variableName : String, variableRange : Elm.Syntax.Range.Range, fixRange : Elm.Syntax.Range.Range, fixReplacement : String }
                    destructuredVariables =
                        letIn.declarations
                            |> List.concatMap
                                (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                    case letDeclaration of
                                        Elm.Syntax.Expression.LetFunction letValueOrFunction ->
                                            []

                                        Elm.Syntax.Expression.LetDestructuring pattern _ ->
                                            pattern |> Pattern.LocalExtra.variablesAndRanges
                                )
                in
                expressionKnowledgeMerge
                    (letIn.expression
                        |> expressionKnowledge
                    )
                    (letIn.declarations
                        |> flatExpressionKnowledgeMap
                            (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                case letDeclaration of
                                    Elm.Syntax.Expression.LetFunction letValueOrFunction ->
                                        letValueOrFunction
                                            |> .declaration
                                            |> Elm.Syntax.Node.value
                                            |> .expression
                                            |> expressionKnowledge
                                            |> expressionKnowledgeAddVariablesUnusedIn
                                                (letValueOrFunction
                                                    |> .declaration
                                                    |> Elm.Syntax.Node.value
                                                    |> .arguments
                                                    |> List.concatMap Pattern.LocalExtra.variablesAndRanges
                                                )

                                    Elm.Syntax.Expression.LetDestructuring _ destructuredExpression ->
                                        destructuredExpression |> expressionKnowledge
                            )
                    )
                    |> expressionKnowledgeAddVariablesUnusedIn destructuredVariables

            Elm.Syntax.Expression.CaseExpression caseOf ->
                expressionKnowledgeMerge
                    (caseOf.expression |> expressionKnowledge)
                    (caseOf.cases
                        |> flatExpressionKnowledgeMap
                            (\( patternInCase, expressionInCase ) ->
                                expressionInCase
                                    |> expressionKnowledge
                                    |> expressionKnowledgeAddVariablesUnusedIn
                                        (patternInCase |> Pattern.LocalExtra.variablesAndRanges)
                            )
                    )

            Elm.Syntax.Expression.LambdaExpression lambda ->
                lambda.expression
                    |> expressionKnowledge
                    |> expressionKnowledgeAddVariablesUnusedIn
                        (lambda
                            |> .args
                            |> List.concatMap Pattern.LocalExtra.variablesAndRanges
                        )

            Elm.Syntax.Expression.RecordExpr fields ->
                fields
                    |> flatExpressionKnowledgeMap
                        (\(Elm.Syntax.Node.Node _ ( _, newFieldValue )) ->
                            newFieldValue |> expressionKnowledge
                        )

            Elm.Syntax.Expression.ListExpr subs ->
                subs |> flatExpressionKnowledgeMap expressionKnowledge

            Elm.Syntax.Expression.RecordAccess record _ ->
                record |> expressionKnowledge

            Elm.Syntax.Expression.RecordAccessFunction _ ->
                { unqualifiedReferences = Set.empty, unusedVariablePatterns = [] }

            Elm.Syntax.Expression.RecordUpdateExpression _ newFields ->
                newFields
                    |> flatExpressionKnowledgeMap
                        (\(Elm.Syntax.Node.Node _ ( _, newFieldValue )) ->
                            newFieldValue |> expressionKnowledge
                        )

            Elm.Syntax.Expression.GLSLExpression _ ->
                { unqualifiedReferences = Set.empty, unusedVariablePatterns = [] }


knowledgeMerge : Knowledge -> Knowledge -> Knowledge
knowledgeMerge a b =
    { unusedPatternVariables = a.unusedPatternVariables ++ b.unusedPatternVariables
    }


report : Knowledge -> List Review.Error
report knowledge =
    knowledge.unusedPatternVariables
        |> List.map
            (\unusedPatternVariable ->
                { path = unusedPatternVariable.modulePath
                , message = "pattern variable " ++ unusedPatternVariable.name ++ " isn't used"
                , details = [ "Maybe you wanted to use this variable for something? If you don't need it, remove the variable here by applying the automatic fix." ]
                , range = unusedPatternVariable.variableRange
                , fix =
                    [ Review.fixReplaceRange
                        unusedPatternVariable.fixRange
                        unusedPatternVariable.fixReplacement
                    ]
                }
            )
