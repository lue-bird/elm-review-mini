module Expression.LocalExtra exposing (identifiers)

import Elm.Syntax.Expression
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import FastDict
import FastDict.LocalExtra
import Pattern.LocalExtra
import Review
import Set exposing (Set)
import Set.LocalExtra
import Type.LocalExtra


identifiers :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
identifiers =
    -- IGNORE TCO
    \(Elm.Syntax.Node.Node expressionRange expression) ->
        case expression of
            Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName ->
                FastDict.singleton ( qualification, unqualifiedName ) [ expressionRange ]

            Elm.Syntax.Expression.LambdaExpression lambda ->
                FastDict.LocalExtra.unionWith (++)
                    (lambda.args |> Pattern.LocalExtra.listIdentifierUses)
                    (lambda.expression
                        |> identifiers
                        |> FastDict.LocalExtra.excludeKeys
                            (lambda.args
                                |> Set.LocalExtra.unionFromListMap Pattern.LocalExtra.variables
                                |> Set.map (\variable -> ( [], variable ))
                            )
                    )

            Elm.Syntax.Expression.CaseExpression caseOf ->
                FastDict.LocalExtra.unionWith (++)
                    (caseOf.expression |> identifiers)
                    (caseOf.cases
                        |> FastDict.LocalExtra.unionFromListWithMap
                            (\( patternNode, caseExpressionNode ) ->
                                FastDict.LocalExtra.unionWith (++)
                                    (patternNode |> Pattern.LocalExtra.identifierUses)
                                    (caseExpressionNode
                                        |> identifiers
                                        |> FastDict.LocalExtra.excludeKeys
                                            (patternNode
                                                |> Pattern.LocalExtra.variables
                                                |> Set.map (\variable -> ( [], variable ))
                                            )
                                    )
                            )
                            (++)
                    )

            Elm.Syntax.Expression.LetExpression letIn ->
                let
                    variablesForWholeLetIn : Set String
                    variablesForWholeLetIn =
                        letIn.declarations
                            |> Set.LocalExtra.unionFromListMap
                                (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                    case letDeclaration of
                                        Elm.Syntax.Expression.LetFunction letFunction ->
                                            letFunction.declaration
                                                |> Elm.Syntax.Node.value
                                                |> .name
                                                |> Elm.Syntax.Node.value
                                                |> Set.singleton

                                        Elm.Syntax.Expression.LetDestructuring patternNode _ ->
                                            patternNode |> Pattern.LocalExtra.variables
                                )
                in
                FastDict.LocalExtra.unionWith (++)
                    (letIn.expression
                        |> identifiers
                        |> FastDict.LocalExtra.excludeKeys
                            (variablesForWholeLetIn |> Set.map (\variable -> ( [], variable )))
                    )
                    (letIn.declarations
                        |> FastDict.LocalExtra.unionFromListWithMap
                            (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                letDeclaration
                                    |> letDeclarationIdentifiers
                                    |> FastDict.LocalExtra.excludeKeys
                                        (variablesForWholeLetIn |> Set.map (\variable -> ( [], variable )))
                            )
                            (++)
                    )

            nonUnqualifiedReferenceOrVariable ->
                nonUnqualifiedReferenceOrVariable
                    |> Elm.Syntax.Node.Node expressionRange
                    |> Review.expressionSubs
                    |> FastDict.LocalExtra.unionFromListWithMap
                        identifiers
                        (++)


letDeclarationIdentifiers :
    Elm.Syntax.Expression.LetDeclaration
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
letDeclarationIdentifiers =
    \letDeclaration ->
        case letDeclaration of
            Elm.Syntax.Expression.LetDestructuring patternNode destructuredExpressionNode ->
                FastDict.LocalExtra.unionWith (++)
                    (patternNode |> Pattern.LocalExtra.identifierUses)
                    (destructuredExpressionNode |> identifiers)

            Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                [ case letValueOrFunctionDeclaration.signature of
                    Nothing ->
                        FastDict.empty

                    Just (Elm.Syntax.Node.Node _ signature) ->
                        signature.typeAnnotation
                            |> Type.LocalExtra.identifierUses
                , letValueOrFunctionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .arguments
                    |> Pattern.LocalExtra.listIdentifierUses
                , (letValueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value |> .expression)
                    |> identifiers
                    |> FastDict.LocalExtra.excludeKeys
                        (letValueOrFunctionDeclaration.declaration
                            |> Elm.Syntax.Node.value
                            |> .arguments
                            |> Set.LocalExtra.unionFromListMap
                                (\patternNode -> patternNode |> Pattern.LocalExtra.variables)
                            |> Set.map (\variable -> ( [], variable ))
                        )
                ]
                    |> FastDict.LocalExtra.unionFromListWithMap identity (++)
