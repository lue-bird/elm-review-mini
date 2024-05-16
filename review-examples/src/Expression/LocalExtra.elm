module Expression.LocalExtra exposing (referenceUsesIgnoringPatternVariables)

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


referenceUsesIgnoringPatternVariables :
    Set String
    ->
        (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
         -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
        )
referenceUsesIgnoringPatternVariables branchLocalVariables =
    -- IGNORE TCO
    \(Elm.Syntax.Node.Node expressionRange expression) ->
        case expression of
            Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName ->
                case qualification of
                    [] ->
                        if branchLocalVariables |> Set.member unqualifiedName then
                            FastDict.empty

                        else
                            FastDict.singleton ( [], unqualifiedName ) [ expressionRange ]

                    qualificationPart0 :: qualificationPart1Up ->
                        FastDict.singleton ( qualificationPart0 :: qualificationPart1Up, unqualifiedName )
                            [ expressionRange ]

            Elm.Syntax.Expression.LambdaExpression lambda ->
                FastDict.LocalExtra.unionWith (++)
                    (lambda.args |> Pattern.LocalExtra.listReferenceUses)
                    (lambda.expression
                        |> referenceUsesIgnoringPatternVariables
                            (Set.union branchLocalVariables
                                (lambda.args
                                    |> Set.LocalExtra.unionFromListMap Pattern.LocalExtra.variables
                                )
                            )
                    )

            Elm.Syntax.Expression.CaseExpression caseOf ->
                FastDict.LocalExtra.unionWith (++)
                    (caseOf.expression |> referenceUsesIgnoringPatternVariables branchLocalVariables)
                    (caseOf.cases
                        |> FastDict.LocalExtra.unionFromListWithMap
                            (\( patternNode, caseExpressionNode ) ->
                                FastDict.LocalExtra.unionWith (++)
                                    (patternNode |> Pattern.LocalExtra.referenceUses)
                                    (caseExpressionNode
                                        |> referenceUsesIgnoringPatternVariables
                                            (Set.union branchLocalVariables
                                                (patternNode |> Pattern.LocalExtra.variables)
                                            )
                                    )
                            )
                            (++)
                    )

            Elm.Syntax.Expression.LetExpression letIn ->
                let
                    variablesForWholeLetIn : Set String
                    variablesForWholeLetIn =
                        Set.union branchLocalVariables
                            (letIn.declarations
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
                            )
                in
                FastDict.LocalExtra.unionWith (++)
                    (letIn.expression |> referenceUsesIgnoringPatternVariables variablesForWholeLetIn)
                    (letIn.declarations
                        |> FastDict.LocalExtra.unionFromListWithMap
                            (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                letDeclaration |> letDeclarationReferencesWithBranchLocalVariables variablesForWholeLetIn
                            )
                            (++)
                    )

            nonUnqualifiedReferenceOrVariable ->
                nonUnqualifiedReferenceOrVariable
                    |> Elm.Syntax.Node.Node expressionRange
                    |> Review.expressionSubs
                    |> FastDict.LocalExtra.unionFromListWithMap
                        (referenceUsesIgnoringPatternVariables branchLocalVariables)
                        (++)


letDeclarationReferencesWithBranchLocalVariables :
    Set String
    ->
        (Elm.Syntax.Expression.LetDeclaration
         -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
        )
letDeclarationReferencesWithBranchLocalVariables branchLocalVariables =
    \letDeclaration ->
        case letDeclaration of
            Elm.Syntax.Expression.LetDestructuring patternNode destructuredExpressionNode ->
                FastDict.LocalExtra.unionWith (++)
                    (patternNode |> Pattern.LocalExtra.referenceUses)
                    (destructuredExpressionNode |> referenceUsesIgnoringPatternVariables branchLocalVariables)

            Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                [ case letValueOrFunctionDeclaration.signature of
                    Nothing ->
                        FastDict.empty

                    Just (Elm.Syntax.Node.Node _ signature) ->
                        signature.typeAnnotation
                            |> Type.LocalExtra.referenceUses
                , letValueOrFunctionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .arguments
                    |> Pattern.LocalExtra.listReferenceUses
                , (letValueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value |> .expression)
                    |> referenceUsesIgnoringPatternVariables
                        (Set.union branchLocalVariables
                            (letValueOrFunctionDeclaration.declaration
                                |> Elm.Syntax.Node.value
                                |> .arguments
                                |> Set.LocalExtra.unionFromListMap
                                    (\patternNode -> patternNode |> Pattern.LocalExtra.variables)
                            )
                        )
                ]
                    |> FastDict.LocalExtra.unionFromListWithMap identity (++)
