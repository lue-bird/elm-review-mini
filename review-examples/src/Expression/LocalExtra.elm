module Expression.LocalExtra exposing (referenceUseCountsWithBranchLocalVariables)

import Elm.Syntax.Expression
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import FastDict
import FastDict.LocalExtra
import Pattern.LocalExtra
import Review
import Set exposing (Set)
import Set.LocalExtra
import Type.LocalExtra


referenceUseCountsMerge :
    FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
referenceUseCountsMerge a b =
    FastDict.LocalExtra.unionWith (\aCount bCount -> aCount + bCount) a b


listReferenceUseCountsMerge :
    List (FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int)
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
listReferenceUseCountsMerge =
    \referenceUseCountsList ->
        referenceUseCountsList
            |> List.foldl (\sub -> referenceUseCountsMerge sub) FastDict.empty


referenceUseCountsWithBranchLocalVariables :
    Set String
    ->
        (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
         -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
        )
referenceUseCountsWithBranchLocalVariables branchLocalVariables =
    -- IGNORE TCO
    \(Elm.Syntax.Node.Node expressionRange expression) ->
        case expression of
            Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName ->
                case qualification of
                    [] ->
                        if branchLocalVariables |> Set.member unqualifiedName then
                            FastDict.empty

                        else
                            FastDict.singleton ( [], unqualifiedName ) 1

                    qualificationPart0 :: qualificationPart1Up ->
                        FastDict.singleton ( qualificationPart0 :: qualificationPart1Up, unqualifiedName ) 1

            Elm.Syntax.Expression.LambdaExpression lambda ->
                (lambda.args |> Pattern.LocalExtra.listReferenceUseCounts)
                    |> referenceUseCountsMerge
                        (lambda.expression
                            |> referenceUseCountsWithBranchLocalVariables
                                (Set.union branchLocalVariables
                                    (lambda.args
                                        |> Set.LocalExtra.unionFromListMap Pattern.LocalExtra.nodeVariables
                                    )
                                )
                        )

            Elm.Syntax.Expression.CaseExpression caseOf ->
                (caseOf.expression |> referenceUseCountsWithBranchLocalVariables branchLocalVariables)
                    |> referenceUseCountsMerge
                        (caseOf.cases
                            |> List.map
                                (\( patternNode, caseExpressionNode ) ->
                                    (patternNode |> Pattern.LocalExtra.referenceUseCounts)
                                        |> referenceUseCountsMerge
                                            (caseExpressionNode
                                                |> referenceUseCountsWithBranchLocalVariables
                                                    (Set.union branchLocalVariables
                                                        (patternNode |> Pattern.LocalExtra.nodeVariables)
                                                    )
                                            )
                                )
                            |> listReferenceUseCountsMerge
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
                                                patternNode |> Pattern.LocalExtra.nodeVariables
                                    )
                            )
                in
                (letIn.expression |> referenceUseCountsWithBranchLocalVariables variablesForWholeLetIn)
                    |> referenceUseCountsMerge
                        (letIn.declarations
                            |> List.map
                                (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                    letDeclaration |> letDeclarationReferencesWithBranchLocalVariables variablesForWholeLetIn
                                )
                            |> listReferenceUseCountsMerge
                        )

            nonUnqualifiedReferenceOrVariable ->
                nonUnqualifiedReferenceOrVariable
                    |> Elm.Syntax.Node.Node expressionRange
                    |> Review.expressionSubs
                    |> List.map (referenceUseCountsWithBranchLocalVariables branchLocalVariables)
                    |> listReferenceUseCountsMerge


letDeclarationReferencesWithBranchLocalVariables :
    Set String
    ->
        (Elm.Syntax.Expression.LetDeclaration
         -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
        )
letDeclarationReferencesWithBranchLocalVariables branchLocalVariables =
    \letDeclaration ->
        case letDeclaration of
            Elm.Syntax.Expression.LetDestructuring patternNode destructuredExpressionNode ->
                (patternNode |> Pattern.LocalExtra.referenceUseCounts)
                    |> referenceUseCountsMerge (destructuredExpressionNode |> referenceUseCountsWithBranchLocalVariables branchLocalVariables)

            Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                [ case letValueOrFunctionDeclaration.signature of
                    Nothing ->
                        FastDict.empty

                    Just (Elm.Syntax.Node.Node _ signature) ->
                        signature.typeAnnotation
                            |> Type.LocalExtra.referenceUseCounts
                , letValueOrFunctionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .arguments
                    |> Pattern.LocalExtra.listReferenceUseCounts
                , (letValueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value |> .expression)
                    |> referenceUseCountsWithBranchLocalVariables
                        (Set.union branchLocalVariables
                            (letValueOrFunctionDeclaration.declaration
                                |> Elm.Syntax.Node.value
                                |> .arguments
                                |> Set.LocalExtra.unionFromListMap
                                    (\patternNode -> patternNode |> Pattern.LocalExtra.nodeVariables)
                            )
                        )
                ]
                    |> listReferenceUseCountsMerge
