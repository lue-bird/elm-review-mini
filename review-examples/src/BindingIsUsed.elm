module BindingIsUsed exposing (review)

{-|

@docs review

-}

import Declaration.LocalExtra
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Expression.LocalExtra
import FastDict
import FastDict.LocalExtra
import FastSet
import Pattern.LocalExtra
import Review


{-| Report introduced names that are associated with a thing but never referenced:

  - variables in patterns
  - let declared values and functions
  - module declared values, functions, type aliases and choice types

An unused stray binding might be a sign that someone wanted to use it for something but didn't do so, yet.
If that's not the case, explicitly say so (usually by replacing the variable with `_` or removing the declaration)


### not reported

    a =
        \used ->
            used

    b used =
        used

    c (Variant ( _, { used } )) =
        used

    d argument =
        case argument of
            used ->
                used

    e =
        let
            { used } =
                { used = 0 }
        in
        used

    f =
        let
            used =
                1
        in
        used


### reported

    a =
        \unused ->
            ""

    b unused =
        ""

    c (Variant ( _, { unused } )) =
        ""

    d argument =
        case argument of
            unused ->
                ""

    e =
        let
            { unused } =
                x
        in
        ""

    f =
        let
            unused =
                1
        in
        ""

    type ChoiceTypeUnused
        = VariantUnused

    type alias TypeALiasUnused =
        String

-}
review : Review.Review
review =
    Review.create
        { inspect =
            [ Review.inspectModule moduleToKnowledge
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
    , unusedLetDeclaredValuesAndFunctions :
        List
            { modulePath : String
            , name : String
            , nameRange : Elm.Syntax.Range.Range
            , declarationRemoveRange : Elm.Syntax.Range.Range
            }
    , usedUnqualifiedIdentifiers :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            (FastSet.Set String)
    , moduleLocalValueAndFunctionAndTypeAliasAndChoiceTypeAndVariantDeclarations :
        List
            { modulePath : String
            , moduleName : Elm.Syntax.ModuleName.ModuleName
            , unqualifiedName : String
            , nameRange : Elm.Syntax.Range.Range
            , removeRange : Elm.Syntax.Range.Range
            }
    }


type alias InnerLocalBindingsKnowledge =
    { unqualifiedReferences : FastSet.Set String
    , unusedPatternVariables :
        List
            { variableName : String
            , variableRange : Elm.Syntax.Range.Range
            , fixRange : Elm.Syntax.Range.Range
            , fixReplacement : String
            }
    , unusedLetDeclaredValuesAndFunctions :
        List
            { name : String
            , nameRange : Elm.Syntax.Range.Range
            , declarationRemoveRange : Elm.Syntax.Range.Range
            }
    }


moduleToKnowledge : { info_ | path : String, syntax : Elm.Syntax.File.File } -> Knowledge
moduleToKnowledge moduleData =
    let
        moduleKnowledge : InnerLocalBindingsKnowledge
        moduleKnowledge =
            moduleData.syntax.declarations
                |> flatInnerLocalBindingsKnowledgeMap
                    (\(Elm.Syntax.Node.Node _ declaration) ->
                        case declaration of
                            Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                                valueOrFunctionDeclaration.declaration
                                    |> Elm.Syntax.Node.value
                                    |> .expression
                                    |> expressionToModuleKnowledge
                                    |> moduleKnowledgeAddVariablesUnusedIn
                                        (valueOrFunctionDeclaration.declaration
                                            |> Elm.Syntax.Node.value
                                            |> .arguments
                                            |> List.concatMap Pattern.LocalExtra.variablesAndRanges
                                        )

                            _ ->
                                { unusedPatternVariables = []
                                , unusedLetDeclaredValuesAndFunctions = []
                                , unqualifiedReferences = FastSet.empty
                                }
                    )

        moduleName : Elm.Syntax.ModuleName.ModuleName
        moduleName =
            moduleData.syntax.moduleDefinition |> Elm.Syntax.Node.value |> Elm.Syntax.Module.moduleName

        moduleExposes : { simpleNames : FastSet.Set String, typesWithVariantNames : FastDict.Dict String (FastSet.Set String) }
        moduleExposes =
            moduleData.syntax |> Review.moduleExposes
    in
    { unusedPatternVariables =
        moduleKnowledge.unusedPatternVariables
            |> List.map
                (\unusedVariable ->
                    { modulePath = moduleData.path
                    , name = unusedVariable.variableName
                    , variableRange = unusedVariable.variableRange
                    , fixRange = unusedVariable.fixRange
                    , fixReplacement = unusedVariable.fixReplacement
                    }
                )
    , unusedLetDeclaredValuesAndFunctions =
        moduleKnowledge.unusedLetDeclaredValuesAndFunctions
            |> List.map
                (\unusedBinding ->
                    { modulePath = moduleData.path
                    , name = unusedBinding.name
                    , declarationRemoveRange = unusedBinding.declarationRemoveRange
                    , nameRange = unusedBinding.nameRange
                    }
                )
    , usedUnqualifiedIdentifiers =
        moduleData.syntax.declarations
            |> FastDict.LocalExtra.unionFromListWithMap
                (\(Elm.Syntax.Node.Node _ declaration) ->
                    declaration
                        |> Declaration.LocalExtra.identifierUses
                        |> FastDict.map (\_ ranges -> ranges |> List.length)
                )
                (+)
            |> FastDict.foldl
                (\( qualification, unqualifiedName ) _ soFar ->
                    case qualification of
                        [] ->
                            soFar |> FastSet.insert unqualifiedName

                        _ :: _ ->
                            soFar
                )
                FastSet.empty
            |> FastDict.singleton moduleName
    , moduleLocalValueAndFunctionAndTypeAliasAndChoiceTypeAndVariantDeclarations =
        moduleData.syntax.declarations
            |> List.concatMap
                (\(Elm.Syntax.Node.Node declarationRange declaration) ->
                    case declaration of
                        Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                            let
                                (Elm.Syntax.Node.Node nameRange unqualifiedName) =
                                    valueOrFunctionDeclaration.declaration
                                        |> Elm.Syntax.Node.value
                                        |> .name
                            in
                            if moduleExposes.simpleNames |> FastSet.member unqualifiedName then
                                []

                            else
                                [ { unqualifiedName = unqualifiedName
                                  , nameRange = nameRange
                                  , removeRange =
                                        { start = declarationRange.start
                                        , end = { row = declarationRange.end.row + 1, column = 1 }
                                        }
                                  }
                                ]

                        Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                            if moduleExposes.simpleNames |> FastSet.member (typeAliasDeclaration.name |> Elm.Syntax.Node.value) then
                                []

                            else
                                [ { unqualifiedName = typeAliasDeclaration.name |> Elm.Syntax.Node.value
                                  , nameRange = typeAliasDeclaration.name |> Elm.Syntax.Node.range
                                  , removeRange = declarationRange
                                  }
                                ]

                        Elm.Syntax.Declaration.CustomTypeDeclaration choiceTypeDeclaration ->
                            if moduleExposes.typesWithVariantNames |> FastDict.member (choiceTypeDeclaration.name |> Elm.Syntax.Node.value) then
                                []

                            else
                                case choiceTypeDeclaration.constructors of
                                    [] ->
                                        []

                                    [ Elm.Syntax.Node.Node _ onlyVariant ] ->
                                        if moduleExposes.simpleNames |> FastSet.member (choiceTypeDeclaration.name |> Elm.Syntax.Node.value) then
                                            []

                                        else
                                            [ { unqualifiedName = onlyVariant.name |> Elm.Syntax.Node.value
                                              , nameRange = onlyVariant.name |> Elm.Syntax.Node.range
                                              , removeRange = declarationRange
                                              }
                                            ]

                                    (Elm.Syntax.Node.Node variant0Range variant0) :: variant1Node :: variant2NodeUp ->
                                        (variant1Node :: variant2NodeUp)
                                            |> List.foldl
                                                (\(Elm.Syntax.Node.Node variantRange variant) soFar ->
                                                    { result =
                                                        { unqualifiedName = variant.name |> Elm.Syntax.Node.value
                                                        , nameRange = variant.name |> Elm.Syntax.Node.range
                                                        , removeRange = { start = soFar.variantEndLocation, end = variantRange.end }
                                                        }
                                                            :: soFar.result
                                                    , variantEndLocation = variantRange.end
                                                    }
                                                )
                                                { result =
                                                    [ { unqualifiedName = variant0.name |> Elm.Syntax.Node.value
                                                      , nameRange = variant0.name |> Elm.Syntax.Node.range
                                                      , removeRange =
                                                            { start = variant0Range.start
                                                            , end = variant1Node |> Elm.Syntax.Node.range |> .end
                                                            }
                                                      }
                                                    ]
                                                , variantEndLocation = variant0Range.end
                                                }
                                            |> .result

                        -- not supported
                        Elm.Syntax.Declaration.InfixDeclaration _ ->
                            []

                        Elm.Syntax.Declaration.PortDeclaration _ ->
                            []

                        -- invalid elm
                        Elm.Syntax.Declaration.Destructuring _ _ ->
                            []
                )
            |> List.map
                (\declared ->
                    { moduleName = moduleName
                    , modulePath = moduleData.path
                    , unqualifiedName = declared.unqualifiedName
                    , nameRange = declared.nameRange
                    , removeRange = declared.removeRange
                    }
                )
    }


moduleKnowledgeAddVariablesUnusedIn :
    List
        { variableName : String
        , variableRange : Elm.Syntax.Range.Range
        , fixRange : Elm.Syntax.Range.Range
        , fixReplacement : String
        }
    -> (InnerLocalBindingsKnowledge -> InnerLocalBindingsKnowledge)
moduleKnowledgeAddVariablesUnusedIn outerVariables expressionInnerKnowledge =
    { unqualifiedReferences = expressionInnerKnowledge.unqualifiedReferences
    , unusedPatternVariables =
        (outerVariables
            |> List.filter
                (\variable ->
                    not (expressionInnerKnowledge.unqualifiedReferences |> FastSet.member variable.variableName)
                )
        )
            ++ expressionInnerKnowledge.unusedPatternVariables
    , unusedLetDeclaredValuesAndFunctions =
        expressionInnerKnowledge.unusedLetDeclaredValuesAndFunctions
    }


moduleKnowledgeAddLetDeclaresValuesAndFunctionsUnusedIn :
    List
        { name : String
        , nameRange : Elm.Syntax.Range.Range
        , declarationRemoveRange : Elm.Syntax.Range.Range
        }
    -> (InnerLocalBindingsKnowledge -> InnerLocalBindingsKnowledge)
moduleKnowledgeAddLetDeclaresValuesAndFunctionsUnusedIn outerLetDeclaredValuesAndFunctions expressionInnerKnowledge =
    { unqualifiedReferences = expressionInnerKnowledge.unqualifiedReferences
    , unusedPatternVariables =
        expressionInnerKnowledge.unusedPatternVariables
    , unusedLetDeclaredValuesAndFunctions =
        (outerLetDeclaredValuesAndFunctions
            |> List.filter
                (\variable ->
                    not (expressionInnerKnowledge.unqualifiedReferences |> FastSet.member variable.name)
                )
        )
            ++ expressionInnerKnowledge.unusedLetDeclaredValuesAndFunctions
    }


flatInnerLocalBindingsKnowledgeMap :
    (element -> InnerLocalBindingsKnowledge)
    -> (List element -> InnerLocalBindingsKnowledge)
flatInnerLocalBindingsKnowledgeMap elementToExpressionKnowledge expressionNodeList =
    expressionNodeList
        |> List.foldl
            (\expressionNode soFar ->
                innerLocalBindingsKnowledgeMerge soFar (expressionNode |> elementToExpressionKnowledge)
            )
            { unqualifiedReferences = FastSet.empty
            , unusedPatternVariables = []
            , unusedLetDeclaredValuesAndFunctions = []
            }


innerLocalBindingsKnowledgeMerge : InnerLocalBindingsKnowledge -> InnerLocalBindingsKnowledge -> InnerLocalBindingsKnowledge
innerLocalBindingsKnowledgeMerge knowledge soFar =
    { unqualifiedReferences =
        FastSet.union knowledge.unqualifiedReferences
            soFar.unqualifiedReferences
    , unusedPatternVariables =
        knowledge.unusedPatternVariables
            ++ soFar.unusedPatternVariables
    , unusedLetDeclaredValuesAndFunctions =
        knowledge.unusedLetDeclaredValuesAndFunctions
            ++ soFar.unusedLetDeclaredValuesAndFunctions
    }


expressionToModuleKnowledge :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> InnerLocalBindingsKnowledge
expressionToModuleKnowledge (Elm.Syntax.Node.Node expressionRange expression) =
    case expression of
        Elm.Syntax.Expression.UnitExpr ->
            { unqualifiedReferences = FastSet.empty
            , unusedPatternVariables = []
            , unusedLetDeclaredValuesAndFunctions = []
            }

        Elm.Syntax.Expression.Application subs ->
            subs |> flatInnerLocalBindingsKnowledgeMap expressionToModuleKnowledge

        Elm.Syntax.Expression.OperatorApplication _ _ left right ->
            innerLocalBindingsKnowledgeMerge (left |> expressionToModuleKnowledge) (right |> expressionToModuleKnowledge)

        Elm.Syntax.Expression.FunctionOrValue qualification unqualified ->
            case qualification of
                _ :: _ ->
                    { unqualifiedReferences = FastSet.empty
                    , unusedPatternVariables = []
                    , unusedLetDeclaredValuesAndFunctions = []
                    }

                [] ->
                    { unqualifiedReferences = unqualified |> FastSet.singleton
                    , unusedPatternVariables = []
                    , unusedLetDeclaredValuesAndFunctions = []
                    }

        Elm.Syntax.Expression.IfBlock condition onTrue onFalse ->
            innerLocalBindingsKnowledgeMerge (condition |> expressionToModuleKnowledge)
                (innerLocalBindingsKnowledgeMerge (onTrue |> expressionToModuleKnowledge)
                    (onFalse |> expressionToModuleKnowledge)
                )

        Elm.Syntax.Expression.PrefixOperator _ ->
            { unqualifiedReferences = FastSet.empty
            , unusedPatternVariables = []
            , unusedLetDeclaredValuesAndFunctions = []
            }

        Elm.Syntax.Expression.Operator _ ->
            { unqualifiedReferences = FastSet.empty
            , unusedPatternVariables = []
            , unusedLetDeclaredValuesAndFunctions = []
            }

        Elm.Syntax.Expression.Integer _ ->
            { unqualifiedReferences = FastSet.empty
            , unusedPatternVariables = []
            , unusedLetDeclaredValuesAndFunctions = []
            }

        Elm.Syntax.Expression.Hex _ ->
            { unqualifiedReferences = FastSet.empty
            , unusedPatternVariables = []
            , unusedLetDeclaredValuesAndFunctions = []
            }

        Elm.Syntax.Expression.Floatable _ ->
            { unqualifiedReferences = FastSet.empty
            , unusedPatternVariables = []
            , unusedLetDeclaredValuesAndFunctions = []
            }

        Elm.Syntax.Expression.Negation negated ->
            negated |> expressionToModuleKnowledge

        Elm.Syntax.Expression.Literal _ ->
            { unqualifiedReferences = FastSet.empty
            , unusedPatternVariables = []
            , unusedLetDeclaredValuesAndFunctions = []
            }

        Elm.Syntax.Expression.CharLiteral _ ->
            { unqualifiedReferences = FastSet.empty
            , unusedPatternVariables = []
            , unusedLetDeclaredValuesAndFunctions = []
            }

        Elm.Syntax.Expression.TupledExpression subs ->
            subs |> flatInnerLocalBindingsKnowledgeMap expressionToModuleKnowledge

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            inParens |> expressionToModuleKnowledge

        Elm.Syntax.Expression.LetExpression letIn ->
            let
                destructuredVariables : List { variableName : String, variableRange : Elm.Syntax.Range.Range, fixRange : Elm.Syntax.Range.Range, fixReplacement : String }
                destructuredVariables =
                    letIn.declarations
                        |> List.concatMap
                            (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                case letDeclaration of
                                    Elm.Syntax.Expression.LetFunction _ ->
                                        []

                                    Elm.Syntax.Expression.LetDestructuring pattern _ ->
                                        pattern |> Pattern.LocalExtra.variablesAndRanges
                            )

                declaredNames : List { name : String, nameRange : Elm.Syntax.Range.Range, declarationRemoveRange : Elm.Syntax.Range.Range }
                declaredNames =
                    letIn.declarations
                        |> List.filterMap
                            (\letDeclarationNode ->
                                case letDeclarationNode of
                                    Elm.Syntax.Node.Node letDeclarationRange (Elm.Syntax.Expression.LetFunction letValueOrFunction) ->
                                        let
                                            nameNode : Elm.Syntax.Node.Node String
                                            nameNode =
                                                letValueOrFunction
                                                    |> .declaration
                                                    |> Elm.Syntax.Node.value
                                                    |> .name
                                        in
                                        { name = nameNode |> Elm.Syntax.Node.value
                                        , nameRange = nameNode |> Elm.Syntax.Node.range
                                        , declarationRemoveRange =
                                            case letIn.declarations of
                                                [ _ ] ->
                                                    { start = expressionRange.start
                                                    , end = letIn.expression |> Elm.Syntax.Node.range |> .start
                                                    }

                                                _ ->
                                                    letDeclarationRange
                                        }
                                            |> Just

                                    Elm.Syntax.Node.Node _ (Elm.Syntax.Expression.LetDestructuring _ _) ->
                                        Nothing
                            )
            in
            innerLocalBindingsKnowledgeMerge
                (letIn.expression
                    |> expressionToModuleKnowledge
                )
                (letIn.declarations
                    |> flatInnerLocalBindingsKnowledgeMap
                        (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                            case letDeclaration of
                                Elm.Syntax.Expression.LetFunction letValueOrFunction ->
                                    letValueOrFunction
                                        |> .declaration
                                        |> Elm.Syntax.Node.value
                                        |> .expression
                                        |> expressionToModuleKnowledge
                                        |> moduleKnowledgeAddVariablesUnusedIn
                                            (letValueOrFunction
                                                |> .declaration
                                                |> Elm.Syntax.Node.value
                                                |> .arguments
                                                |> List.concatMap Pattern.LocalExtra.variablesAndRanges
                                            )

                                Elm.Syntax.Expression.LetDestructuring _ destructuredExpression ->
                                    destructuredExpression |> expressionToModuleKnowledge
                        )
                )
                |> moduleKnowledgeAddVariablesUnusedIn destructuredVariables
                |> moduleKnowledgeAddLetDeclaresValuesAndFunctionsUnusedIn declaredNames

        Elm.Syntax.Expression.CaseExpression caseOf ->
            innerLocalBindingsKnowledgeMerge
                (caseOf.expression |> expressionToModuleKnowledge)
                (caseOf.cases
                    |> flatInnerLocalBindingsKnowledgeMap
                        (\( patternInCase, expressionInCase ) ->
                            expressionInCase
                                |> expressionToModuleKnowledge
                                |> moduleKnowledgeAddVariablesUnusedIn
                                    (patternInCase |> Pattern.LocalExtra.variablesAndRanges)
                        )
                )

        Elm.Syntax.Expression.LambdaExpression lambda ->
            lambda.expression
                |> expressionToModuleKnowledge
                |> moduleKnowledgeAddVariablesUnusedIn
                    (lambda
                        |> .args
                        |> List.concatMap Pattern.LocalExtra.variablesAndRanges
                    )

        Elm.Syntax.Expression.RecordExpr fields ->
            fields
                |> flatInnerLocalBindingsKnowledgeMap
                    (\(Elm.Syntax.Node.Node _ ( _, newFieldValue )) ->
                        newFieldValue |> expressionToModuleKnowledge
                    )

        Elm.Syntax.Expression.ListExpr subs ->
            subs |> flatInnerLocalBindingsKnowledgeMap expressionToModuleKnowledge

        Elm.Syntax.Expression.RecordAccess record _ ->
            record |> expressionToModuleKnowledge

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            { unqualifiedReferences = FastSet.empty
            , unusedPatternVariables = []
            , unusedLetDeclaredValuesAndFunctions = []
            }

        Elm.Syntax.Expression.RecordUpdateExpression _ newFields ->
            newFields
                |> flatInnerLocalBindingsKnowledgeMap
                    (\(Elm.Syntax.Node.Node _ ( _, newFieldValue )) ->
                        newFieldValue |> expressionToModuleKnowledge
                    )

        Elm.Syntax.Expression.GLSLExpression _ ->
            { unqualifiedReferences = FastSet.empty
            , unusedPatternVariables = []
            , unusedLetDeclaredValuesAndFunctions = []
            }


knowledgeMerge : Knowledge -> Knowledge -> Knowledge
knowledgeMerge a b =
    { unusedPatternVariables =
        a.unusedPatternVariables ++ b.unusedPatternVariables
    , unusedLetDeclaredValuesAndFunctions =
        a.unusedLetDeclaredValuesAndFunctions
            ++ b.unusedLetDeclaredValuesAndFunctions
    , usedUnqualifiedIdentifiers =
        FastDict.union a.usedUnqualifiedIdentifiers b.usedUnqualifiedIdentifiers
    , moduleLocalValueAndFunctionAndTypeAliasAndChoiceTypeAndVariantDeclarations =
        a.moduleLocalValueAndFunctionAndTypeAliasAndChoiceTypeAndVariantDeclarations
            ++ b.moduleLocalValueAndFunctionAndTypeAliasAndChoiceTypeAndVariantDeclarations
    }


report : Knowledge -> List Review.Error
report knowledge =
    [ knowledge.unusedPatternVariables
        |> List.map
            (\unusedPatternVariable ->
                { path = unusedPatternVariable.modulePath
                , message = "pattern variable " ++ unusedPatternVariable.name ++ " isn't used"
                , details = [ "Maybe you wanted to use this variable for something? If you don't need it, remove the variable here by applying the automatic fix." ]
                , range = unusedPatternVariable.variableRange
                , fix =
                    [ { path = unusedPatternVariable.modulePath
                      , edits =
                            [ Review.replaceRange
                                unusedPatternVariable.fixRange
                                unusedPatternVariable.fixReplacement
                            ]
                      }
                    ]
                }
            )
    , knowledge.unusedLetDeclaredValuesAndFunctions
        |> List.map
            (\unusedLetDeclaredValueOrFunction ->
                { path = unusedLetDeclaredValueOrFunction.modulePath
                , message = "let declared " ++ unusedLetDeclaredValueOrFunction.name ++ " isn't used"
                , details = [ "Maybe you wanted to use it for something? If you don't need it, remove its declaration by applying the automatic fix." ]
                , range = unusedLetDeclaredValueOrFunction.nameRange
                , fix =
                    [ { path = unusedLetDeclaredValueOrFunction.modulePath
                      , edits =
                            [ Review.removeRange
                                unusedLetDeclaredValueOrFunction.declarationRemoveRange
                            ]
                      }
                    ]
                }
            )
    , knowledge.moduleLocalValueAndFunctionAndTypeAliasAndChoiceTypeAndVariantDeclarations
        |> List.filterMap
            (\moduleLocalBinding ->
                case knowledge.usedUnqualifiedIdentifiers |> FastDict.get moduleLocalBinding.moduleName of
                    Nothing ->
                        Nothing

                    Just usedUnqualifiedIdentifiersInModule ->
                        if usedUnqualifiedIdentifiersInModule |> FastSet.member moduleLocalBinding.unqualifiedName then
                            Nothing

                        else
                            { path = moduleLocalBinding.modulePath
                            , message =
                                "declared "
                                    ++ (moduleLocalBinding.moduleName |> String.join ".")
                                    ++ "."
                                    ++ moduleLocalBinding.unqualifiedName
                                    ++ " isn't used"
                            , details =
                                [ "Maybe you wanted to use it for something? If you don't need it, remove its declaration by applying the automatic fix." ]
                            , range = moduleLocalBinding.nameRange
                            , fix =
                                [ { path = moduleLocalBinding.modulePath
                                  , edits = [ Review.removeRange moduleLocalBinding.removeRange ]
                                  }
                                ]
                            }
                                |> Just
            )
    ]
        |> List.concat
