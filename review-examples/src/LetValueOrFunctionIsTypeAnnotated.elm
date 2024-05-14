module LetValueOrFunctionIsTypeAnnotated exposing (review)

{-|

@docs review

-}

import Dict
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression exposing (LetDeclaration)
import Elm.Syntax.File
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import FastDict
import Review exposing (Review)
import Set exposing (Set)


{-| Enforces that `let in` value/function declarations have a type annotation.

Type annotations help to quickly understand what's expected in the code, and it will help the compiler give better error messages.

This rule does not report _top-level_ declarations without a type annotation.
For that, enable [`ModuleValueOrFunctionIsTypeAnnotated`](ModuleValueOrFunctionIsTypeAnnotated).


## reported

    a : number
    a =
        let
            b =
                2
        in
        b


## not reported

    a =
        let
            b : number
            b =
                2
        in
        b

-}
review : Review
review =
    Review.create
        { inspect =
            [ Review.inspectModule moduleDataToKnowledge
            ]
        , knowledgeMerge = knowledgeMerge
        , report = report
        }


type alias Knowledge =
    { valueAndFunctionDeclarationsWithoutTypeAnnotation :
        List
            { modulePath : String
            , moduleName : Elm.Syntax.ModuleName.ModuleName
            , name : String
            , nameRange : Elm.Syntax.Range.Range
            }
    }


knowledgeMerge : Knowledge -> Knowledge -> Knowledge
knowledgeMerge a b =
    { valueAndFunctionDeclarationsWithoutTypeAnnotation =
        a.valueAndFunctionDeclarationsWithoutTypeAnnotation ++ b.valueAndFunctionDeclarationsWithoutTypeAnnotation
    }


moduleDataToKnowledge : { data_ | path : String, syntax : Elm.Syntax.File.File } -> Knowledge
moduleDataToKnowledge moduleData =
    let
        moduleName : Elm.Syntax.ModuleName.ModuleName
        moduleName =
            moduleData.syntax.moduleDefinition |> Review.moduleHeaderNameNode |> Elm.Syntax.Node.value

        letDeclarationToKnowledge :
            LetDeclaration
            ->
                Maybe
                    { modulePath : String
                    , moduleName : Elm.Syntax.ModuleName.ModuleName
                    , name : String
                    , nameRange : Elm.Syntax.Range.Range
                    }
        letDeclarationToKnowledge letDeclaration =
            case letDeclaration of
                Elm.Syntax.Expression.LetDestructuring _ _ ->
                    Nothing

                Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                    case letValueOrFunctionDeclaration.signature of
                        Just _ ->
                            Nothing

                        Nothing ->
                            let
                                (Elm.Syntax.Node.Node nameRange name) =
                                    letValueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value |> .name
                            in
                            { modulePath = moduleData.path
                            , moduleName = moduleName
                            , name = name
                            , nameRange = nameRange
                            }
                                |> Just
    in
    { valueAndFunctionDeclarationsWithoutTypeAnnotation =
        moduleData.syntax.declarations
            |> List.concatMap
                (\(Elm.Syntax.Node.Node _ declaration) ->
                    case declaration of
                        Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                            (valueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value |> .expression)
                                |> Review.expressionFold
                                    (\(Elm.Syntax.Node.Node _ expression) soFar ->
                                        case expression of
                                            Elm.Syntax.Expression.LetExpression letIn ->
                                                (letIn.declarations
                                                    |> List.filterMap
                                                        (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                                            letDeclaration |> letDeclarationToKnowledge
                                                        )
                                                )
                                                    ++ soFar

                                            _ ->
                                                soFar
                                    )
                                    []

                        _ ->
                            []
                )
    }


report : Knowledge -> List Review.Error
report knowledge =
    knowledge.valueAndFunctionDeclarationsWithoutTypeAnnotation
        |> List.map
            (\valueOrFunctionDeclarationWithoutTypeAnnotation ->
                { path = valueOrFunctionDeclarationWithoutTypeAnnotation.modulePath
                , message =
                    "let declaration "
                        ++ valueOrFunctionDeclarationWithoutTypeAnnotation.name
                        ++ " has no type annotation"
                , details =
                    [ "Type annotations help to quickly understand what's expected in the code, and it will help the compiler give better error messages."
                    , "Try adding a line "
                        ++ valueOrFunctionDeclarationWithoutTypeAnnotation.name
                        ++ " : ItsType above the declaration. If you don't know the type, you can start by using the \"infer type\" feature of your IDE or inserting the type like `Never` and letting the compiler fill you in on the details."
                    ]
                , range = valueOrFunctionDeclarationWithoutTypeAnnotation.nameRange
                , fix = []
                }
            )
