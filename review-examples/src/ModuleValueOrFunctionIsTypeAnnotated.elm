module ModuleValueOrFunctionIsTypeAnnotated exposing (review)

{-|

@docs review

-}

import Dict
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.File
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import FastDict
import FastSet
import Review


{-| Enforce that all top-level value/function declarations have a type annotation.

Type annotations help to quickly understand what's expected in the code, and it will help the compiler give better error messages.

To also report `let in` declarations without a type annotation,
additionally enable [`LetValueOrFunctionIsTypeAnnotated`](LetValueOrFunctionIsTypeAnnotated).


### reported

    a =
        1


### not reported

    b : number
    b =
        let
            c =
                2
        in
        c

-}
review : Review.Review
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
        a.valueAndFunctionDeclarationsWithoutTypeAnnotation
            ++ b.valueAndFunctionDeclarationsWithoutTypeAnnotation
    }


moduleDataToKnowledge : { data_ | path : String, syntax : Elm.Syntax.File.File } -> Knowledge
moduleDataToKnowledge moduleData =
    let
        moduleName : Elm.Syntax.ModuleName.ModuleName
        moduleName =
            moduleData.syntax.moduleDefinition |> Elm.Syntax.Node.value |> Elm.Syntax.Module.moduleName
    in
    { valueAndFunctionDeclarationsWithoutTypeAnnotation =
        moduleData.syntax.declarations
            |> List.filterMap
                (\(Elm.Syntax.Node.Node _ declaration) ->
                    case declaration of
                        Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                            case valueOrFunctionDeclaration.signature of
                                Just _ ->
                                    Nothing

                                Nothing ->
                                    let
                                        (Elm.Syntax.Node.Node nameRange name) =
                                            valueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value |> .name
                                    in
                                    { modulePath = moduleData.path
                                    , moduleName = moduleName
                                    , name = name
                                    , nameRange = nameRange
                                    }
                                        |> Just

                        _ ->
                            Nothing
                )
    }


report : Knowledge -> List Review.Error
report knowledge =
    knowledge.valueAndFunctionDeclarationsWithoutTypeAnnotation
        |> List.map
            (\valueOrFunctionDeclarationWithoutTypeAnnotation ->
                { path = valueOrFunctionDeclarationWithoutTypeAnnotation.modulePath
                , message =
                    "declaration "
                        ++ (valueOrFunctionDeclarationWithoutTypeAnnotation.moduleName |> String.join ".")
                        ++ "."
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
