module Declaration.LocalExtra exposing (declaredNames, identifierUses)

import Elm.Syntax.Declaration
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Expression.LocalExtra
import FastDict
import FastDict.LocalExtra
import FastSet
import FastSet.LocalExtra
import Pattern.LocalExtra
import Type.LocalExtra


{-| Declared names (+ possible variant names)
-}
declaredNames : Elm.Syntax.Declaration.Declaration -> FastSet.Set String
declaredNames =
    \declaration ->
        case declaration of
            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                functionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .name
                    |> Elm.Syntax.Node.value
                    |> FastSet.singleton

            Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                typeAliasDeclaration.name |> Elm.Syntax.Node.value |> FastSet.singleton

            Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
                variantType.constructors
                    |> List.map (\(Elm.Syntax.Node.Node _ variant) -> variant.name |> Elm.Syntax.Node.value)
                    |> FastSet.fromList
                    |> FastSet.insert (variantType.name |> Elm.Syntax.Node.value)

            Elm.Syntax.Declaration.PortDeclaration signature ->
                signature.name |> Elm.Syntax.Node.value |> FastSet.singleton

            Elm.Syntax.Declaration.InfixDeclaration infixDeclaration ->
                infixDeclaration.operator |> Elm.Syntax.Node.value |> FastSet.singleton

            -- invalid
            Elm.Syntax.Declaration.Destructuring _ _ ->
                FastSet.empty


identifierUses :
    Elm.Syntax.Declaration.Declaration
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
identifierUses =
    \declaration ->
        case declaration of
            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                let
                    argumentPatterns : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
                    argumentPatterns =
                        functionDeclaration.declaration
                            |> Elm.Syntax.Node.value
                            |> .arguments
                in
                [ case functionDeclaration.signature of
                    Nothing ->
                        FastDict.empty

                    Just (Elm.Syntax.Node.Node _ signature) ->
                        signature
                            |> .typeAnnotation
                            |> Type.LocalExtra.identifierUses
                , functionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .expression
                    |> Expression.LocalExtra.identifiers
                    |> FastDict.LocalExtra.excludeKeys
                        (argumentPatterns
                            |> FastSet.LocalExtra.unionFromListMap Pattern.LocalExtra.variables
                            |> FastSet.map (\unqualified -> ( [], unqualified ))
                        )
                , argumentPatterns
                    |> Pattern.LocalExtra.listIdentifierUses
                ]
                    |> FastDict.LocalExtra.unionFromListWithMap identity (++)

            Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                typeAliasDeclaration.typeAnnotation |> Type.LocalExtra.identifierUses

            Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
                variantType.constructors
                    |> List.concatMap (\(Elm.Syntax.Node.Node _ variant) -> variant.arguments)
                    |> FastDict.LocalExtra.unionFromListWithMap Type.LocalExtra.identifierUses (++)

            Elm.Syntax.Declaration.PortDeclaration signature ->
                signature.typeAnnotation |> Type.LocalExtra.identifierUses

            -- not supported
            Elm.Syntax.Declaration.InfixDeclaration _ ->
                FastDict.empty

            -- invalid
            Elm.Syntax.Declaration.Destructuring _ _ ->
                FastDict.empty
