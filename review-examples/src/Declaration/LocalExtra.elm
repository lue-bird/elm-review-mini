module Declaration.LocalExtra exposing (identifierUses)

import Elm.Syntax.Declaration
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Expression.LocalExtra
import FastDict
import FastDict.LocalExtra
import Pattern.LocalExtra
import Set exposing (Set)
import Set.LocalExtra
import Type.LocalExtra


{-| Declared name (+ possible variant names)
-}
names : Elm.Syntax.Declaration.Declaration -> Set String
names =
    \declaration ->
        case declaration of
            Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
                functionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .name
                    |> Elm.Syntax.Node.value
                    |> Set.singleton

            Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                typeAliasDeclaration.name |> Elm.Syntax.Node.value |> Set.singleton

            Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
                variantType.constructors
                    |> List.map (\(Elm.Syntax.Node.Node _ variant) -> variant.name |> Elm.Syntax.Node.value)
                    |> Set.fromList
                    |> Set.insert (variantType.name |> Elm.Syntax.Node.value)

            Elm.Syntax.Declaration.PortDeclaration signature ->
                signature.name |> Elm.Syntax.Node.value |> Set.singleton

            Elm.Syntax.Declaration.InfixDeclaration infixDeclaration ->
                infixDeclaration.operator |> Elm.Syntax.Node.value |> Set.singleton

            -- invalid
            Elm.Syntax.Declaration.Destructuring _ _ ->
                Set.empty


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
                            |> Set.LocalExtra.unionFromListMap Pattern.LocalExtra.variables
                            |> Set.map (\unqualified -> ( [], unqualified ))
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
