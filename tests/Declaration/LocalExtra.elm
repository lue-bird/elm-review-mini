module Declaration.LocalExtra exposing (listReferenceCountUses)

import Elm.Syntax.Declaration
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Expression.LocalExtra
import FastDict
import FastDict.LocalExtra
import Pattern.LocalExtra
import Set exposing (Set)
import Set.LocalExtra
import Type.LocalExtra


listReferenceUseCountsMerge :
    List (FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int)
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
listReferenceUseCountsMerge =
    \referenceUseCountsList ->
        referenceUseCountsList
            |> List.foldl (\sub -> referenceUseCountsMerge sub) FastDict.empty


referenceUseCountsMerge :
    FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
referenceUseCountsMerge a b =
    FastDict.LocalExtra.unionWith (\aCount bCount -> aCount + bCount) a b


listReferenceCountUses :
    List (Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration)
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
listReferenceCountUses =
    \declarations ->
        let
            declarationListNames : Set String
            declarationListNames =
                declarations |> Set.LocalExtra.unionFromListMap (\(Elm.Syntax.Node.Node _ declaration) -> declaration |> names)
        in
        declarations
            |> List.map
                (\(Elm.Syntax.Node.Node _ declaration) ->
                    declaration |> referenceUseCountsWithBranchLocalVariables declarationListNames
                )
            |> listReferenceUseCountsMerge


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


referenceUseCountsWithBranchLocalVariables :
    Set String
    -> (Elm.Syntax.Declaration.Declaration -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int)
referenceUseCountsWithBranchLocalVariables branchLocalVariables =
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
                            |> Type.LocalExtra.referenceUseCounts
                , functionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .expression
                    |> Expression.LocalExtra.referenceUseCountsWithBranchLocalVariables
                        (Set.union branchLocalVariables
                            (argumentPatterns |> Set.LocalExtra.unionFromListMap Pattern.LocalExtra.nodeVariables)
                        )
                , argumentPatterns
                    |> Pattern.LocalExtra.listReferenceUseCounts
                ]
                    |> listReferenceUseCountsMerge

            Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
                typeAliasDeclaration.typeAnnotation |> Type.LocalExtra.referenceUseCounts

            Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
                variantType.constructors
                    |> List.concatMap (\(Elm.Syntax.Node.Node _ variant) -> variant.arguments)
                    |> List.map Type.LocalExtra.referenceUseCounts
                    |> listReferenceUseCountsMerge

            Elm.Syntax.Declaration.PortDeclaration signature ->
                signature.typeAnnotation |> Type.LocalExtra.referenceUseCounts

            -- not supported
            Elm.Syntax.Declaration.InfixDeclaration _ ->
                FastDict.empty

            -- invalid
            Elm.Syntax.Declaration.Destructuring _ _ ->
                FastDict.empty
