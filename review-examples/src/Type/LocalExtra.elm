module Type.LocalExtra exposing (identifierUses)

import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import FastDict
import FastDict.LocalExtra


referenceUsesMerge :
    FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
referenceUsesMerge a b =
    FastDict.LocalExtra.unionWith (\aRanges bRanges -> aRanges ++ bRanges) a b


identifierUses :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
identifierUses =
    \(Elm.Syntax.Node.Node _ type_) ->
        case type_ of
            Elm.Syntax.TypeAnnotation.GenericType _ ->
                FastDict.empty

            Elm.Syntax.TypeAnnotation.Unit ->
                FastDict.empty

            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
                referenceUsesMerge (input |> identifierUses) (output |> identifierUses)

            Elm.Syntax.TypeAnnotation.Tupled parts ->
                parts |> listReferenceUses

            Elm.Syntax.TypeAnnotation.Record fields ->
                fields |> List.map (\(Elm.Syntax.Node.Node _ ( _, fieldValue )) -> fieldValue) |> listReferenceUses

            Elm.Syntax.TypeAnnotation.GenericRecord _ (Elm.Syntax.Node.Node _ fields) ->
                fields |> List.map (\(Elm.Syntax.Node.Node _ ( _, fieldValue )) -> fieldValue) |> listReferenceUses

            Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node referenceRange ( moduleName, unqualifiedName )) arguments ->
                arguments
                    |> listReferenceUses
                    |> referenceUsesMerge
                        (FastDict.singleton ( moduleName, unqualifiedName ) [ referenceRange ])


listReferenceUses :
    List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
listReferenceUses =
    \patternNodeList ->
        patternNodeList
            |> List.foldl (\sub -> referenceUsesMerge (sub |> identifierUses)) FastDict.empty
