module Type.LocalExtra exposing (referenceUseCounts)

import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.TypeAnnotation
import FastDict
import FastDict.LocalExtra


referenceUseCountsMerge :
    FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
referenceUseCountsMerge a b =
    FastDict.LocalExtra.unionWith (\aCount bCount -> aCount + bCount) a b


referenceUseCounts :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
referenceUseCounts =
    \(Elm.Syntax.Node.Node _ type_) ->
        case type_ of
            Elm.Syntax.TypeAnnotation.GenericType _ ->
                FastDict.empty

            Elm.Syntax.TypeAnnotation.Unit ->
                FastDict.empty

            Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
                referenceUseCountsMerge (input |> referenceUseCounts) (output |> referenceUseCounts)

            Elm.Syntax.TypeAnnotation.Tupled parts ->
                parts |> listReferenceUseCounts

            Elm.Syntax.TypeAnnotation.Record fields ->
                fields |> List.map (\(Elm.Syntax.Node.Node _ ( _, fieldValue )) -> fieldValue) |> listReferenceUseCounts

            Elm.Syntax.TypeAnnotation.GenericRecord _ (Elm.Syntax.Node.Node _ fields) ->
                fields |> List.map (\(Elm.Syntax.Node.Node _ ( _, fieldValue )) -> fieldValue) |> listReferenceUseCounts

            Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ ( moduleName, unqualifiedName )) arguments ->
                arguments
                    |> listReferenceUseCounts
                    |> referenceUseCountsMerge
                        (FastDict.singleton ( moduleName, unqualifiedName ) 1)


listReferenceUseCounts :
    List (Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation)
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
listReferenceUseCounts =
    \patternNodeList ->
        patternNodeList
            |> List.foldl (\sub -> referenceUseCountsMerge (sub |> referenceUseCounts)) FastDict.empty
