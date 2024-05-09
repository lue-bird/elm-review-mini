module Pattern.LocalExtra exposing (listReferenceUseCounts, nodeVariables, referenceUseCounts)

import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import FastDict
import FastDict.LocalExtra
import Set exposing (Set)
import Set.LocalExtra


referenceUseCountsMerge :
    FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
referenceUseCountsMerge a b =
    FastDict.LocalExtra.unionWith (\aCount bCount -> aCount + bCount) a b


listReferenceUseCounts :
    List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
listReferenceUseCounts =
    \patternNodeList ->
        patternNodeList
            |> List.foldl (\sub -> referenceUseCountsMerge (sub |> referenceUseCounts)) FastDict.empty


referenceUseCounts :
    Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) Int
referenceUseCounts =
    -- IGNORE TCO
    \(Elm.Syntax.Node.Node _ pattern) ->
        case pattern of
            Elm.Syntax.Pattern.AllPattern ->
                FastDict.empty

            Elm.Syntax.Pattern.UnitPattern ->
                FastDict.empty

            Elm.Syntax.Pattern.CharPattern _ ->
                FastDict.empty

            Elm.Syntax.Pattern.StringPattern _ ->
                FastDict.empty

            Elm.Syntax.Pattern.IntPattern _ ->
                FastDict.empty

            Elm.Syntax.Pattern.HexPattern _ ->
                FastDict.empty

            Elm.Syntax.Pattern.FloatPattern _ ->
                FastDict.empty

            Elm.Syntax.Pattern.VarPattern _ ->
                FastDict.empty

            Elm.Syntax.Pattern.RecordPattern _ ->
                FastDict.empty

            Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
                inParens |> referenceUseCounts

            Elm.Syntax.Pattern.AsPattern aliased _ ->
                aliased |> referenceUseCounts

            Elm.Syntax.Pattern.UnConsPattern head tail ->
                referenceUseCountsMerge (tail |> referenceUseCounts) (head |> referenceUseCounts)

            Elm.Syntax.Pattern.TuplePattern parts ->
                parts |> listReferenceUseCounts

            Elm.Syntax.Pattern.ListPattern elements ->
                elements |> listReferenceUseCounts

            Elm.Syntax.Pattern.NamedPattern fullyQualified arguments ->
                arguments
                    |> listReferenceUseCounts
                    |> referenceUseCountsMerge
                        (FastDict.singleton ( fullyQualified.moduleName, fullyQualified.name ) 1)


{-| Recursively find all bindings in a pattern.
-}
nodeVariables : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern -> Set String
nodeVariables =
    \(Elm.Syntax.Node.Node _ pattern) -> pattern |> variables


variables : Elm.Syntax.Pattern.Pattern -> Set String
variables =
    -- IGNORE TCO
    \pattern ->
        case pattern of
            Elm.Syntax.Pattern.VarPattern name ->
                name |> Set.singleton

            Elm.Syntax.Pattern.AsPattern afterAsPattern (Elm.Syntax.Node.Node _ name) ->
                Set.insert name (afterAsPattern |> nodeVariables)

            Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
                inParens |> nodeVariables

            Elm.Syntax.Pattern.ListPattern patterns ->
                patterns |> Set.LocalExtra.unionFromListMap nodeVariables

            Elm.Syntax.Pattern.TuplePattern patterns ->
                patterns |> Set.LocalExtra.unionFromListMap nodeVariables

            Elm.Syntax.Pattern.RecordPattern patterns ->
                patterns |> Set.LocalExtra.fromListMap (\(Elm.Syntax.Node.Node _ name) -> name)

            Elm.Syntax.Pattern.NamedPattern _ patterns ->
                patterns |> Set.LocalExtra.unionFromListMap nodeVariables

            Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
                Set.union (tailPattern |> nodeVariables) (headPattern |> nodeVariables)

            Elm.Syntax.Pattern.AllPattern ->
                Set.empty

            Elm.Syntax.Pattern.UnitPattern ->
                Set.empty

            Elm.Syntax.Pattern.CharPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.StringPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.IntPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.HexPattern _ ->
                Set.empty

            Elm.Syntax.Pattern.FloatPattern _ ->
                Set.empty
