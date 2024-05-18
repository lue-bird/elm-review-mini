module Pattern.LocalExtra exposing (identifierUses, listIdentifierUses, variables, variablesAndRanges)

import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import FastDict
import FastDict.LocalExtra
import List.LocalExtra
import Review
import Set exposing (Set)
import Set.LocalExtra


identifierUsesMerge :
    FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
identifierUsesMerge a b =
    FastDict.LocalExtra.unionWith (\aRanges bRanges -> aRanges ++ bRanges) a b


listIdentifierUses :
    List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
listIdentifierUses =
    \patternNodeList ->
        patternNodeList
            |> List.foldl (\sub -> identifierUsesMerge (sub |> identifierUses)) FastDict.empty


identifierUses :
    Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> FastDict.Dict ( Elm.Syntax.ModuleName.ModuleName, String ) (List Elm.Syntax.Range.Range)
identifierUses =
    -- IGNORE TCO
    \(Elm.Syntax.Node.Node patternRange pattern) ->
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
                inParens |> identifierUses

            Elm.Syntax.Pattern.AsPattern aliased _ ->
                aliased |> identifierUses

            Elm.Syntax.Pattern.UnConsPattern head tail ->
                identifierUsesMerge (tail |> identifierUses) (head |> identifierUses)

            Elm.Syntax.Pattern.TuplePattern parts ->
                parts |> listIdentifierUses

            Elm.Syntax.Pattern.ListPattern elements ->
                elements |> listIdentifierUses

            Elm.Syntax.Pattern.NamedPattern fullyQualified arguments ->
                arguments
                    |> listIdentifierUses
                    |> identifierUsesMerge
                        (FastDict.singleton ( fullyQualified.moduleName, fullyQualified.name )
                            [ { start = patternRange.start
                              , end =
                                    { row = patternRange.end.row
                                    , column =
                                        patternRange.start.column
                                            + (( fullyQualified.moduleName, fullyQualified.name ) |> qualifiedToString |> String.length)
                                    }
                              }
                            ]
                        )


qualifiedToString : ( Elm.Syntax.ModuleName.ModuleName, String ) -> String
qualifiedToString =
    \( qualification, unqualified ) ->
        case qualification of
            [] ->
                unqualified

            qualificationPart0 :: qualificationPart1Up ->
                ((qualificationPart0 :: qualificationPart1Up) |> String.join ".")
                    ++ "."
                    ++ unqualified


{-| Recursively find all bindings in the pattern
-}
variables : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern -> Set String
variables =
    \patternNode -> patternNode |> variablesAndRanges |> List.LocalExtra.toSetMap (\variable -> variable.variableName)


{-| Recursively find all bindings + ranges in the pattern
-}
variablesAndRanges :
    Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    -> List { variableName : String, variableRange : Elm.Syntax.Range.Range, fixRange : Elm.Syntax.Range.Range, fixReplacement : String }
variablesAndRanges =
    -- IGNORE TCO
    \(Elm.Syntax.Node.Node patternRange pattern) ->
        case pattern of
            Elm.Syntax.Pattern.VarPattern name ->
                [ { variableName = name, variableRange = patternRange, fixRange = patternRange, fixReplacement = "_" } ]

            Elm.Syntax.Pattern.AsPattern afterAsPattern (Elm.Syntax.Node.Node aliasNameRange name) ->
                { variableName = name
                , variableRange = aliasNameRange
                , fixReplacement = ""
                , fixRange = { start = afterAsPattern |> Elm.Syntax.Node.range |> .end, end = aliasNameRange.end }
                }
                    :: (afterAsPattern |> variablesAndRanges)

            Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
                inParens |> variablesAndRanges

            Elm.Syntax.Pattern.ListPattern patterns ->
                patterns |> List.concatMap variablesAndRanges

            Elm.Syntax.Pattern.TuplePattern patterns ->
                patterns |> List.concatMap variablesAndRanges

            Elm.Syntax.Pattern.RecordPattern fields ->
                case fields of
                    [] ->
                        []

                    [ Elm.Syntax.Node.Node onlyFieldRange onlyFieldName ] ->
                        [ { variableName = onlyFieldName
                          , variableRange = onlyFieldRange
                          , fixReplacement = "_"
                          , fixRange = patternRange
                          }
                        ]

                    (Elm.Syntax.Node.Node field0Range field0Name) :: (Elm.Syntax.Node.Node field1Range field1Name) :: field2Up ->
                        (Elm.Syntax.Node.Node field1Range field1Name :: field2Up)
                            -- take ranges starting at previous comma
                            |> List.foldl
                                (\(Elm.Syntax.Node.Node fieldRange fieldName) soFar ->
                                    { previousEnd = fieldRange.end
                                    , variables =
                                        soFar.variables
                                            |> (::)
                                                { variableName = fieldName
                                                , variableRange = fieldRange
                                                , fixRange =
                                                    { start = soFar.previousEnd
                                                    , end = fieldRange.end
                                                    }
                                                , fixReplacement = ""
                                                }
                                    }
                                )
                                { previousEnd = field0Range.end, variables = [] }
                            |> .variables
                            |> (::)
                                { variableName = field0Name
                                , variableRange = field0Range
                                , fixRange = { start = field0Range.start, end = field1Range.start }
                                , fixReplacement = ""
                                }

            Elm.Syntax.Pattern.NamedPattern _ patterns ->
                patterns |> List.concatMap variablesAndRanges

            Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
                (tailPattern |> variablesAndRanges) ++ (headPattern |> variablesAndRanges)

            Elm.Syntax.Pattern.AllPattern ->
                []

            Elm.Syntax.Pattern.UnitPattern ->
                []

            Elm.Syntax.Pattern.CharPattern _ ->
                []

            Elm.Syntax.Pattern.StringPattern _ ->
                []

            Elm.Syntax.Pattern.IntPattern _ ->
                []

            Elm.Syntax.Pattern.HexPattern _ ->
                []

            Elm.Syntax.Pattern.FloatPattern _ ->
                []
