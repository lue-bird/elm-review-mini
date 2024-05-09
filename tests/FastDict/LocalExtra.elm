module FastDict.LocalExtra exposing (firstJustMap, justsToListMap, justsToSetMap, keys, unionWith)

import FastDict
import Set exposing (Set)


unionWith :
    (v -> v -> v)
    -> FastDict.Dict comparable v
    -> FastDict.Dict comparable v
    -> FastDict.Dict comparable v
unionWith valueABMerge a b =
    FastDict.merge
        FastDict.insert
        (\reference aValue bValue soFar ->
            soFar |> FastDict.insert reference (valueABMerge aValue bValue)
        )
        FastDict.insert
        a
        b
        FastDict.empty


justsToListMap : (key -> value -> Maybe element) -> (FastDict.Dict key value -> List element)
justsToListMap keyValueToMaybeElement =
    \fastDict ->
        fastDict
            |> FastDict.foldl
                (\key value soFar ->
                    case keyValueToMaybeElement key value of
                        Nothing ->
                            soFar

                        Just element ->
                            soFar |> (::) element
                )
                []


justsToSetMap : (key -> value -> Maybe comparableElement) -> (FastDict.Dict key value -> Set comparableElement)
justsToSetMap keyValueToMaybeElement =
    \fastDict ->
        fastDict
            |> FastDict.foldl
                (\key value soFar ->
                    case keyValueToMaybeElement key value of
                        Nothing ->
                            soFar

                        Just element ->
                            soFar |> Set.insert element
                )
                Set.empty


keys : FastDict.Dict comparableKey value_ -> Set comparableKey
keys =
    \fastDict ->
        fastDict
            |> FastDict.foldl
                (\key _ soFar -> soFar |> Set.insert key)
                Set.empty


firstJustMap : (key -> value -> Maybe found) -> FastDict.Dict key value -> Maybe found
firstJustMap keyValueToMaybeFound =
    \fastDict ->
        fastDict
            |> FastDict.stoppableFoldl
                (\key value _ ->
                    case keyValueToMaybeFound key value of
                        Nothing ->
                            FastDict.Continue Nothing

                        Just found ->
                            FastDict.Stop (Just found)
                )
                Nothing
