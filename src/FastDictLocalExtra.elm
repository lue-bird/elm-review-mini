module FastDictLocalExtra exposing (all, concatToListMap, fromListMap, unionWith)

import FastDict


fromListMap :
    (element -> { key : comparableKey, value : value })
    -> (List element -> FastDict.Dict comparableKey value)
fromListMap elementToEntry =
    \list ->
        list
            |> List.foldl
                (\element soFar ->
                    let
                        newEntry : { key : comparableKey, value : value }
                        newEntry =
                            element |> elementToEntry
                    in
                    soFar |> FastDict.insert newEntry.key newEntry.value
                )
                FastDict.empty


concatToListMap :
    (key -> value -> List element)
    -> (FastDict.Dict key value -> List element)
concatToListMap keyValueToElement =
    \dict ->
        dict
            |> FastDict.foldr
                (\key value soFar ->
                    keyValueToElement key value ++ soFar
                )
                []


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


all : (key -> value -> Bool) -> FastDict.Dict key value -> Bool
all isOkay =
    FastDict.restructure True
        (\state ->
            isOkay state.key state.value && state.left () && state.right ()
        )
