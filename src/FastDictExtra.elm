module FastDictExtra exposing (concatToListMap, fromListMap)

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
