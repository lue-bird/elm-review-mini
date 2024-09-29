module FastDictLocalExtra exposing (concatToListMap, firstJustMap, fromListMap, unionWith)

import FastDict


fromListMap :
    (element -> { key : comparableKey, value : value })
    -> (List element -> FastDict.Dict comparableKey value)
fromListMap elementToEntry list =
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
concatToListMap keyValueToElement dict =
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
unionWith valueABMerge aDict bDict =
    if (aDict |> FastDict.size) > (bDict |> FastDict.size) then
        FastDict.foldl
            (\key b soFar ->
                soFar
                    |> FastDict.update key
                        (\existingValueAtKey ->
                            case existingValueAtKey of
                                Nothing ->
                                    b |> Just

                                Just a ->
                                    valueABMerge a b |> Just
                        )
            )
            aDict
            bDict

    else
        FastDict.foldl
            (\key a soFar ->
                soFar
                    |> FastDict.update key
                        (\existingValueAtKey ->
                            case existingValueAtKey of
                                Nothing ->
                                    a |> Just

                                Just b ->
                                    valueABMerge a b |> Just
                        )
            )
            bDict
            aDict





firstJustMap : (key -> value -> Maybe found) -> FastDict.Dict key value -> Maybe found
firstJustMap keyValueToMaybeFound fastDict =
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
