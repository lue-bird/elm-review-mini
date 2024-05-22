module FastDict.LocalExtra exposing (excludeKeys, firstJustMap, justsToListMap, justsToSetMap, keys, toListMap, unionFromListMap, unionFromListWithMap, unionToSetMap, unionWith)

import FastDict
import Set exposing (Set)


unionFromListWithMap :
    (element -> FastDict.Dict comparableResultKey resultValue)
    -> (resultValue -> resultValue -> resultValue)
    ->
        (List element
         -> FastDict.Dict comparableResultKey resultValue
        )
unionFromListWithMap elementToDict resultValueMerge =
    \dict ->
        dict
            |> List.foldl
                (\element soFar ->
                    unionWith resultValueMerge (element |> elementToDict) soFar
                )
                FastDict.empty


unionFromListMap :
    (element -> FastDict.Dict comparableKey value)
    -> (List element -> FastDict.Dict comparableKey value)
unionFromListMap elementToDict =
    \list ->
        list
            |> List.foldl
                (\element soFar ->
                    FastDict.union (element |> elementToDict) soFar
                )
                FastDict.empty


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


unionToSetMap : (key -> value -> Set comparableElement) -> (FastDict.Dict key value -> Set comparableElement)
unionToSetMap keyValueToMaybeElement =
    \fastDict ->
        fastDict
            |> FastDict.foldl
                (\key value soFar ->
                    Set.union (keyValueToMaybeElement key value) soFar
                )
                Set.empty


toSetMap : (key -> value -> comparableElement) -> (FastDict.Dict key value -> Set comparableElement)
toSetMap keyValueToMaybeElement =
    \fastDict ->
        fastDict
            |> FastDict.foldl
                (\key value soFar ->
                    soFar |> Set.insert (keyValueToMaybeElement key value)
                )
                Set.empty


keys : FastDict.Dict comparableKey value_ -> Set comparableKey
keys =
    \fastDict ->
        fastDict |> toSetMap (\key _ -> key)


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


toListMap : (key -> value -> element) -> (FastDict.Dict key value -> List element)
toListMap keyValueToElement =
    \dict ->
        dict
            |> FastDict.foldr
                (\key value soFar ->
                    soFar |> (::) (keyValueToElement key value)
                )
                []


excludeKeys :
    Set comparableKey
    -> (FastDict.Dict comparableKey value -> FastDict.Dict comparableKey value)
excludeKeys keysToRemove =
    \dict ->
        dict
            |> FastDict.filter
                (\key _ -> not (keysToRemove |> Set.member key))
