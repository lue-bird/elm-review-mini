module List.LocalExtra exposing (firstJustMap, justsToSetMap, toSetMap, unionToSetMap)

import FastSet


toSetMap : (element -> comparableSetElement) -> (List element -> FastSet.Set comparableSetElement)
toSetMap keyValueToMaybeElement fastDict =
    fastDict
        |> List.foldl
            (\element soFar ->
                soFar |> FastSet.insert (keyValueToMaybeElement element)
            )
            FastSet.empty


justsToSetMap : (element -> Maybe comparableSetElement) -> (List element -> FastSet.Set comparableSetElement)
justsToSetMap keyValueToMaybeSetElement fastDict =
    fastDict
        |> List.foldl
            (\element soFar ->
                case keyValueToMaybeSetElement element of
                    Nothing ->
                        soFar

                    Just setElement ->
                        soFar |> FastSet.insert setElement
            )
            FastSet.empty


unionToSetMap : (element -> FastSet.Set comparableSetElement) -> (List element -> FastSet.Set comparableSetElement)
unionToSetMap keyValueToMaybeSetElement fastDict =
    fastDict
        |> List.foldl
            (\element soFar ->
                FastSet.union (keyValueToMaybeSetElement element) soFar
            )
            FastSet.empty


firstJustMap : (element -> Maybe found) -> (List element -> Maybe found)
firstJustMap elementToMaybeFound list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case head |> elementToMaybeFound of
                Nothing ->
                    tail |> firstJustMap elementToMaybeFound

                Just found ->
                    found |> Just
