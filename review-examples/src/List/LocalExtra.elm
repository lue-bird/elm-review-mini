module List.LocalExtra exposing (firstJustMap, justsToSetMap, toSetMap, unionToSetMap)

import Set exposing (Set)


toSetMap : (element -> comparableSetElement) -> (List element -> Set comparableSetElement)
toSetMap keyValueToMaybeElement =
    \fastDict ->
        fastDict
            |> List.foldl
                (\element soFar ->
                    soFar |> Set.insert (keyValueToMaybeElement element)
                )
                Set.empty


justsToSetMap : (element -> Maybe comparableSetElement) -> (List element -> Set comparableSetElement)
justsToSetMap keyValueToMaybeSetElement =
    \fastDict ->
        fastDict
            |> List.foldl
                (\element soFar ->
                    case keyValueToMaybeSetElement element of
                        Nothing ->
                            soFar

                        Just setElement ->
                            soFar |> Set.insert setElement
                )
                Set.empty


unionToSetMap : (element -> Set comparableSetElement) -> (List element -> Set comparableSetElement)
unionToSetMap keyValueToMaybeSetElement =
    \fastDict ->
        fastDict
            |> List.foldl
                (\element soFar ->
                    Set.union (keyValueToMaybeSetElement element) soFar
                )
                Set.empty


firstJustMap : (element -> Maybe found) -> (List element -> Maybe found)
firstJustMap elementToMaybeFound =
    \list ->
        case list of
            [] ->
                Nothing

            head :: tail ->
                case head |> elementToMaybeFound of
                    Nothing ->
                        tail |> firstJustMap elementToMaybeFound

                    Just found ->
                        found |> Just
