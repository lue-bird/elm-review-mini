module FastSet.LocalExtra exposing (fromListMap, unionFromList, unionFromListMap)

import FastSet


unionFromList : List (FastSet.Set comparableElement) -> FastSet.Set comparableElement
unionFromList =
    \list -> list |> unionFromListMap identity


unionFromListMap :
    (element -> FastSet.Set comparableElement)
    -> (List element -> FastSet.Set comparableElement)
unionFromListMap elementToSet =
    \list ->
        list
            |> List.foldl
                (\element soFar ->
                    FastSet.union soFar (element |> elementToSet)
                )
                FastSet.empty


fromListMap : (element -> comparableElement) -> (List element -> FastSet.Set comparableElement)
fromListMap toComparable =
    \list ->
        list
            |> List.foldl
                (\element acc ->
                    acc |> FastSet.insert (element |> toComparable)
                )
                FastSet.empty
