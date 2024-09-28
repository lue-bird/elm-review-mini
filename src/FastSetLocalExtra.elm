module FastSetLocalExtra exposing (fromListMap)

import FastSet


fromListMap : (element -> comparableElement) -> (List element -> FastSet.Set comparableElement)
fromListMap toComparable =
    \list ->
        list
            |> List.foldl
                (\element acc ->
                    acc |> FastSet.insert (element |> toComparable)
                )
                FastSet.empty
