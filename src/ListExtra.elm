module ListExtra exposing (last)


last : List a -> Maybe a
last =
    \list ->
        case list of
            [] ->
                Nothing

            [ onlyElement ] ->
                onlyElement |> Just

            el0 :: el1 :: el2Up ->
                (el1 :: el2Up) |> last
