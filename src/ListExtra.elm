module ListExtra exposing (firstElementWhere, last, lastMap)


last : List a -> Maybe a
last =
    \list ->
        case list of
            [] ->
                Nothing

            [ onlyElement ] ->
                onlyElement |> Just

            _ :: el1 :: el2Up ->
                (el1 :: el2Up) |> last


lastMap : (a -> a) -> (List a -> List a)
lastMap mapper lines =
    case List.reverse lines of
        [] ->
            lines

        first :: rest ->
            List.reverse (mapper first :: rest)


firstElementWhere : (a -> Bool) -> (List a -> Maybe a)
firstElementWhere isFound =
    \list ->
        case list of
            [] ->
                Nothing

            head :: tail ->
                if isFound head then
                    Just head

                else
                    firstElementWhere isFound tail
