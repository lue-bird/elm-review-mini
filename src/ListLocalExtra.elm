module ListLocalExtra exposing (anyPair, consJust, elementAtIndex, headMap, last, lastMap)


last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        [ onlyElement ] ->
            onlyElement |> Just

        _ :: el1 :: el2Up ->
            last (el1 :: el2Up)


lastMap : (a -> a) -> (List a -> List a)
lastMap mapper lines =
    case List.reverse lines of
        [] ->
            lines

        first :: rest ->
            List.reverse (mapper first :: rest)


headMap : (a -> a) -> (List a -> List a)
headMap headChange list =
    case list of
        [] ->
            []

        head :: tail ->
            (head |> headChange) :: tail


consJust : Maybe a -> (List a -> List a)
consJust maybeNewHead list =
    case maybeNewHead of
        Nothing ->
            list

        Just newHead ->
            newHead :: list


anyPair : (element -> element -> Bool) -> (List element -> Bool)
anyPair isFound list =
    case list of
        [] ->
            False

        head :: tail ->
            if List.any (\tailElement -> isFound head tailElement) tail then
                True

            else
                anyPair isFound tail


elementAtIndex : Int -> (List a -> Maybe a)
elementAtIndex index list =
    list |> List.drop index |> List.head
