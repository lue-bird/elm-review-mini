module Unicode exposing (dropLeft, left)

{-| String functions that consider Unicode characters as a single characters, unlike `elm/core`'s `String` type.

@docs dropLeft, left

-}


dropLeft : Int -> String -> String
dropLeft n string =
    string
        |> String.toList
        |> List.drop n
        |> String.fromList


left : Int -> String -> String
left n string =
    string
        |> String.toList
        |> List.take n
        |> String.fromList
