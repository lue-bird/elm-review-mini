module Unicode exposing (dropLeft, left, length)

{-| String functions that consider Unicode characters as a single characters, unlike `elm/core`'s `String` type.
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


length : String -> Int
length =
    \string -> string |> String.toList |> List.length
