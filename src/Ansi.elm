module Ansi exposing (backgroundRed, bold, cyan, green, red, yellow)


bold : String -> String
bold text =
    "\u{001B}[1m" ++ text ++ "\u{001B}[22m"


applyColor : String -> String -> String
applyColor color string =
    "\u{001B}[" ++ color ++ "m" ++ string ++ "\u{001B}[39m"


red : String -> String
red =
    applyColor "31"


green : String -> String
green =
    applyColor "32"


yellow : String -> String
yellow =
    applyColor "33"


cyan : String -> String
cyan =
    applyColor "36"


backgroundRed : String -> String
backgroundRed =
    applyBackgroundColor "41"


applyBackgroundColor : String -> String -> String
applyBackgroundColor color string =
    "\u{001B}[" ++ color ++ "m" ++ string ++ "\u{001B}[0m"
