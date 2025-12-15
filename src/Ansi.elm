module Ansi exposing (backgroundRed, bold, cyan, green, red, yellow)

-- FONTS


bold : String -> String
bold text =
    "\u{001B}[1m" ++ text ++ "\u{001B}[22m"



-- COLORS


applyColor : String -> String -> String
applyColor color string =
    "\u{001B}[" ++ color ++ "m" ++ string ++ noColor


red : String -> String
red string =
    applyColor "31" string


green : String -> String
green string =
    applyColor "32" string


yellow : String -> String
yellow string =
    applyColor "33" string


cyan : String -> String
cyan string =
    applyColor "36" string


noColor : String
noColor =
    "\u{001B}[39m"



-- BACKGROUND COLORS


applyBackgroundColor : String -> String -> String
applyBackgroundColor color string =
    "\u{001B}[" ++ color ++ "m" ++ string ++ noBackgroundColor


backgroundRed : String -> String
backgroundRed string =
    applyBackgroundColor "41" string


noBackgroundColor : String
noBackgroundColor =
    "\u{001B}[0m"
