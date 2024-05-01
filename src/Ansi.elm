module Ansi exposing (backgroundRed, bold, cyan, red, yellow)

-- FONTS


bold : String -> String
bold text =
    String.concat [ "\u{001B}[1m", text, "\u{001B}[22m" ]



-- COLORS


applyColor : String -> String -> String
applyColor color string =
    String.concat [ "\u{001B}[", color, "m", string, noColor ]


noColor : String
noColor =
    "\u{001B}[39m"


red : String -> String
red =
    applyColor "31"


yellow : String -> String
yellow =
    applyColor "33"


cyan : String -> String
cyan =
    applyColor "36"



-- BACKGROUND COLORS


backgroundRed : String -> String
backgroundRed =
    applyBackgroundColor "41"


applyBackgroundColor : String -> String -> String
applyBackgroundColor color string =
    String.concat [ "\u{001B}[", color, "m", string, noBackgroundColor ]


noBackgroundColor : String
noBackgroundColor =
    "\u{001B}[0m"
