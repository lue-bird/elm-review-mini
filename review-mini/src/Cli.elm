port module Cli exposing (main)

{-| You do not need to touch this file. Go to `src/ReviewConfiguration.elm` to add reviews
-}

import Json.Encode
import Review.Cli
import ReviewConfiguration


main : Review.Cli.Program
main =
    Review.Cli.program { configuration = ReviewConfiguration.configuration, toJs = toJs, fromJs = fromJs }


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
