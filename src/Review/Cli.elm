module Review.Cli exposing (program, Program, ProgramEvent, ProgramState)

{-| Run a review in the terminal

@docs program, Program, ProgramEvent, ProgramState

-}

import Json.Decode
import Json.Encode
import Review exposing (Review)


{-| An elm worker produced by [`Review.Cli.Program`](Review-Cli#Program)
-}
type alias Program =
    Platform.Program () ProgramState ProgramEvent


{-| elm model of a [`Review.program`](#program)
-}
type alias ProgramState =
    {}


{-| elm msg of a [`Review.Cli.program`](#program)
-}
type ProgramEvent
    = ModuleAddedOrChanged { path : String, source : String }
    | ModuleRemoved { path : String }


eventJsonDecoder : Json.Decode.Decoder ProgramEvent
eventJsonDecoder =
    Json.Decode.field "tag"
        (Json.Decode.string
            |> Json.Decode.andThen
                (\tag ->
                    Json.Decode.field "value"
                        (case tag of
                            "ModuleAddedOrChanged" ->
                                Json.Decode.map2
                                    (\path source -> ModuleAddedOrChanged { path = path, source = source })
                                    (Json.Decode.field "path" Json.Decode.string)
                                    (Json.Decode.field "source" Json.Decode.string)

                            "ModuleRemoved" ->
                                Json.Decode.map
                                    (\path -> ModuleRemoved { path = path })
                                    (Json.Decode.field "path" Json.Decode.string)

                            unknownTag ->
                                Json.Decode.fail ("unknown js message tag " ++ unknownTag)
                        )
                )
        )


{-| A node.js CLI program.
Start initializing one as showcased in [elm-review-mini-cli-starter](https://github.com/lue-bird/elm-review-mini-cli-starter)
-}
program :
    { toJs : Json.Encode.Value -> Cmd Never
    , fromJson : (Json.Encode.Value -> ProgramEvent) -> Sub ProgramEvent
    , reviews : List Review
    }
    -> Program
program config =
    -- TODO
    Platform.worker
        { init = \() -> ( {}, Cmd.none )
        , update =
            \event state ->
                ( state, Cmd.none )

        -- toJs
        , subscriptions =
            \_ ->
                Sub.none

        -- fromJs
        }
