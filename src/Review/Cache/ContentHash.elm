module Review.Cache.ContentHash exposing (ContentHash, areEqual, areEqualForMaybe, hash)

import FNV1a


type ContentHash
    = ContentHash Int


hash : String -> ContentHash
hash source =
    ContentHash (FNV1a.hashWithSeed source 0)


areEqual : ContentHash -> ContentHash -> Bool
areEqual (ContentHash a) (ContentHash b) =
    a == b


areEqualForMaybe : Maybe ContentHash -> Maybe ContentHash -> Bool
areEqualForMaybe a b =
    case ( a, b ) of
        ( Just a_, Just b_ ) ->
            a_ == b_

        ( Nothing, Nothing ) ->
            True

        ( Nothing, Just _ ) ->
            False

        ( Just _, Nothing ) ->
            False
