module Benchmarks exposing (benchmarks)

import Array exposing (Array)
import Benchmark
import Benchmark.Alternative
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Codec
import Dict exposing (Dict)
import Elm.Syntax.Range
import Int.Morph
import Integer.Morph
import Json.Decode
import Json.Encode
import List.Morph
import Morph
import Serialize
import String.Morph
import Value exposing (Value)
import Value.Morph


benchmarks : Benchmark.Benchmark
benchmarks =
    Benchmark.describe "elm-review-mini"
        [ Benchmark.Alternative.rank "source extract segment in range"
            (\inRange ->
                String.repeat 200 exampleLine
                    |> inRange
                        { start = { row = 100, column = 4 }
                        , end = { row = 110, column = 10 }
                        }
            )
            [ ( "using array"
              , sourceExtractInRangeArray
              )
            , ( "using lines drop |> take"
              , sourceExtractInRangeDropTake
              )
            , ( "using lines take |> drop"
              , sourceExtractInRangeTakeDrop
              )

            {- not included because it takes 100x the time
               , ( "using String.fold"
               , sourceExtractInRangeFold
               )
            -}
            ]
        , Benchmark.Alternative.rank "from generic"
            (\f -> f ())
            [ ( "(can't be equated) serialize json"
              , \() ->
                    exampleNeighborhoodsSerializedJson
                        |> Serialize.decodeFromJson neighborhoodsSerialize
                        |> Result.toMaybe
              )
            , ( "(can't be equated) codec json"
              , \() ->
                    exampleNeighborhoodsCodecJson
                        |> Codec.decodeValue neighborhoodsCodec
                        |> Result.toMaybe
              )
            , ( "(can't be equated) bytes"
              , \() ->
                    exampleNeighborhoodsSerializedBytes
                        |> Serialize.decodeFromBytes neighborhoodsSerialize
                        |> Result.toMaybe
              )
            , ( "base 64"
              , \() ->
                    exampleNeighborhoodsSerializedBase64
                        |> Serialize.decodeFromString neighborhoodsSerialize
                        |> Result.toMaybe
              )
            , ( "serialize json string"
              , \() ->
                    exampleNeighborhoodsSerializedJsonString
                        |> Json.Decode.decodeString (Serialize.getJsonDecoder never neighborhoodsSerialize)
                        |> Result.toMaybe
              )
            , ( "codec json string"
              , \() ->
                    exampleNeighborhoodsCodecJsonString
                        |> Codec.decodeString neighborhoodsCodec
                        |> Result.toMaybe
              )
            , ( "unsigned int 8 list"
              , \() ->
                    exampleNeighborhoodsSerializedUnsignedInt8List
                        |> bytesFromUnsignedInt8List
                        |> Serialize.decodeFromBytes neighborhoodsSerialize
                        |> Result.toMaybe
              )
            , ( "morph value"
              , \() ->
                    exampleNeighborhoodsMorphValue
                        |> Morph.toNarrow
                            (neighborhoodsMorph |> Morph.over (Value.Morph.eachTag Value.Morph.descriptive))
                        |> Result.toMaybe
              )
            ]
        ]



--


exampleLine : String
exampleLine =
    """Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."""


sourceExtractInRangeFold : Elm.Syntax.Range.Range -> (String -> String)
sourceExtractInRangeFold range =
    \string ->
        string
            |> String.foldl
                (\char soFar ->
                    let
                        isInRange : Bool
                        isInRange =
                            (soFar.row >= range.start.row)
                                && (soFar.row <= range.start.row)
                    in
                    case char of
                        '\n' ->
                            { row = soFar.row + 1
                            , lines =
                                if isInRange then
                                    "" :: soFar.lines

                                else
                                    soFar.lines
                            }

                        charNonNewLine ->
                            if isInRange then
                                { row = soFar.row
                                , lines =
                                    case soFar.lines of
                                        [] ->
                                            [ charNonNewLine |> String.fromChar ]

                                        currentLine :: linesBefore ->
                                            -- TODO either append instead or reverse once complete
                                            String.cons charNonNewLine currentLine :: linesBefore
                                }

                            else
                                soFar
                )
                { row = 1, lines = [] }
            |> .lines
            |> List.map String.reverse
            |> List.reverse
            |> listLastMap (\lastLine -> lastLine |> unicodeLeft range.end.column)
            |> String.join "\n"
            |> unicodeDropLeft (range.start.column - 1)


sourceExtractInRangeDropTake : Elm.Syntax.Range.Range -> (String -> String)
sourceExtractInRangeDropTake range =
    \string ->
        string
            |> String.split "\n"
            |> List.drop (range.start.row - 1)
            |> List.take (range.end.row - range.start.row + 1)
            |> listLastMap (\lastLine -> lastLine |> unicodeLeft range.end.column)
            |> String.join "\n"
            |> unicodeDropLeft (range.start.column - 1)


sourceExtractInRangeTakeDrop : Elm.Syntax.Range.Range -> (String -> String)
sourceExtractInRangeTakeDrop range =
    \string ->
        string
            |> String.split "\n"
            |> List.take range.end.row
            |> List.drop (range.start.row - 1)
            |> listLastMap (\lastLine -> lastLine |> unicodeLeft range.end.column)
            |> String.join "\n"
            |> unicodeDropLeft (range.start.column - 1)


listLastMap : (a -> a) -> (List a -> List a)
listLastMap mapper lines =
    case List.reverse lines of
        [] ->
            lines

        first :: rest ->
            List.reverse (mapper first :: rest)


listElementAtNaturalIndex : Int -> (List a -> Maybe a)
listElementAtNaturalIndex index =
    \list ->
        case list of
            [] ->
                Nothing

            head :: tail ->
                case index of
                    0 ->
                        head |> Just

                    indexAtLeast1 ->
                        listElementAtNaturalIndex (indexAtLeast1 - 1) tail


sourceExtractInRangeArray : Elm.Syntax.Range.Range -> String -> String
sourceExtractInRangeArray { start, end } source =
    if start.row == end.row then
        source
            |> String.split "\n"
            |> listElementAtNaturalIndex (start.row - 1)
            |> Maybe.map (unicodeSlice (start.column - 1) (end.column - 1))
            |> Maybe.withDefault ""

    else
        let
            lines : Array String
            lines =
                String.split "\n" source
                    |> Array.fromList

            firstLine : String
            firstLine =
                case Array.get (start.row - 1) lines of
                    Just str ->
                        unicodeDropLeft (start.column - 1) str

                    Nothing ->
                        ""

            lastLine : String
            lastLine =
                case Array.get (end.row - 1) lines of
                    Just str ->
                        unicodeLeft end.column str

                    Nothing ->
                        ""

            resultingLines : List String
            resultingLines =
                if start.row + 1 == end.row then
                    [ firstLine
                    , lastLine
                    ]

                else
                    [ firstLine
                    , Array.slice start.row (end.row - 1) lines
                        |> Array.toList
                        |> String.join "\n"
                    , lastLine
                    ]
        in
        resultingLines |> String.join "\n"


unicodeSlice : Int -> Int -> String -> String
unicodeSlice start end string =
    string
        |> String.toList
        |> List.drop start
        |> List.take (end - start)
        |> String.fromList


unicodeDropLeft : Int -> String -> String
unicodeDropLeft n string =
    string
        |> String.toList
        |> List.drop n
        |> String.fromList


unicodeLeft : Int -> String -> String
unicodeLeft n string =
    string
        |> String.toList
        |> List.take n
        |> String.fromList



--


exampleNeighborhoodsSerializedBase64 : String
exampleNeighborhoodsSerializedBase64 =
    exampleNeighborhoods |> Serialize.encodeToString neighborhoodsSerialize


exampleNeighborhoodsSerializedJson : Json.Encode.Value
exampleNeighborhoodsSerializedJson =
    exampleNeighborhoods |> Serialize.encodeToJson neighborhoodsSerialize


exampleNeighborhoodsSerializedJsonString : String
exampleNeighborhoodsSerializedJsonString =
    exampleNeighborhoods |> Serialize.encodeToJson neighborhoodsSerialize |> Json.Encode.encode 0


exampleNeighborhoodsCodecJsonString : String
exampleNeighborhoodsCodecJsonString =
    exampleNeighborhoods |> Codec.encodeToString 0 neighborhoodsCodec


exampleNeighborhoodsCodecJson : Json.Encode.Value
exampleNeighborhoodsCodecJson =
    exampleNeighborhoods |> Codec.encodeToValue neighborhoodsCodec


exampleNeighborhoodsSerializedBytes : Bytes
exampleNeighborhoodsSerializedBytes =
    exampleNeighborhoods |> Serialize.encodeToBytes neighborhoodsSerialize


exampleNeighborhoodsSerializedUnsignedInt8List : List Int
exampleNeighborhoodsSerializedUnsignedInt8List =
    exampleNeighborhoodsSerializedBytes |> bytesToUnsignedInt8List


exampleNeighborhoodsMorphValue : Value.Value String
exampleNeighborhoodsMorphValue =
    exampleNeighborhoods
        |> Morph.toBroad
            (neighborhoodsMorph |> Morph.over (Value.Morph.eachTag Value.Morph.descriptive))


exampleNeighborhoods : Dict String Neighborhood
exampleNeighborhoods =
    List.range 0 29
        |> List.map String.fromInt
        |> List.map
            (\i ->
                ( i
                , { meAndMyFriends =
                        { me = i, myFriends = List.range -10 -1 |> List.map String.fromInt }
                  , semaphore = Red 3 i False
                  }
                )
            )
        |> Dict.fromList


type alias Neighborhood =
    { meAndMyFriends : MeAndMyFriends, semaphore : Semaphore }


type alias MeAndMyFriends =
    { me : String
    , myFriends : List String
    }


type Semaphore
    = Red Int String Bool
    | Yellow Int
    | Green



-- elm-serialize


neighborhoodsSerialize : Serialize.Codec e (Dict String Neighborhood)
neighborhoodsSerialize =
    Serialize.dict Serialize.string neighborhoodSerialize


neighborhoodSerialize : Serialize.Codec e Neighborhood
neighborhoodSerialize =
    Serialize.record
        (\meAndMyFriends semaphore ->
            { meAndMyFriends = meAndMyFriends, semaphore = semaphore }
        )
        |> Serialize.field .meAndMyFriends meAndMyFriendsSerialize
        |> Serialize.field .semaphore semaphoreSerialize
        |> Serialize.finishRecord


meAndMyFriendsSerialize : Serialize.Codec e MeAndMyFriends
meAndMyFriendsSerialize =
    Serialize.record (\me myFriends -> { me = me, myFriends = myFriends })
        |> Serialize.field .me Serialize.string
        |> Serialize.field .myFriends (Serialize.list Serialize.string)
        |> Serialize.finishRecord


semaphoreSerialize : Serialize.Codec e Semaphore
semaphoreSerialize =
    Serialize.customType
        (\redEncoder yellowEncoder greenEncoder value ->
            case value of
                Red i s b ->
                    redEncoder i s b

                Yellow f ->
                    yellowEncoder f

                Green ->
                    greenEncoder
        )
        |> Serialize.variant3 Red Serialize.int Serialize.string Serialize.bool
        |> Serialize.variant1 Yellow Serialize.int
        |> Serialize.variant0 Green
        |> Serialize.finishCustomType



-- elm-codec


neighborhoodsCodec : Codec.Codec (Dict String Neighborhood)
neighborhoodsCodec =
    Codec.dict neighborhoodCodec


neighborhoodCodec : Codec.Codec Neighborhood
neighborhoodCodec =
    Codec.object
        (\meAndMyFriends semaphore ->
            { meAndMyFriends = meAndMyFriends, semaphore = semaphore }
        )
        |> Codec.field "meAndMyFriends" .meAndMyFriends meAndMyFriendsCodec
        |> Codec.field "semaphore" .semaphore semaphoreCodec
        |> Codec.buildObject


meAndMyFriendsCodec : Codec.Codec MeAndMyFriends
meAndMyFriendsCodec =
    Codec.object (\me myFriends -> { me = me, myFriends = myFriends })
        |> Codec.field "me" .me Codec.string
        |> Codec.field "myFriends" .myFriends (Codec.list Codec.string)
        |> Codec.buildObject


semaphoreCodec : Codec.Codec Semaphore
semaphoreCodec =
    Codec.custom
        (\redEncoder yellowEncoder greenEncoder value ->
            case value of
                Red i s b ->
                    redEncoder i s b

                Yellow f ->
                    yellowEncoder f

                Green ->
                    greenEncoder
        )
        |> Codec.variant3 "Red" Red Codec.int Codec.string Codec.bool
        |> Codec.variant1 "Yellow" Yellow Codec.int
        |> Codec.variant0 "Green" Green
        |> Codec.buildCustom



-- elm-morph


neighborhoodsMorph : Value.Morph.MorphValue (Dict String Neighborhood)
neighborhoodsMorph =
    Morph.oneToOne Dict.fromList Dict.toList
        |> Morph.over
            (List.Morph.value
                (Value.Morph.group Tuple.pair
                    |> Value.Morph.part ( Tuple.first, "key" ) String.Morph.value
                    |> Value.Morph.part ( Tuple.second, "value" ) neighborhoodMorph
                    |> Value.Morph.groupFinish
                )
            )


neighborhoodMorph : Value.Morph.MorphValue Neighborhood
neighborhoodMorph =
    Value.Morph.group
        (\meAndMyFriends semaphore ->
            { meAndMyFriends = meAndMyFriends, semaphore = semaphore }
        )
        |> Value.Morph.part ( .meAndMyFriends, "meAndMyFriends" ) meAndMyFriendsMorph
        |> Value.Morph.part ( .semaphore, "semaphore" ) semaphoreMorph
        |> Value.Morph.groupFinish


meAndMyFriendsMorph : Value.Morph.MorphValue MeAndMyFriends
meAndMyFriendsMorph =
    Value.Morph.group (\me myFriends -> { me = me, myFriends = myFriends })
        |> Value.Morph.part ( .me, "me" ) String.Morph.value
        |> Value.Morph.part ( .myFriends, "myFriends" ) (List.Morph.value String.Morph.value)
        |> Value.Morph.groupFinish


semaphoreMorph : Value.Morph.MorphValue Semaphore
semaphoreMorph =
    Morph.choice
        (\redEncoder yellowEncoder greenEncoder value ->
            case value of
                Red i s b ->
                    redEncoder { i = i, s = s, b = b }

                Yellow f ->
                    yellowEncoder f

                Green ->
                    greenEncoder ()
        )
        |> Value.Morph.variant ( \r -> Red r.i r.s r.b, "Red" )
            (Value.Morph.group (\i s b -> { i = i, s = s, b = b })
                |> Value.Morph.part ( .i, "i" ) intMorph
                |> Value.Morph.part ( .s, "s" ) String.Morph.value
                |> Value.Morph.part ( .b, "b" ) boolMorph
                |> Value.Morph.groupFinish
            )
        |> Value.Morph.variant ( Yellow, "Yellow" ) intMorph
        |> Value.Morph.variant ( \() -> Green, "Green" ) Value.Morph.unit
        |> Value.Morph.choiceFinish


intMorph : Value.Morph.MorphValue Int
intMorph =
    Int.Morph.integer |> Morph.over Integer.Morph.value


boolMorph : Value.Morph.MorphValue Bool
boolMorph =
    Morph.choice
        (\true false isTrue ->
            if isTrue then
                true ()

            else
                false ()
        )
        |> Value.Morph.variant ( \() -> True, "True" ) Value.Morph.unit
        |> Value.Morph.variant ( \() -> False, "False" ) Value.Morph.unit
        |> Value.Morph.choiceFinish



-- bytes unsigned int 8 list conversion


bytesToUnsignedInt8List : Bytes -> List Int
bytesToUnsignedInt8List =
    \bytes ->
        bytes
            |> Bytes.Decode.decode
                (unsignedInt8ListBytesDecoder (bytes |> Bytes.width))
            |> -- above decoder should never fail
               Maybe.withDefault []


unsignedInt8ListBytesDecoder : Int -> Bytes.Decode.Decoder (List Int)
unsignedInt8ListBytesDecoder length =
    Bytes.Decode.loop ( length, [] )
        (\( n, elements ) ->
            if n <= 0 then
                Bytes.Decode.succeed (Bytes.Decode.Done (elements |> List.reverse))

            else
                Bytes.Decode.map
                    (\byte -> Bytes.Decode.Loop ( n - 1, byte :: elements ))
                    Bytes.Decode.unsignedInt8
        )


bytesFromUnsignedInt8List : List Int -> Bytes
bytesFromUnsignedInt8List =
    \uint8s ->
        uint8s
            |> List.map Bytes.Encode.unsignedInt8
            |> Bytes.Encode.sequence
            |> Bytes.Encode.encode
