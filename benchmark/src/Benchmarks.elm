module Benchmarks exposing (benchmarks)

import Benchmark
import Benchmark.Alternative
import Serialize
import Dict exposing (Dict)
import Json.Encode
import Json.Decode
import Bytes exposing (Bytes)
import Bytes.Encode
import Bytes.Decode
import Codec
import Value exposing (Value)
import Value.Morph
import Morph
import String.Morph
import List.Morph
import Int.Morph
import Integer.Morph


benchmarks : Benchmark.Benchmark
benchmarks =
    Benchmark.describe "deserialize"
        [ Benchmark.Alternative.rank "formats"
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
                    redEncoder { i = i, s = s, b =  b }

                Yellow f ->
                    yellowEncoder f

                Green ->
                    greenEncoder ()
        )
        |> Value.Morph.variant ( \r -> Red r.i r.s r.b, "Red" )
            (Value.Morph.group (\i s b -> { i = i, s = s, b =  b })
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
