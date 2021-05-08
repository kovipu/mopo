module WeechatMessage exposing (Message, Object, WeechatData(..), parse)

import Dict exposing (Dict)
import String.UTF8 as UTF8


type alias Message =
    { id : Maybe String
    , data : WeechatData
    }


type WeechatData
    = Str (Maybe String)
    | Ptr String
    | Inf ( String, String )
    | Int Int
    | Hda (List Object)
    | Inv String


type alias Object =
    Dict String WeechatData


type alias Bytes =
    List Int


parse : Bytes -> Message
parse message =
    let
        -- Drop header bytes.
        messageWithoutHeader =
            List.drop 5 message

        ( id, payload ) =
            readString messageWithoutHeader
    in
    { id = id
    , data = parseWeechatData payload
    }


parseWeechatData : Bytes -> WeechatData
parseWeechatData bytes =
    let
        operation =
            List.take 3 bytes
                |> parseUTF8

        payload =
            List.drop 3 bytes
    in
    case operation of
        "inf" ->
            Inf (readInfo payload)

        "hda" ->
            Hda (readHdata payload)

        _ ->
            Inv ("Invalid weechat datatype " ++ operation)



-- OPERATION PARSERS
-- Info


readInfo : Bytes -> ( String, String )
readInfo message =
    let
        ( key, rest ) =
            readString message

        ( value, _ ) =
            readString rest
    in
    ( Maybe.withDefault "" key, Maybe.withDefault "" value )



-- Hdata


readHdata : Bytes -> List Object
readHdata payload =
    let
        ( hpath, hpathTail ) =
            readString payload

        ( keys, keysTail ) =
            readString hpathTail

        keyList =
            String.split "," (Maybe.withDefault "" keys)
                |> List.map splitKeyPair

        -- the first value is always p-path pointer.
        keyListWithPointer =
            ( "ppath", "ptr" ) :: keyList

        count =
            parseNumber (List.take 4 keysTail)

        tail =
            List.drop 4 keysTail
    in
    readHdataTable [] keyListWithPointer count tail


readHdataTable : List Object -> List ( String, String ) -> Int -> Bytes -> List Object
readHdataTable acc keyList count bytes =
    let
        ( values, tail ) =
            readKeyValue Dict.empty keyList bytes

        newAcc =
            acc ++ [ values ]
    in
    if count == 1 then
        newAcc

    else
        readHdataTable newAcc keyList (count - 1) tail


readKeyValue : Object -> List ( String, String ) -> Bytes -> ( Object, Bytes )
readKeyValue acc keyList bytes =
    case keyList of
        [] ->
            ( acc, bytes )

        keyPair :: keyListTail ->
            let
                ( key, valueType ) =
                    keyPair

                ( value, bytesTail ) =
                    readType valueType bytes

                ( keyValue, tail ) =
                    readKeyValue acc keyListTail bytesTail
            in
            ( Dict.insert key value keyValue, tail )


readType : String -> Bytes -> ( WeechatData, Bytes )
readType valueType bytes =
    case valueType of
        "int" ->
            ( Int (parseNumber bytes), List.drop 4 bytes )

        "ptr" ->
            let
                ( pointer, pointerTail ) =
                    readPointer bytes
            in
            ( Ptr pointer, pointerTail )

        "str" ->
            let
                ( string, stringTail ) =
                    readString bytes
            in
            ( Str string, stringTail )

        _ ->
            ( Inv ("Invalid type: " ++ valueType), bytes )


splitKeyPair : String -> ( String, String )
splitKeyPair keyPair =
    let
        pair =
            String.split ":" keyPair

        first =
            List.head pair

        second =
            List.head (List.drop 1 pair)
    in
    ( Maybe.withDefault "" first, Maybe.withDefault "" second )



-- Pointer


readPointer : Bytes -> ( String, Bytes )
readPointer bytes =
    case bytes of
        length :: tail ->
            ( List.take length tail |> parseUTF8
            , List.drop length tail
            )

        [] ->
            ( "", [] )



-- String


readString : Bytes -> ( Maybe String, Bytes )
readString bytes =
    let
        length =
            parseNumber (List.take 4 bytes)

        rest =
            List.drop 4 bytes
    in
    if
        -- This means the string is null in Weechat.
        length == 0xFFFFFFFF
    then
        ( Nothing, rest )

    else
        ( Just (parseUTF8 (List.take length rest))
        , List.drop length rest
        )



-- Number


parseNumber : Bytes -> Int
parseNumber bytes =
    case bytes of
        b3 :: b2 :: b1 :: b0 :: _ ->
            b0 + (0x0100 * b1) + (0x00010000 * b2) + (0x01000000 * b3)

        _ ->
            0



-- Helpers


parseUTF8 : Bytes -> String
parseUTF8 message =
    case UTF8.toString message of
        Ok value ->
            value

        Err err ->
            err



-- Split a list into lengths defined by a list of breakpoints.


splitAtLengths : List Int -> List a -> List (List a)
splitAtLengths breakpoints list =
    case breakpoints of
        [] ->
            [ list ]

        break :: breakTail ->
            let
                segment =
                    List.take break list

                tail =
                    List.drop break list
            in
            [ segment ] ++ splitAtLengths breakTail tail
