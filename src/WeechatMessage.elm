module WeechatMessage exposing (Message, Object, WeechatData(..), parse)

import Dict exposing (Dict)
import String.UTF8 as UTF8


type alias Message =
    { id : Maybe String
    , data : WeechatData
    }


type WeechatData
    = Str (Maybe String)
    | Chr Char
    | Pth (List String)
    | Ptr String
    | Tim String
    | Inf ( String, String )
    | Int Int
    | Hda (List Object)
    | Arr (List WeechatData)
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

        numPointers =
            hpath
                |> Maybe.andThen (\h -> Just (String.split "/" h))
                |> Maybe.andThen (\elements -> Just (List.length elements))
                |> Maybe.withDefault 1

        ( keys, keysTail ) =
            readString hpathTail

        keyList =
            String.split "," (Maybe.withDefault "" keys)
                |> List.map splitKeyPair

        -- the first value is always a list of p-path pointers.
        keyListWithPpath =
            ( "ppath", "pth" ) :: keyList

        count =
            parseNumber (List.take 4 keysTail)

        tail =
            List.drop 4 keysTail
    in
    readHdataTable [] keyListWithPpath numPointers count tail


readHdataTable : List Object -> List ( String, String ) -> Int -> Int -> Bytes -> List Object
readHdataTable acc keyList numPointers count bytes =
    let
        ( values, tail ) =
            readKeyValue Dict.empty keyList numPointers bytes

        newAcc =
            acc ++ [ values ]
    in
    if count == 1 then
        newAcc

    else
        readHdataTable newAcc keyList numPointers (count - 1) tail


readKeyValue : Object -> List ( String, String ) -> Int -> Bytes -> ( Object, Bytes )
readKeyValue acc keyList numPointers bytes =
    case keyList of
        [] ->
            ( acc, bytes )

        keyPair :: keyListTail ->
            let
                ( key, valueType ) =
                    keyPair

                ( value, bytesTail ) =
                    readType valueType numPointers bytes

                ( keyValue, tail ) =
                    readKeyValue acc keyListTail numPointers bytesTail
            in
            ( Dict.insert key value keyValue, tail )


readType : String -> Int -> Bytes -> ( WeechatData, Bytes )
readType valueType numPointers bytes =
    case valueType of
        "int" ->
            ( Int (parseNumber bytes), List.drop 4 bytes )

        "pth" ->
            let
                ( pointers, pointersTail ) =
                    readPpath [] bytes numPointers
            in
            ( Pth pointers, pointersTail )

        "ptr" ->
            let
                ( pointer, pointerTail ) =
                    readStringData bytes
            in
            ( Ptr pointer, pointerTail )

        "tim" ->
            let
                ( time, timeTail ) =
                    readStringData bytes
            in
            ( Tim time, timeTail )

        "str" ->
            let
                ( string, stringTail ) =
                    readString bytes
            in
            ( Str string, stringTail )

        "chr" ->
            let
                ( char, charTail ) =
                    readChar bytes
            in
            ( Chr char, charTail )

        "arr" ->
            let
                ( array, arrayTail ) =
                    readArray bytes
            in
            ( Arr array, arrayTail )

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



-- String encoded data (pointer, time)


readStringData : Bytes -> ( String, Bytes )
readStringData bytes =
    case bytes of
        length :: tail ->
            ( List.take length tail |> parseUTF8
            , List.drop length tail
            )

        [] ->
            ( "", [] )



-- Ppath


readPpath : List String -> Bytes -> Int -> ( List String, Bytes )
readPpath acc bytes numPointers =
    if numPointers == 0 then
        ( acc, bytes )

    else
        case bytes of
            length :: tail ->
                let
                    pointer =
                        List.take length tail
                            |> parseUTF8

                    newAcc =
                        pointer :: acc

                    newTail =
                        List.drop length tail
                in
                readPpath newAcc newTail (numPointers - 1)

            [] ->
                ( [], [] )



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



-- Char


readChar : Bytes -> ( Char, Bytes )
readChar bytes =
    case bytes of
        head :: tail ->
            ( Char.fromCode head, tail )

        _ ->
            ( '�', bytes )



-- Array


readArray : Bytes -> ( List WeechatData, Bytes )
readArray bytes =
    case bytes of
        [] ->
            ( [], bytes )

        arrayData ->
            let
                dataType =
                    List.take 3 arrayData
                        |> parseUTF8

                length =
                    List.drop 3 arrayData
                        |> parseNumber

                data =
                    List.drop 7 arrayData
            in
            readArrayRecursive [] dataType length data


readArrayRecursive : List WeechatData -> String -> Int -> Bytes -> ( List WeechatData, Bytes )
readArrayRecursive acc dataType length bytes =
    if length == 0 then
        ( acc, bytes )

    else
        let
            ( value, tail ) =
                readType dataType 0 bytes

            newAcc =
                acc ++ [ value ]
        in
        readArrayRecursive newAcc dataType (length - 1) tail



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
