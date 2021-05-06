module WeechatMessage exposing (Buffer, Message(..), parse)

import String.UTF8 as UTF8


type Message
    = Info ( String, String )
    | Hdata (List Buffer)
    | Invalid String


type alias Buffer =
    { hpath : String
    , number : Int
    , fullName : String
    }


parse : List Int -> Message
parse message =
    let
        -- Drop header bytes.
        messageWithoutHeader =
            List.drop 9 message

        operation =
            List.take 3 messageWithoutHeader
                |> parseUTF8

        rest =
            List.drop 3 messageWithoutHeader
    in
    case operation of
        "inf" ->
            Info (readInfo rest)

        "hda" ->
            Hdata (readHdata rest)

        _ ->
            Invalid "Unknown operation."



-- OPERATION PARSERS


readInfo : List Int -> ( String, String )
readInfo message =
    let
        ( key, rest ) =
            readString message

        ( value, _ ) =
            readString rest
    in
    ( key, value )


readHdata : List Int -> List Buffer
readHdata message =
    let
        -- TODO: make use of this data.
        ( keys, keysrest ) =
            readString message

        ( values, tail ) =
            readString keysrest
    in
    readHdataChunk (List.drop 4 tail)


readHdataChunk : List Int -> List Buffer
readHdataChunk chunk =
    let
        ( hpath, rest ) =
            readShortString chunk

        number =
            parseNumber (List.take 4 rest)

        ( fullName, tail ) =
            readString (List.drop 4 rest)

        buffer =
            { hpath = hpath
            , number = number
            , fullName = fullName
            }
    in
    if List.length tail == 0 then
        [ buffer ]

    else
        [ buffer ] ++ readHdataChunk tail



-- HELPERS


readShortString : List Int -> ( String, List Int )
readShortString bytes =
    case bytes of
        length :: tail ->
            ( parseUTF8 (List.take length tail)
            , List.drop length tail
            )

        [] ->
            ( "", [] )


readString : List Int -> ( String, List Int )
readString bytes =
    let
        length =
            parseNumber (List.take 4 bytes)

        rest =
            List.drop 4 bytes
    in
    ( parseUTF8 (List.take length rest)
    , List.drop length rest
    )


parseNumber : List Int -> Int
parseNumber bytes =
    case bytes of
        b3 :: b2 :: b1 :: b0 :: _ ->
            b0 + (16 * b1) + (256 * b2) + (4096 * b3)

        _ ->
            0


parseUTF8 : List Int -> String
parseUTF8 message =
    case UTF8.toString message of
        Ok value ->
            value

        Err err ->
            err
