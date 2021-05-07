module WeechatMessage exposing (Buffer, Message(..), parse)

import String.UTF8 as UTF8


type Message
    = Info ( Maybe String, Maybe String )
    | Buffers (List Buffer)
    | Invalid String


type alias Buffer =
    { ppath : String
    , number : Int
    , fullName : String
    , shortName : Maybe String
    }


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

        operation =
            List.take 3 payload
                |> parseUTF8

        rest =
            List.drop 3 payload
    in
    case operation of
        "inf" ->
            Info (readInfo rest)

        "hda" ->
            case id of
                Just "hdata_buffers" ->
                    Buffers (readBuffers rest)

                _ ->
                    Invalid "Unknown Hdata operation."

        _ ->
            Invalid "Unknown operation."



-- OPERATION PARSERS


readInfo : Bytes -> ( Maybe String, Maybe String )
readInfo message =
    let
        ( key, rest ) =
            readString message

        ( value, _ ) =
            readString rest
    in
    ( key, value )


readBuffers : Bytes -> List Buffer
readBuffers message =
    let
        -- TODO: make use of this data.
        ( keys, keysrest ) =
            readString message

        ( values, tail ) =
            readString keysrest
    in
    readHashTable (List.drop 4 tail)


readHashTable : Bytes -> List Buffer
readHashTable chunk =
    let
        ( ppath, rest ) =
            readPointer chunk

        number =
            parseNumber (List.take 4 rest)

        ( fullName, tailShortName ) =
            readString (List.drop 4 rest)

        ( shortName, tail ) =
            readString tailShortName

        buffer =
            { ppath = ppath
            , number = number
            , fullName = Maybe.withDefault "" fullName -- This should never be null.
            , shortName = shortName
            }
    in
    if List.length tail == 0 then
        [ buffer ]

    else
        [ buffer ] ++ readHashTable tail



-- HELPERS


readPointer : Bytes -> ( String, Bytes )
readPointer bytes =
    case bytes of
        length :: tail ->
            ( List.take length tail |> parseUTF8
            , List.drop length tail
            )

        [] ->
            ( "", [] )


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


parseNumber : Bytes -> Int
parseNumber bytes =
    case bytes of
        b3 :: b2 :: b1 :: b0 :: _ ->
            b0 + (0x0100 * b1) + (0x00010000 * b2) + (0x01000000 * b3)

        _ ->
            0


parseUTF8 : Bytes -> String
parseUTF8 message =
    case UTF8.toString message of
        Ok value ->
            value

        Err err ->
            err
