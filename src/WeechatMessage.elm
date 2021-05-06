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
            Buffers (readHdata rest)

        _ ->
            Invalid "Unknown operation."



-- OPERATION PARSERS


readInfo : List Int -> ( Maybe String, Maybe String )
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
    readHashTable (List.drop 4 tail)


readHashTable : List Int -> List Buffer
readHashTable chunk =
    let
        ( hpath, rest ) =
            readPointer chunk

        number =
            parseNumber (List.take 4 rest)

        ( fullName, tailShortName ) =
            readString (List.drop 4 rest)

        ( shortName, tail ) =
            readString tailShortName

        buffer =
            { ppath = hpath
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


readPointer : List Int -> ( String, List Int )
readPointer bytes =
    case bytes of
        length :: tail ->
            ( parseUTF8 (List.take length tail)
            , List.drop length tail
            )

        [] ->
            ( "", [] )


readString : List Int -> ( Maybe String, List Int )
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


parseNumber : List Int -> Int
parseNumber bytes =
    case bytes of
        b3 :: b2 :: b1 :: b0 :: _ ->
            b0 + (0x0100 * b1) + (0x00010000 * b2) + (0x01000000 * b3)

        _ ->
            0


parseUTF8 : List Int -> String
parseUTF8 message =
    case UTF8.toString message of
        Ok value ->
            value

        Err err ->
            err
