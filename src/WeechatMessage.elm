module WeechatMessage exposing (parse)

import String.UTF8 as UTF8


type alias Message =
    { operation : String
    , data : ( String, String )
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
            { operation = operation
            , data = readInfo rest
            }

        _ ->
            { operation = "err"
            , data = ( "err", "err" )
            }



-- OPERATION PARSERS.


readInfo : List Int -> ( String, String )
readInfo message =
    let
        ( key, rest ) =
            readString message

        ( value, _ ) =
            readString rest
    in
    ( key, value )



-- HELPERS


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
