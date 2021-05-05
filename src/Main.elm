port module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src, class)
import List
import Debug
import String.UTF8 as UTF8


---- MODEL ----


type alias Model =
    {
        messages : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { messages = [] }
    , Cmd.none )



---- UPDATE ----


type Msg
    = Recv (List Int) 
    | Status Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Status isConnected ->
            let _ = Debug.log "status" isConnected
            in 
                ( model
                , Cmd.none
                )

        Recv message ->
            -- Remove header bytes.
            let parsed = parseMessage (List.drop 9 message)
            in
                ( model
                , Cmd.none
                )


parseMessage : List Int -> String
parseMessage message =
    let
        _ = Debug.log "Message" message
        operation = parseString (List.take 3 message)
        _ = Debug.log "Operation" operation

        payloadBytes = List.drop 3 message
        payload = case operation of
            "inf" -> parseInfo payloadBytes
            _ -> ( "err", "err" )
        _ = Debug.log "Payload" payload
    in
        ""


parseString : List Int -> String
parseString message =
    case UTF8.toString message of
        Ok value -> value
        Err err -> err


parseVariableLengthString : List Int -> (String, List Int)
parseVariableLengthString bytes =
    let
        length = parseNumber (List.take 4 bytes)
        rest = List.drop 4 bytes
    in
        ( parseString (List.take length rest)
        , (List.drop length rest ) )



parseInfo : List Int -> ( String, String )
parseInfo message =
    let
        (key, rest) = parseVariableLengthString message
        (value, _) = parseVariableLengthString rest
    in
        ( key, value )


parseNumber : List Int -> Int
parseNumber bytes =
    case bytes of
        (b3::b2::b1::b0::_) -> b0 + (16 * b1) + (256 * b2) + (4096 * b3)
        _ -> 0



-- PORTS


-- Receive socket status on connect.
port socketStatus : (Bool -> msg) -> Sub msg


-- Communicate with Weechat.
port weechatSend : String -> Cmd msg
port weechatReceive : (List Int -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.batch [ weechatReceive Recv, socketStatus Status ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "Container" ]
        [ div [ class "Panel" ] [ text "Panel" ]
        , div
            [ class "ChatContainer" ]
            (List.map renderMessage model.messages)
        ]


renderMessage : String -> Html Msg
renderMessage message =
    div [ class "Message" ]
        [ text message ]


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> subscriptions
        }
