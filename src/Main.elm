port module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src, class)
import List
import Debug
import WeechatMessage


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
                , Cmd.batch
                    -- Init needs to be last for it to be sent first.
                    -- This might cause a race condition.
                    [ weechatSend "info version\n"
                    , weechatSend "init password=test,compression=off\n"
                    ]
                )

        Recv message ->
            let
                _ = Debug.log "received message:" message
                _ = Debug.log "parsed message:" (WeechatMessage.parse message)
            in
                ( model
                , Cmd.none
                )



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
