module View.MessageInput exposing (render)

import Html exposing (Attribute, Html, input)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onInput)
import Json.Decode as Json
import Types.Msg exposing (Msg(..))


---- MessageInput ----


render : String -> Html Msg
render messageInput =
    input
        [ class "MessageInput"
        , placeholder "Message"
        , onInput ChangeInput
        , onKeyDown KeyDown
        , value messageInput
        ]
        []


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)
