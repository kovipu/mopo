module View.MessageInput exposing (render)

import Html exposing (Attribute, Html, input)
import Html.Attributes exposing (class, value, placeholder)
import Html.Events exposing (keyCode, on, onInput)
import Json.Decode as Decode
import Types.Msg exposing (Msg(..))



---- MESSAGEINPUT ----


render : String -> Html Msg
render messageInput =
    input [ class "w-full bg-input py-2.5 px-5", placeholder "Write a message", value messageInput, onInput ChangeInput, onEnter SendMessage ] []


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "not ENTER"
    in
    on "keydown" (Decode.andThen isEnter keyCode)
