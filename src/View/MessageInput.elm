module View.MessageInput exposing (render)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (text)
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Theme exposing (theme)
import Types.Msg exposing (Msg(..))



---- MESSAGEINPUT ----


render : String -> Element Msg
render messageInput =
    Input.text
        [ onEnter SendMessage
        , Background.color theme.messageInputColor
        , Border.width 0
        , Font.color theme.mainTextColor
        ]
        { onChange = \m -> ChangeInput m
        , text = messageInput
        , placeholder = Nothing
        , label = Input.labelHidden "MessageInput"
        }


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )
