module View.Login exposing (render)

import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Types.Model exposing (ConnectionState(..))
import Types.Msg exposing (Msg(..))



---- LOGIN ----


render : ConnectionState -> String -> String -> List (Html Msg)
render connectionState address password =
    [ div
        [ class "mx-auto flex flex-col justify-center items-center text-xl pt-12" ]
        [ h1
            [ class "m-2 text-3xl" ]
            [ text "🛵 Mopo" ]
        , input [ class "w-72 lg:w-96 m-2 p-2 bg-input text-lg", value address, onInput ChangeAddress ] []
        , input [ type_ "password", class "w-72 lg:w-96 m-2 p-2 bg-input text-lg", value password, onInput ChangePassword ] []
        , renderConnectionState connectionState
        ]
    ]


renderConnectionState connectionState =
    case connectionState of
        NotConnected ->
            button [ onClick Connect, class "border-2 border-foreground px-4 py-2 m-2" ] [ text "Connect" ]

        Connecting ->
            text "Connecting..."

        Initializing ->
            text "Initializing..."

        Connected ->
            text "Connected"
