module View.Panel exposing (render)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Events exposing (onClick)
import Theme exposing (theme)
import Types.Model exposing (Buffer, BuffersModel(..))
import Types.Msg exposing (Msg(..))



---- PANEL ----


render : BuffersModel -> Element Msg
render bufferModel =
    column
        [ height fill
        , width <| fillPortion 1
        , paddingXY 0 10
        , Font.color theme.mainTextColor
        , Background.color theme.panelColor
        ]
        (case bufferModel of
            BuffersLoading ->
                [ text "Loading buffers..." ]

            BuffersError err ->
                [ text err ]

            BuffersLoaded buffers ->
                List.map renderBuffer buffers
        )


renderBuffer : Buffer -> Element Msg
renderBuffer buffer =
    let
        pointer =
            List.head buffer.ppath
                |> Maybe.withDefault "Missing buffer pointer."

        name =
            buffer.shortName
                |> Maybe.withDefault buffer.fullName
    in
    Input.button
        [ paddingXY 15 5
        ]
        { onPress = Just (ChangeBuffer pointer)
        , label = text name
        }
