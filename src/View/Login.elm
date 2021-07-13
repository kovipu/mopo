module View.Login exposing (render)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (text)
import Theme exposing (theme)
import Types.Model exposing (ConnectionState(..))
import Types.Msg exposing (Msg(..))



---- LOGIN ----


render : ConnectionState -> String -> String -> Element Msg
render connectionState address password =
    column
        [ centerX
        , centerY
        , Background.color theme.loginColor
        , Font.color theme.mainTextColor
        , padding 60
        , spacing 20
        ]
        [ el
            [ Font.size 30 ]
            (Element.text "🛵 Mopo")
        , Input.text
            inputStyle
            { onChange = ChangeAddress
            , text = address
            , placeholder = Nothing
            , label = renderLabel "Address"
            }
        , Input.currentPassword
            inputStyle
            { onChange = ChangePassword
            , text = password
            , placeholder = Nothing
            , show = False
            , label = renderLabel "Password"
            }
        , renderConnectionState connectionState
        ]


inputStyle =
    [ Background.color theme.background
    , Border.width 0
    , Border.rounded 0
    , width <| px 320
    , Font.size 16
    ]


renderLabel label =
    Input.labelAbove
        [ Font.size 18, paddingXY 0 5 ]
        (Element.text label)


renderConnectionState connectionState =
    case connectionState of
        NotConnected ->
            Input.button
                [ Border.color theme.mainTextColor
                , Border.width 2
                , paddingXY 25 10
                ]
                { onPress = Just Connect
                , label = Element.text "Connect"
                }

        Connecting ->
            Element.text "Connecting..."

        Initializing ->
            Element.text "Initializing..."

        Connected ->
            Element.text "Connected"
