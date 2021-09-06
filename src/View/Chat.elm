module View.Chat exposing (render)

import Constants exposing (closeEscape, colorEscape)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Regex exposing (Regex)
import Theme exposing (theme)
import Time
import Types.Model exposing (Line, LinesModel(..), Model)
import Types.Msg exposing (Msg)



---- CHAT ----


render : Model -> Element Msg
render model =
    column
        [ height fill
        , width fill
        , Font.color theme.mainTextColor
        , scrollbarY
        , paddingXY 5 0
        ]
        (case model.lines of
            LinesLoading ->
                [ text "loading lines..." ]

            LinesError err ->
                [ text err ]

            LinesLoaded linesByBuffer ->
                case model.currentBuffer of
                    Just buffer ->
                        Dict.get buffer linesByBuffer
                            |> Maybe.andThen (\m -> Just (List.take 100 m))
                            |> Maybe.andThen (\m -> Just (List.reverse m))
                            |> Maybe.andThen (\m -> renderLines model.timeZone m)
                            |> Maybe.withDefault [ text "Invalid buffer selected." ]

                    Nothing ->
                        [ text "No buffer selected" ]
        )


renderLines : Time.Zone -> List Line -> Maybe (List (Element Msg))
renderLines timeZone lines =
    List.foldr reducer [] lines
        |> List.map (\group -> renderLineGroup timeZone group)
        |> Just


type alias LineGroup =
    { prefix : Maybe String
    , date : String
    , messages : List String
    }


reducer : Line -> List LineGroup -> List LineGroup
reducer line acc =
    let
        prevPrefix =
            List.head acc
                |> Maybe.andThen .prefix
    in
    case acc of
        head :: tail ->
            if line.prefix == prevPrefix then
                { prefix = line.prefix
                , date = line.date
                , messages = line.message :: head.messages
                }
                    :: tail

            else
                { prefix = line.prefix
                , date = line.date
                , messages = [ line.message ]
                }
                    :: acc

        [] ->
            [ { prefix = line.prefix
              , date = line.date
              , messages = [ line.message ]
              }
            ]


renderLineGroup : Time.Zone -> LineGroup -> Element Msg
renderLineGroup timeZone lineGroup =
    let
        nick =
            lineGroup.prefix
                |> Maybe.withDefault "Server"

        escapeChar =
            String.slice 4 5 nick

        nickColor =
            if escapeChar == colorEscape then
                if String.slice 5 6 nick == "F" then
                    String.slice 5 8 nick

                else
                    String.slice 5 7 nick

            else
                "F00"

        timestamp =
            lineGroup.date
                |> String.toInt
                |> Maybe.andThen (\m -> Just (Time.millisToPosix (m * 1000)))
                |> Maybe.andThen
                    (\t ->
                        Just
                            (String.fromInt (Time.toHour timeZone t)
                                ++ ":"
                                ++ String.pad 2 '0' (String.fromInt (Time.toMinute timeZone t))
                            )
                    )
                |> Maybe.withDefault ""
    in
    column
        [ paddingXY 5 2
        , width fill
        ]
        [ row
            [ paddingXY 0 2, width fill ]
            [ row [] (lineGroup.prefix |> Maybe.withDefault "Server" |> formatColoredText)
            , el [ alignRight, Font.color theme.timestampColor ] (text timestamp)
            ]
        , column
            [ padding 5
            , Border.widthEach { left = 2, bottom = 0, top = 0, right = 0 }
            , Border.roundEach { topLeft = 2, bottomLeft = 2, topRight = 0, bottomRight = 0 }
            , Border.color (Theme.colorCode nickColor)
            ]
            (lineGroup.messages
                |> List.map
                    (\m ->
                        paragraph [ padding 2 ] (formatColoredText m)
                    )
            )
        ]


formatColoredText : String -> List (Element Msg)
formatColoredText line =
    let
        re =
            Maybe.withDefault Regex.never <|
                Regex.fromString (colorEscape ++ "|" ++ closeEscape)

        splitAtColorCode =
            Regex.splitAtMost 2 re line
    in
    case splitAtColorCode of
        before :: inner :: tail ->
            let
                firstChar =
                    String.left 1 inner

                colorCodeLength =
                    if isInt firstChar || firstChar == "F" then
                        if firstChar == "F" then
                            3

                        else
                            2

                    else
                        0

                colorCode =
                    String.left colorCodeLength inner

                content =
                    String.dropLeft colorCodeLength inner

                className =
                    "highlight-" ++ colorCode

                tailJoined =
                    if tail == [] then
                        ""

                    else
                        colorEscape ++ String.join colorEscape tail
            in
            if colorCodeLength == 0 then
                [ text before ] ++ formatColoredText content ++ formatColoredText tailJoined

            else
                [ text before
                , paragraph
                    [ Font.color (Theme.colorCode colorCode) ]
                    (formatColoredText content)
                ]
                    ++ formatColoredText tailJoined

        [ msg ] ->
            [ text msg ]

        [] ->
            []


isInt : String -> Bool
isInt s =
    case String.toInt s of
        Just _ ->
            True

        Nothing ->
            False
