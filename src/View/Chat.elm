module View.Chat exposing (render)

import Constants exposing (closeEscape, colorEscape)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Regex exposing (Regex)
import Theme exposing (theme)
import Types.Model exposing (Line, LinesModel(..))
import Types.Msg exposing (Msg)



---- CHAT ----


render : Maybe String -> LinesModel -> Element Msg
render currentBuffer linesModel =
    column
        [ height fill
        , width fill
        , Font.color theme.mainTextColor
        , scrollbarY
        ]
        (case linesModel of
            LinesLoading ->
                [ text "loading lines..." ]

            LinesError err ->
                [ text err ]

            LinesLoaded linesByBuffer ->
                case currentBuffer of
                    Just buffer ->
                        Dict.get buffer linesByBuffer
                            |> Maybe.andThen (\m -> Just (List.take 100 m))
                            |> Maybe.andThen renderLines
                            |> Maybe.withDefault [ text "Invalid buffer selected." ]

                    Nothing ->
                        [ text "No buffer selected" ]
        )


renderLines : List Line -> Maybe (List (Element Msg))
renderLines lines =
    List.foldr reducer [] lines
        |> List.map renderLineGroup
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


renderLineGroup : LineGroup -> Element Msg
renderLineGroup lineGroup =
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
    in
    column
        [ paddingXY 5 2
        ]
        [ row
            [ paddingXY 0 2 ]
            (lineGroup.prefix |> Maybe.withDefault "Server" |> formatColoredText)
        , column
            [ padding 5
            , Border.widthEach { left = 2, bottom = 0, top = 0, right = 0 }
            , Border.roundEach { topLeft = 2, bottomLeft = 2, topRight = 0, bottomRight = 0 }
            , Border.color (Theme.colorCode nickColor)
            ]
            (lineGroup.messages
                |> List.map
                    (\m ->
                        paragraph [] (formatColoredText m)
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
