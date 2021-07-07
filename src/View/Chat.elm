module View.Chat exposing (render)

import Types.Model exposing (LinesModel(..), Line)
import Types.Msg exposing (Msg)
import Html exposing (Html, div, text, em, h1, p)
import Html.Attributes exposing (..)
import Dict exposing (Dict)

import Regex exposing (Regex)
import Constants exposing (closeEscape, colorEscape)



---- CHAT ----


render : Maybe String -> LinesModel -> Html Msg
render currentBuffer linesModel =
    div [ class "LinesContainer" ]
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


renderLines : List Line -> Maybe (List (Html Msg))
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


renderLineGroup : LineGroup -> Html Msg
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
    div [ class "LineGroup" ]
        [ h1 [] (lineGroup.prefix |> Maybe.withDefault "Server" |> formatColoredText)
        , div
            [ class ("LineGroup-messages border-color-" ++ nickColor) ]
            (lineGroup.messages
                |> List.reverse
                |> List.map
                    (\m ->
                        p [] (formatColoredText m)
                    )
            )
        ]


formatColoredText : String -> List (Html Msg)
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
                , em [ class className ] (formatColoredText content)
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