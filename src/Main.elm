port module Main exposing (..)

import Browser
import Constants exposing (closeEscape, colorEscape)
import Debug
import DecodeMessage exposing (Buffer, BuffersResult(..), Line, LineResult(..), LinesResult(..))
import Dict exposing (Dict)
import Html exposing (Html, button, div, em, h1, img, li, p, text, ul)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List
import Regex exposing (Regex)
import WeechatMessage exposing (Message, Object, WeechatData)



---- MODEL ----


type alias Model =
    { buffers : BuffersModel
    , currentBuffer : Maybe String
    , lines : LinesModel
    }


type BuffersModel
    = BuffersLoaded (List Buffer)
    | BuffersLoading
    | BuffersError String


type LinesModel
    = LinesLoaded (Dict String (List Line))
    | LinesLoading
    | LinesError String


init : ( Model, Cmd Msg )
init =
    ( { buffers = BuffersLoading
      , currentBuffer = Nothing
      , lines = LinesLoading
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Recv Bytes
    | Status Bool
    | ChangeBuffer String


type alias Bytes =
    List Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Status isConnected ->
            ( model
            , Cmd.batch
                -- Init needs to be last for it to be sent first.
                -- This might cause a race condition.
                [ weechatSend "sync\n"
                , weechatSend "(hdata_lines) hdata buffer:gui_buffers(*)/own_lines/first_line(*)/data message,buffer,date,prefix\n"
                , weechatSend "(hdata_buffers) hdata buffer:gui_buffers(*) number,full_name,short_name\n"
                , weechatSend "init password=test,compression=off\n"
                ]
            )

        Recv message ->
            let
                { id, data } =
                    WeechatMessage.parse message

                newModel =
                    case id of
                        Just "hdata_buffers" ->
                            case DecodeMessage.parseHdataBuffers data of
                                Buffers buffers ->
                                    { model
                                        | buffers = BuffersLoaded buffers
                                        , currentBuffer =
                                            List.head buffers
                                                |> Maybe.andThen
                                                    (\b ->
                                                        b.ppath
                                                            |> List.head
                                                    )
                                    }

                                BuffersErr buffersErr ->
                                    { model | buffers = BuffersError buffersErr }

                        Just "hdata_lines" ->
                            case DecodeMessage.parseHdataLines data of
                                Lines lines ->
                                    { model | lines = LinesLoaded lines }

                                LinesErr linesErr ->
                                    { model | lines = LinesError linesErr }

                        Just "_buffer_line_added" ->
                            case DecodeMessage.parseBufferLineAdded data of
                                LineOk line ->
                                    case model.lines of
                                        LinesLoaded lines ->
                                            let
                                                -- Add line to correct buffer.
                                                newLines =
                                                    Dict.update line.buffer
                                                        (\oldBuffer ->
                                                            case oldBuffer of
                                                                Just oldLines ->
                                                                    Just (line :: oldLines)

                                                                Nothing ->
                                                                    Just [ line ]
                                                        )
                                                        lines
                                            in
                                            { model | lines = LinesLoaded newLines }

                                        _ ->
                                            model

                                LineFailure err ->
                                    { model | lines = LinesError err }

                        _ ->
                            model
            in
            ( newModel
            , Cmd.none
            )

        ChangeBuffer buffer ->
            ( { model | currentBuffer = Just buffer }
            , Cmd.none
            )



-- PORTS


port socketStatus : (Bool -> msg) -> Sub msg


port weechatReceive : (Bytes -> msg) -> Sub msg


port weechatSend : String -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.batch [ weechatReceive Recv, socketStatus Status ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "Container" ]
        [ div [ class "Panel" ]
            [ ul [ class "Buffers" ]
                (case model.buffers of
                    BuffersLoading ->
                        [ text "Loading buffers..." ]

                    BuffersError err ->
                        [ text err ]

                    BuffersLoaded buffers ->
                        List.map renderBuffer buffers
                )
            ]
        , div
            [ class "ChatContainer" ]
            (renderChat model.currentBuffer model.lines)
        ]


renderBuffer : Buffer -> Html Msg
renderBuffer buffer =
    let
        pointer =
            List.head buffer.ppath
                |> Maybe.withDefault "Missing buffer pointer."

        name =
            buffer.shortName
                |> Maybe.withDefault buffer.fullName
    in
    li
        [ class "Buffer" ]
        [ button [ onClick (ChangeBuffer pointer) ] [ text name ] ]


renderChat : Maybe String -> LinesModel -> List (Html Msg)
renderChat currentBuffer linesModel =
    case linesModel of
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
    div [ class "LineGroup" ]
        [ h1 [] (lineGroup.prefix |> Maybe.withDefault "Server" |> formatColoredText)
        , div
            [ class "LineGroup-messages" ]
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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> subscriptions
        }
