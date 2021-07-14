port module Main exposing (..)

import Browser
import DecodeMessage exposing (BuffersResult(..), LineResult(..), LinesResult(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Html exposing (Html)
import List
import Theme exposing (theme)
import Types.Bytes exposing (Bytes)
import Types.Model as Model exposing (Buffer, BuffersModel(..), ConnectionState(..), Line, LinesModel(..), Model)
import Types.Msg exposing (Msg(..))
import View.Chat as Chat
import View.Login as Login
import View.MessageInput as MessageInput
import View.Panel as Panel
import WeechatMessage exposing (Message, Object, WeechatData)



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeAddress addr ->
            ( { model | address = addr }
            , Cmd.none
            )

        ChangePassword pw ->
            ( { model | password = pw }
            , Cmd.none
            )

        Connect ->
            ( { model | connectionState = Connecting }
            , connectWebSocket model.address
            )

        Status isConnected ->
            ( { model | connectionState = Initializing }
            , Cmd.batch
                -- Init needs to be last for it to be sent first.
                -- This might cause a race condition.
                [ weechatSend "sync\n"
                , weechatSend "(hdata_lines) hdata buffer:gui_buffers(*)/own_lines/first_line(*)/data message,buffer,date,prefix\n"
                , weechatSend "(hdata_buffers) hdata buffer:gui_buffers(*) number,full_name,short_name\n"
                , weechatSend <| "init password=" ++ model.password ++ ",compression=off\n"
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
                                                                    Just (oldLines ++ [ line ])

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
            ( { newModel | connectionState = Connected }
            , Cmd.none
            )

        ChangeBuffer buffer ->
            ( { model | currentBuffer = Just buffer }
            , Cmd.none
            )

        -- Save message to model on change.
        ChangeInput message ->
            ( { model | messageInput = message }
            , Cmd.none
            )

        -- Send message on enter pressed.
        SendMessage ->
            case model.currentBuffer of
                Just buffer ->
                    let
                        message =
                            "input 0x" ++ buffer ++ " " ++ model.messageInput ++ "\n"
                    in
                    ( { model | messageInput = "" }
                    , weechatSend message
                    )
                Nothing ->
                    ( model, Cmd.none )



-- PORTS


port connectWebSocket : String -> Cmd msg


port socketStatus : (Bool -> msg) -> Sub msg


port weechatSend : String -> Cmd msg


port weechatReceive : (Bytes -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.batch [ weechatReceive Recv, socketStatus Status ]



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [] <|
        row
            [ height fill
            , width fill
            , Background.color theme.background
            ]
            (case model.connectionState of
                Connected ->
                    [ Panel.render model.buffers
                    , column
                        [ width <| fillPortion 5
                        , height fill
                        ]
                        [ Chat.render model.currentBuffer model.lines
                        , MessageInput.render model.messageInput
                        ]
                    ]

                _ ->
                    [ Login.render model.connectionState model.address model.password ]
            )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> ( Model.default, Cmd.none )
        , update = update
        , subscriptions = \_ -> subscriptions
        }
