port module Main exposing (..)

import Browser
import Debug
import DecodeMessage exposing (BuffersResult(..), LineResult(..), LinesResult(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Html exposing (Html)
import List
import Theme exposing (theme)
import Types.Bytes exposing (Bytes)
import Types.Model as Model exposing (Buffer, BuffersModel(..), Line, LinesModel(..), Model)
import Types.Msg exposing (Msg(..))
import View.Chat as Chat
import View.MessageInput as MessageInput
import View.Panel as Panel
import WeechatMessage exposing (Message, Object, WeechatData)



---- UPDATE ----


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
            ( newModel
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
            let
                currentBuffer =
                    case model.currentBuffer of
                        Just buffer ->
                            buffer

                        Nothing ->
                            Debug.todo "You tried to send without selecting a buffer."

                message =
                    "input 0x" ++ currentBuffer ++ " " ++ model.messageInput ++ "\n"

                m =
                    Debug.log "message" message
            in
            ( { model | messageInput = "" }
            , weechatSend message
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
    layout [] <|
        row [ height fill, width fill ]
            [ Panel.render model.buffers
            , column
                [ width <| fillPortion 5
                , height fill
                , Background.color theme.chatColor
                ]
                [ Chat.render model.currentBuffer model.lines
                , MessageInput.render model.messageInput
                ]
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> ( Model.default, Cmd.none )
        , update = update
        , subscriptions = \_ -> subscriptions
        }
