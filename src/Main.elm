port module Main exposing (..)

import Browser
import Debug
import DecodeMessage exposing (Buffer, BuffersResult(..), parseHdataBuffers)
import Html exposing (Html, div, h1, img, li, text, ul)
import Html.Attributes exposing (class, src)
import List
import WeechatMessage exposing (Message, Object, WeechatData)



---- MODEL ----


type alias Model =
    { buffers : List Buffer
    , buffersErr : Maybe String
    , messages : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { buffers = []
      , buffersErr = Nothing
      , messages = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Recv Bytes
    | Status Bool


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
                [ weechatSend "(hdata_buffers) hdata buffer:gui_buffers(*) number,full_name,short_name\n"
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
                            case parseHdataBuffers data of
                                Buffers buffers ->
                                    { model | buffers = buffers }

                                BuffersErr buffersErr ->
                                    { model | buffersErr = Just buffersErr }

                        _ ->
                            model
            in
            ( newModel
            , Cmd.none
            )



-- PORTS


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
    div [ class "Container" ]
        [ div [ class "Panel" ]
            [ ul [ class "Buffers" ]
                (case model.buffersErr of
                    Just err ->
                        [ text err ]

                    Nothing ->
                        List.map renderBuffer model.buffers
                )
            ]
        , div
            [ class "ChatContainer" ]
            (List.map renderMessage model.messages)
        ]


renderBuffer : Buffer -> Html Msg
renderBuffer buffer =
    let
        name =
            buffer.shortName
                |> Maybe.withDefault buffer.fullName
    in
    li [ class "Buffer" ] [ text name ]


renderMessage : String -> Html Msg
renderMessage message =
    div [ class "Message" ]
        [ text message ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> subscriptions
        }
