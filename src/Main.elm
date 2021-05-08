port module Main exposing (..)

import Browser
import Debug
import Dict exposing (Dict)
import Html exposing (Html, div, h1, img, li, text, ul)
import Html.Attributes exposing (class, src)
import List
import WeechatMessage exposing (Message, Object, WeechatData(..))



---- MODEL ----


type alias Model =
    { buffers : List Object
    , messages : List String
    }


type alias Buffer =
    { pointer : String
    , fullName : String
    , shortName : Maybe String
    , number : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { buffers = []
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
                            case data of
                                Hda buffers ->
                                    { model | buffers = buffers }

                                _ ->
                                    model

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
        [ div [ class "Panel" ] [ ul [ class "Buffers" ] (List.map renderBuffer model.buffers) ]
        , div
            [ class "ChatContainer" ]
            (List.map renderMessage model.messages)
        ]


renderBuffer : Object -> Html Msg
renderBuffer buffer =
    let
        shortName =
            Dict.get "short_name" buffer

        bufferName =
            case shortName of
                Just value ->
                    case value of
                        Str str ->
                            Maybe.withDefault "No short name." str

                        _ ->
                            Debug.todo "Invalid value."

                Nothing ->
                    Debug.todo "Field does not exist."
    in
    li [ class "Buffer" ] [ text bufferName ]


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
