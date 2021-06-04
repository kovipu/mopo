port module Main exposing (..)

import Browser
import Debug
import DecodeMessage exposing (Buffer, BuffersResult(..), Line, LinesResult(..))
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, img, li, p, text, ul)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List
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
                [ weechatSend "(hdata_lines) hdata buffer:gui_buffers(*)/own_lines/first_line(*)/data message,buffer,date,prefix\n"
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
    let
        _ =
            Debug.log "model" model
    in
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
                        |> Maybe.andThen renderLines
                        |> Maybe.withDefault [ text "Invalid buffer selected." ]

                Nothing ->
                    [ text "No buffer selected" ]


renderLines : List Line -> Maybe (List (Html Msg))
renderLines lines =
    let
        linesGrouped =
            List.foldl reducer [] lines

        _ =
            Debug.log "grouped" linesGrouped
    in
    List.foldr reducer [] lines
        |> List.map renderLineGroup
        |> Just


renderLineGroup : LineGroup -> Html Msg
renderLineGroup lineGroup =
    div [ class "LineGroup" ]
        [ h1 [] [ lineGroup.prefix |> Maybe.withDefault "Server" |> text ]
        , div [ class "LineGroup-messages" ] (List.map (\m -> p [] [ text m ]) lineGroup.messages)
        ]


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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> subscriptions
        }
