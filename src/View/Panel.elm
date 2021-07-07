module View.Panel exposing (render)

import Html exposing (Html, button, div, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types.Model exposing (Buffer, BuffersModel(..))
import Types.Msg exposing (Msg(..))



---- PANEL ----


render : BuffersModel -> Html Msg
render bufferModel =
    div [ class "Panel" ]
        [ ul [ class "Buffers" ]
            (case bufferModel of
                BuffersLoading ->
                    [ text "Loading buffers..." ]

                BuffersError err ->
                    [ text err ]

                BuffersLoaded buffers ->
                    List.map renderBuffer buffers
            )
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
