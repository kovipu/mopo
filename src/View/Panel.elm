module View.Panel exposing (render)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types.Model exposing (Buffer, BuffersModel(..))
import Types.Msg exposing (Msg(..))



---- PANEL ----


render : BuffersModel -> Html Msg
render bufferModel =
    div
        [ class "h-full w-56 flex flex-col items-start justify-center text-lg" ]
        (case bufferModel of
            BuffersLoading ->
                [ text "Loading buffers..." ]

            BuffersError err ->
                [ text err ]

            BuffersLoaded buffers ->
                List.map renderBuffer buffers
        )


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
    button
        [ class "w-full px-8 py-1 hover:bg-input text-left", onClick (ChangeBuffer pointer) ]
        [ text name ]
