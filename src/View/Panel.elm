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
        [ class "h-full w-56 flex flex-col items-start justify-center" ]
        (case bufferModel of
            BuffersLoading ->
                [ text "Loading buffers..." ]

            BuffersError err ->
                [ text err ]

            BuffersLoaded buffers ->
                List.filter (\buf -> String.startsWith "irc.server" buf.fullName) buffers
                    |> List.map (renderBufferGroup buffers)
        )


renderBufferGroup : List Buffer -> Buffer -> Html Msg
renderBufferGroup buffers server =
    let
        pointer =
            List.head server.ppath
                |> Maybe.withDefault "Missing buffer pointer."

        title =
            server.shortName
                |> Maybe.withDefault server.fullName

        channels =
            case server.shortName of
                Nothing ->
                    [ Html.text "" ]

                Just shortName ->
                    List.filter (\buf -> String.startsWith ("irc." ++ shortName) buf.fullName) buffers
                        |> List.map renderBuffer
    in
    div
        [ class "flex flex-col py-4 items-start justify-start" ]
        (button
            [ class "w-full px-8 py-1 text-timestamp text-left transition ease-in-out"
            , onClick (ChangeBuffer pointer)
            ]
            [ text title ]
            :: channels
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
        [ class "w-full px-8 py-1 hover:bg-input text-left text-lg transition ease-in-out"
        , onClick (ChangeBuffer pointer)
        ]
        [ text name ]
