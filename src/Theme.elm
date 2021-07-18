module Theme exposing (colorCode, theme)

import Element exposing (..)
import Element.HexColor as HexColor



---- THEME ----


theme =
    { panelColor = hex "#011627"
    , background = hex "#011627"
    , loginColor = hex "#0b253a"
    , messageInputColor = hex "#000f1b"
    , mainTextColor = hex "#d6deeb"
    , timestampColor = hex "#575656"

    -- chat colors.
    , color00 = hex "#d6deeb"
    , color01 = hex "#da0f7a"
    , color02 = hex "#22da6e"
    , color03 = hex "#addb67"
    , color04 = hex "#82aaff"
    , color05 = hex "#c792ea"
    , color06 = hex "#21c7a8"
    , color07 = hex "#22da6e"
    , color08 = hex "#da0f7a"
    , color09 = hex "#ff217c"
    , color10 = hex "#22da6e"
    , color11 = hex "#7fdbca"
    , color12 = hex "#82aaff"
    , color13 = hex "#c792ea"
    , color14 = hex "#7fdbca"
    , color15 = hex "#22da6e"
    }



-- util to convert ansi colorcode to colors using theme as a lookup table.


colorCode : String -> Color
colorCode code =
    let
        codeWithoutF =
            String.right 2 code
    in
    case codeWithoutF of
        "00" ->
            theme.color00

        "01" ->
            theme.color01

        "02" ->
            theme.color02

        "03" ->
            theme.color03

        "04" ->
            theme.color04

        "05" ->
            theme.color05

        "06" ->
            theme.color06

        "07" ->
            theme.color07

        "08" ->
            theme.color08

        "09" ->
            theme.color09

        "10" ->
            theme.color10

        "11" ->
            theme.color11

        "12" ->
            theme.color12

        "13" ->
            theme.color13

        "14" ->
            theme.color14

        "15" ->
            theme.color15

        _ ->
            theme.color00



-- util to convert hex string to Color.


hex : String -> Color
hex colorStr =
    case HexColor.hex colorStr of
        Just color ->
            color

        Nothing ->
            rgb255 255 0 0
