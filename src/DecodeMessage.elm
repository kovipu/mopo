module DecodeMessage exposing (BuffersResult(..), LineResult(..), LinesResult(..), parseBufferLineAdded, parseHdataBuffers, parseHdataLines)

import Dict exposing (Dict)
import Types.Model exposing (Buffer, Line)
import WeechatMessage exposing (Object, WeechatData(..))



-- This file includes parsers from WeechatData into specified objects.
-- This is somewhat inspired by Elm's amazing Json.Decode.
-- I'm just not decoding from json here, so had to build something custom.
-- parse hdata_buffers


type BuffersResult
    = Buffers (List Buffer)
    | BuffersErr String


parseHdataBuffers : WeechatData -> BuffersResult
parseHdataBuffers data =
    case data of
        Hda buffers ->
            let
                parsedBuffers =
                    List.map parseBuffer buffers
            in
            if allValidBuffers parsedBuffers then
                List.foldl
                    (\b acc ->
                        case b of
                            BufferOk buffer ->
                                buffer :: acc

                            -- If there's a failure, we return BuffersErr already.
                            -- So this state is never reached.
                            BufferFailure _ ->
                                acc
                    )
                    []
                    parsedBuffers
                    |> Buffers

            else
                BuffersErr "An invalid buffer found."

        _ ->
            BuffersErr "Datatype is not Hda."


allValidBuffers : List BufferResult -> Bool
allValidBuffers buffers =
    List.all
        (\b ->
            case b of
                BufferOk _ ->
                    True

                BufferFailure _ ->
                    False
        )
        buffers


type BufferResult
    = BufferOk Buffer
    | BufferFailure String


parseBuffer : Object -> BufferResult
parseBuffer data =
    let
        ppathResult =
            getPpathOrFail "ppath" data

        fullNameResult =
            getStrOrFail "full_name" data

        shortNameResult =
            getMaybeStrOrFail "short_name" data

        numberResult =
            getIntOrFail "number" data
    in
    Maybe.map4
        (\ppath fullName shortName number ->
            BufferOk { ppath = ppath, fullName = fullName, shortName = shortName, number = number }
        )
        ppathResult
        fullNameResult
        shortNameResult
        numberResult
        |> Maybe.withDefault (BufferFailure "Invalid buffer.")


type LinesResult
    = Lines (Dict String (List Line))
    | LinesErr String



-- parse hdata_lines


parseHdataLines : WeechatData -> LinesResult
parseHdataLines data =
    case data of
        Hda lines ->
            let
                parsedLines =
                    List.map parseLine lines
            in
            if allValidLines parsedLines then
                List.foldl
                    (\m acc ->
                        case m of
                            LineOk line ->
                                line :: acc

                            -- This state is never reached, as we check all lines are valid.
                            LineFailure _ ->
                                acc
                    )
                    []
                    parsedLines
                    |> groupLinesByBuffer
                    |> Lines

            else
                LinesErr "An invalid message found."

        _ ->
            LinesErr "Datatype is not Hda."


groupLinesByBuffer : List Line -> Dict String (List Line)
groupLinesByBuffer lines =
    List.foldr
        (\m acc ->
            let
                oldBuffer =
                    Dict.get m.buffer acc
                        |> Maybe.withDefault []

                newBuffer =
                    m :: oldBuffer
            in
            Dict.insert m.buffer newBuffer acc
        )
        Dict.empty
        lines


allValidLines : List LineResult -> Bool
allValidLines lines =
    List.all
        (\b ->
            case b of
                LineOk _ ->
                    True

                LineFailure fail ->
                    False
        )
        lines


type LineResult
    = LineOk Line
    | LineFailure String


parseLine : Object -> LineResult
parseLine data =
    let
        ppathResult =
            getPpathOrFail "ppath" data

        bufferResult =
            getPointerOrFail "buffer" data

        dateResult =
            getTimeOrFail "date" data

        prefixResult =
            getMaybeStrOrFail "prefix" data

        messageResult =
            getStrOrFail "message" data
    in
    Maybe.map5
        (\ppath buffer date prefix message ->
            LineOk { ppath = ppath, buffer = buffer, date = date, prefix = prefix, message = message }
        )
        ppathResult
        bufferResult
        dateResult
        prefixResult
        messageResult
        |> Maybe.withDefault (LineFailure "Invalid message.")



-- parse _buffer_line_added


parseBufferLineAdded : WeechatData -> LineResult
parseBufferLineAdded data =
    case data of
        Hda objects ->
            case List.head objects of
                Just object ->
                    parseLine object

                Nothing ->
                    LineFailure "Empty array."

        _ ->
            LineFailure "Datatype is not Hda."


-- Object helpers.


getPpathOrFail : String -> Object -> Maybe (List String)
getPpathOrFail field object =
    Dict.get field object
        |> Maybe.andThen
            (\weechatData ->
                case weechatData of
                    Pth ppath ->
                        Just ppath

                    _ ->
                        Nothing
            )


getPointerOrFail : String -> Object -> Maybe String
getPointerOrFail field object =
    Dict.get field object
        |> Maybe.andThen
            (\weechatData ->
                case weechatData of
                    Ptr pointer ->
                        Just pointer

                    _ ->
                        Nothing
            )


getStrOrFail : String -> Object -> Maybe String
getStrOrFail field object =
    Dict.get field object
        |> Maybe.andThen
            (\weechatData ->
                case weechatData of
                    Str str ->
                        Maybe.andThen (\s -> Just s) str

                    _ ->
                        Nothing
            )


getMaybeStrOrFail : String -> Object -> Maybe (Maybe String)
getMaybeStrOrFail field object =
    Dict.get field object
        |> Maybe.andThen
            (\weechatData ->
                case weechatData of
                    Str str ->
                        Just str

                    _ ->
                        Nothing
            )


getIntOrFail : String -> Object -> Maybe Int
getIntOrFail field object =
    Dict.get field object
        |> Maybe.andThen
            (\weechatData ->
                case weechatData of
                    Int int ->
                        Just int

                    _ ->
                        Nothing
            )


getTimeOrFail : String -> Object -> Maybe String
getTimeOrFail field object =
    Dict.get field object
        |> Maybe.andThen
            (\weechatData ->
                case weechatData of
                    Tim time ->
                        Just time

                    _ ->
                        Nothing
            )
