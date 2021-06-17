module DecodeMessage exposing (Buffer, BuffersResult(..), Line, LineResult(..), LinesResult(..), parseBufferLineAdded, parseHdataBuffers, parseHdataLines)

import Dict exposing (Dict)
import WeechatMessage exposing (Object, WeechatData(..))



-- This file includes parsers from WeechatData into specified objects.
-- This is somewhat inspired by Elm's amazing Json.Decode.
-- I'm just not decoding from json here, so had to build something custom.
-- parse hdata_buffers


type alias Buffer =
    { ppath : List String
    , fullName : String
    , shortName : Maybe String
    , number : Int
    }


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
                List.map
                    (\b ->
                        case b of
                            BufferOk buffer ->
                                buffer

                            _ ->
                                Debug.todo "This should never happen."
                    )
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
    -- This is ugly.
    -- Elm's pattern matching *should* have a nicer way to get the pure values out of this.
    -- I was limited by allowing only three values per pattern matched tuple.
    if isJust ppathResult && isJust fullNameResult && isJust shortNameResult && isJust numberResult then
        BufferOk
            { ppath = getJust ppathResult
            , fullName = getJust fullNameResult
            , shortName = getJust shortNameResult
            , number = getJust numberResult
            }

    else
        BufferFailure "Invalid buffer."


type LinesResult
    = Lines (Dict String (List Line))
    | LinesErr String


type alias Line =
    { ppath : List String
    , buffer : String
    , date : String
    , prefix : Maybe String
    , message : String
    }



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
                List.map
                    (\m ->
                        case m of
                            LineOk line ->
                                line

                            _ ->
                                Debug.todo "This should never happen."
                    )
                    parsedLines
                    |> groupLinesByBuffer
                    |> Lines

            else
                LinesErr "An invalid message found."

        _ ->
            LinesErr "Datatype is not Hda."


groupLinesByBuffer : List Line -> Dict String (List Line)
groupLinesByBuffer lines =
    List.foldl
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
    if isJust ppathResult && isJust bufferResult && isJust dateResult && isJust prefixResult && isJust messageResult then
        LineOk
            { ppath = getJust ppathResult
            , buffer = getJust bufferResult
            , date = getJust dateResult
            , prefix = getJust prefixResult
            , message = getJust messageResult
            }

    else
        LineFailure "Invalid message."



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



-- Maybe helpers.


isJust : Maybe v -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


getJust : Maybe v -> v
getJust maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            Debug.todo "You f*cked up."



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
