module DecodeMessage exposing (Buffer, BuffersResult(..), parseHdataBuffers)

import Dict exposing (Dict)
import WeechatMessage exposing (Object, WeechatData(..))



-- This file includes parsers from WeechatData into specified objects.
-- This is somewhat inspired by Elm's amazing Json.Decode.
-- I'm just not decoding from json here, so had to build something custom.


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
