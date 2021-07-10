module Types.Msg exposing (Msg(..))

import Types.Bytes exposing (Bytes)



---- MESSAGE ----


type Msg
    = Recv Bytes
    | Status Bool
    | ChangeBuffer String
    | ChangeInput String
    | SendMessage
