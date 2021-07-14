module Types.Msg exposing (Msg(..))

import Types.Bytes exposing (Bytes)



---- MESSAGE ----


type Msg
    = ChangeAddress String
    | ChangePassword String
    | Connect
    | Recv Bytes
    | Status Bool
    | ChangeBuffer String
    | ChangeInput String
    | SendMessage