module Types.Msg exposing (Msg(..))

import Time
import Types.Bytes exposing (Bytes)



---- MESSAGE ----


type Msg
    = ChangeAddress String
    | ChangePassword String
    | Connect
    | Recv Bytes
    | Status Bool
    | ReceiveSession (Maybe String)
    | ChangeBuffer String
    | ChangeInput String
    | SendMessage
    | TimeZone Time.Zone
