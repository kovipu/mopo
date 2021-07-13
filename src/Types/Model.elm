module Types.Model exposing (Buffer, BuffersModel(..), ConnectionState(..), Line, LinesModel(..), Model, default)

import Dict exposing (Dict)



---- MODEL ----


type alias Model =
    { connectionState : ConnectionState
    , address : String
    , password : String
    , buffers : BuffersModel
    , currentBuffer : Maybe String
    , lines : LinesModel
    , messageInput : String
    }



-- Connection


type ConnectionState
    = NotConnected
    | Connecting
    | Initializing
    | Connected



-- Buffers


type BuffersModel
    = BuffersLoaded (List Buffer)
    | BuffersLoading
    | BuffersError String


type alias Buffer =
    { ppath : List String
    , fullName : String
    , shortName : Maybe String
    , number : Int
    }



-- Lines


type LinesModel
    = LinesLoaded (Dict String (List Line))
    | LinesLoading
    | LinesError String


type alias Line =
    { ppath : List String
    , buffer : String
    , date : String
    , prefix : Maybe String
    , message : String
    }


default : Model
default =
    { connectionState = NotConnected
    , address = ""
    , password = ""
    , buffers = BuffersLoading
    , currentBuffer = Nothing
    , lines = LinesLoading
    , messageInput = ""
    }
