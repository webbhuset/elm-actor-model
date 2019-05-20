module Webbhuset.Internal.Component exposing (..)

import Webbhuset.Internal.Msg as Msg
import Webbhuset.Internal.PID as PID exposing (PID)


type alias Layout model msgIn msgOut output msg =
    { init : PID -> ( model, List msgOut, Cmd msgIn )
    , update : msgIn -> model -> ( model, List msgOut, Cmd msgIn )
    , view : (msgIn -> msg) -> model -> (PID -> output) -> output
    , kill : model -> List msgOut
    , subs : model -> Sub msgIn
    }


type alias UI model msgIn msgOut output =
    { init : PID -> ( model, List msgOut, Cmd msgIn )
    , update : msgIn -> model -> ( model, List msgOut, Cmd msgIn )
    , view : model -> output
    , kill : model -> List msgOut
    , subs : model -> Sub msgIn
    }