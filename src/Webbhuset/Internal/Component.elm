module Webbhuset.Internal.Component exposing (..)

import Webbhuset.Internal.Msg as Msg
import Webbhuset.Internal.PID as PID exposing (PID)
import Webbhuset.Component.SystemEvent exposing (SystemEvent)


type alias Layout model msgIn msgOut output msg =
    { init : PID -> ( model, List msgOut, Cmd msgIn )
    , update : msgIn -> model -> ( model, List msgOut, Cmd msgIn )
    , view : (msgIn -> msg) -> model -> (PID -> output) -> output
    , onSystem : SystemEvent -> Maybe msgIn
    , subs : model -> Sub msgIn
    }


type alias UI model msgIn msgOut output =
    { init : PID -> ( model, List msgOut, Cmd msgIn )
    , update : msgIn -> model -> ( model, List msgOut, Cmd msgIn )
    , view : model -> output
    , onSystem : SystemEvent -> Maybe msgIn
    , subs : model -> Sub msgIn
    }
