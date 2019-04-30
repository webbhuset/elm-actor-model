module Webbhuset.Internal.Control exposing (..)

import Webbhuset.Internal.PID as PID exposing (PID(..))


type Control actor msg
    = Batch (List msg)
    | Cmd (Cmd msg)
    | Kill PID
    | SendToPID PID msg
    | SendToSingleton actor msg
    | Spawn actor (PID -> msg)
    | SpawnSingleton actor
    | AddView PID

