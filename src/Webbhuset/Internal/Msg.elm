module Webbhuset.Internal.Msg exposing (..)

import Webbhuset.Internal.PID as PID exposing (PID(..))


type Msg name appMsg
    = None
    | AppMsg appMsg
    | UnmappedMsg appMsg
    | Ctrl (Control name (Msg name appMsg))
    | Init (Msg name appMsg) String


type Control actor msg
    = Batch (List msg)
    | Cmd (Cmd msg)
    | Kill PID
    | SendToPID PID msg
    | SendToSingleton actor msg
    | Spawn actor (PID -> msg)
    | SpawnSingleton actor
    | AddView PID

