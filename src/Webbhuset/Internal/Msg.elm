module Webbhuset.Internal.Msg exposing (..)

import Webbhuset.Internal.PID as PID exposing (PID(..))


type Msg name appMsg
    = None
    | AppMsg appMsg
    | UnmappedMsg appMsg
    | Ctrl (Control name (Msg name appMsg))
    | Context PID (Msg name appMsg)
    | Init (Msg name appMsg) String
    | SetDocumentTitle String


type Control name msg
    = Batch (List msg)
    | Cmd (Cmd msg)
    | Kill PID
    | SendToPID PID msg
    | SendToSingleton name msg
    | Spawn name (PID -> msg)
    | SpawnSingleton name
    | AddView PID
    | WithSingletonPID name (PID -> msg)

