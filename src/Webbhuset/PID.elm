module Webbhuset.PID exposing (..)

import Webbhuset.Internal.PID as PID exposing (PID(..))


type alias PID = PID.PID


toString : PID -> String
toString (PID prefix pid) =
    prefix ++ (String.fromInt pid)


null : PID
null =
    PID "null" 0
