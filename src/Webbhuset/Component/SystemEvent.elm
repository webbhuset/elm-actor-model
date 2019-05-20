module Webbhuset.Component.SystemEvent exposing (SystemEvent(..))

import Webbhuset.Internal.PID exposing (PID)

type SystemEvent
    = Gone PID
    | Kill
