module Webbhuset.Component.SystemEvent exposing (SystemEvent(..))

{-|

# System Events

- **Gone**: If you send a message to a PID that does not exist
  anymore you will receive a `Gone` event.
- **Kill** is received when your component is going to be killed.

@docs SystemEvent

-}
import Webbhuset.Internal.PID exposing (PID)


{-| System Event

    onSystem : SystemEvent -> Maybe MsgIn
    onSystem event =
        case event of
            Gone pid ->
                UnObserveData pid
                    |> Just

            Kill ->
                Nothing
-}
type SystemEvent
    = Gone PID
    | Kill
