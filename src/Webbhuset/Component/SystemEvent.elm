module Webbhuset.Component.SystemEvent exposing (SystemEvent(..))

{-|

# System Events

System events are sent to a component's `onSystem` function.

    onSystem : SystemEvent -> Maybe MsgIn
    onSystem event =
        case event of
            Gone pid ->
                UnObserveData pid
                    |> Just

            Kill ->
                Nothing

Mapping a system event to a component in-message you can respond to
the following:


## Gone

If your component sends a message to a PID that does not exist
anymore you will receive a `Gone` event containing the PID of the
killed process.
This lets you clean up any PID's you stored in your model for example.

## Kill

Kill is received when your component is going to be killed. You
have the chance to say some last words before dying.

@docs SystemEvent

-}
import Webbhuset.Internal.PID exposing (PID)


{-| System Event

-}
type SystemEvent
    = Gone PID
    | Kill
