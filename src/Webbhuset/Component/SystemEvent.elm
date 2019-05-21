module Webbhuset.Component.SystemEvent exposing
    ( SystemEvent(..)
    , Handling
    , default
    , doNothing
    , iWillHandleIt
    , mapHandling
    )

{-|

# System Events

@docs SystemEvent

System events are sent to a component's `onSystem` function.

    onSystem : SystemEvent -> SystemEvent.Handling MsgIn
    onSystem event =
        case event of
            PIDNotFound pid ->
                UnObserveData pid
                    |> SystemEvent.iWillHandleIt

            Kill ->
                SystemEvent.default

This gives you the choice of how to handle them by returning `Handling`.

If you don't care you can just set the handling to default in your component's
record.

    component =
        { init = init
        , onSystem = always SystemEvent.default
        ...
        }

## PIDNotFound

If your component sends a message to a PID that does not exist
anymore you will receive a `PIDNotFound` event containing the PID of the
killed process.
This lets you clean up any PID's you stored in your model for example.

**Default handling** for this event is to do nothing.

## Kill

Kill is received when your component is going to be killed. You
have the chance to say some last words before dying.

**Default handling** for Kill is to also kill all the children of
the process.


# Handling

@docs Handling, default, doNothing, iWillHandleIt, mapHandling

-}
import Webbhuset.Internal.PID exposing (PID)
import Webbhuset.Internal.SystemEvent as Internal


{-| System Event

-}
type SystemEvent
    = PIDNotFound PID
    | Kill


{-| How should events be handeled.

-}
type alias Handling msgIn =
    Internal.Handling msgIn


{-| Use event default handling.

-}
default : Handling msgIn
default =
    Internal.Default


{-| Don't do anyting.

-}
doNothing : Handling msgIn
doNothing =
    Internal.DoNothing


{-| Handle it yourself.

-}
iWillHandleIt : msgIn -> Handling msgIn
iWillHandleIt msgIn =
    Internal.HandleWith msgIn


{-| Map the Handling type.

-}
mapHandling : (msg1 -> msg2) -> Handling msg1 -> Handling msg2
mapHandling fn handling =
    case handling of
        Internal.Default ->
            Internal.Default

        Internal.DoNothing ->
            Internal.DoNothing

        Internal.HandleWith msg1 ->
            Internal.HandleWith (fn msg1)

