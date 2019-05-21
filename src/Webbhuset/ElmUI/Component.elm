module Webbhuset.ElmUI.Component exposing
    ( Layout
    , Service
    , UI
    , PID
    , Queue
    , addCmd
    , addOutMsg
    , addToQueue
    , emptyQueue
    , andThen
    , mapFirst
    , mapSecond
    , mapThird
    , runQueue
    , toCmd
    , toCmdWithDelay
    )

{-|

@docs PID

# Elm UI Components

This module has an identical API to Webbhuset.Component
but with the view functions returning `Element msg`.

Check Webbhuset.Component for more info.


@docs UI
@docs Service
@docs Layout
    , mapFirst
    , mapSecond
    , mapThird
    , andThen
    , addOutMsg
    , addCmd
    , toCmd
    , toCmdWithDelay
    , Queue
    , emptyQueue
    , addToQueue
    , runQueue
-}
import Element exposing (Element)
import Process
import Task
import Webbhuset.PID as PID
import Webbhuset.Component as Component
import Webbhuset.Component.SystemEvent as SystemEvent exposing (SystemEvent)


{-| A PID is an identifier for a Process.

-}
type alias PID =
    PID.PID


{-| Service Component Type

-}
type alias Service model msgIn msgOut =
    { init : PID -> ( model, List msgOut, Cmd msgIn )
    , update : msgIn -> model -> ( model, List msgOut, Cmd msgIn )
    , onSystem : SystemEvent -> SystemEvent.Handling msgIn
    , subs : model -> Sub msgIn
    }


{-| UI Component Type

-}
type alias UI model msgIn msgOut =
    { init : PID -> ( model, List msgOut, Cmd msgIn )
    , update : msgIn -> model -> ( model, List msgOut, Cmd msgIn )
    , view : model -> Element msgIn
    , onSystem : SystemEvent -> SystemEvent.Handling msgIn
    , subs : model -> Sub msgIn
    }


{-| Layout Component Type

-}
type alias Layout model msgIn msgOut msg =
    { init : PID -> ( model, List msgOut, Cmd msgIn )
    , update : msgIn -> model -> ( model, List msgOut, Cmd msgIn )
    , view : (msgIn -> msg) -> model -> (PID -> Element msg) -> Element msg
    , onSystem : SystemEvent -> SystemEvent.Handling msgIn
    , subs : model -> Sub msgIn
    }


{-| Store messages in a queue.

-}
type alias Queue msgIn =
    Component.Queue msgIn

{-| Run a series of updates on the model

-}
andThen :
    (model -> ( model, List msgOut, Cmd msgIn ))
    -> ( model, List msgOut, Cmd msgIn )
    -> ( model, List msgOut, Cmd msgIn )
andThen fn ( m0, out0, cmd0 ) =
    let
        ( m1, out1, cmd1 ) =
            fn m0
    in
    ( m1
    , out0 ++ out1
    , Cmd.batch
        [ cmd0
        , cmd1
        ]
    )


{-| Map the first argument (Model).

-}
mapFirst : (input -> out) -> ( input, x, y ) -> ( out, x, y )
mapFirst fn ( a, x, y ) =
    ( fn a, x, y )


{-| Map the second argument (List MsgOut).

-}
mapSecond : (input -> out) -> ( x, input, y ) -> ( x, out, y )
mapSecond fn ( x, a, y ) =
    ( x, fn a, y )


{-| Map the third argument (Cmd).

-}
mapThird : (input -> out) -> ( x, y, input ) -> ( x, y, out )
mapThird fn ( x, y, a ) =
    ( x, y, fn a )


{-| Add an out message to the output 3-Tuple.

-}
addOutMsg : msg -> ( x, List msg, y ) -> ( x, List msg, y )
addOutMsg msg ( x, list, y ) =
    ( x, msg :: list, y )


{-| Add a Cmd to the output 3-Tuple.

-}
addCmd : Cmd msg -> ( x, y, Cmd msg ) -> ( x, y, Cmd msg )
addCmd cmd1 ( x, y, cmd0 ) =
    ( x, y, Cmd.batch [ cmd0, cmd1 ] )


{-| Convert a msg to Cmd.

-}
toCmd : msg -> Cmd msg
toCmd msg =
    Task.perform identity
        (Task.succeed msg)


{-| Convert a msg to Cmd with a timeout in milliseconds.

-}
toCmdWithDelay : Float -> msg -> Cmd msg
toCmdWithDelay delay msg =
    Task.perform identity
        (Process.sleep delay
            |> Task.andThen (\_ -> Task.succeed msg)
        )


{-| Create an Empty Queue

-}
emptyQueue : Queue msgIn
emptyQueue =
    Component.emptyQueue


{-| Run the `update` function on all messages in the queue
and compose all output.
-}
runQueue :
    Queue msgIn
    -> (msgIn -> model -> ( model, List msgOut, Cmd msgIn ))
    -> ( model, List msgOut, Cmd msgIn )
    -> ( model, List msgOut, Cmd msgIn )
runQueue =
    Component.runQueue


{-| Add a msg to the queue

-}
addToQueue : msgIn -> Queue msgIn -> Queue msgIn
addToQueue =
    Component.addToQueue
