module Webbhuset.Component exposing
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

# Components

A component is a independent part in the system and has a single responsibility. They
can not import other components.

Components are like normal elm programs. They have their own
model, they can do commands and subscriptions. In addition to that
they also have out messages.

This means you will have two `Msg` types: `MsgIn` and `MsgOut`. MsgIn
is conceptually the same as your normal `Msg` type would be.

MsgOut is a way to "tell" the rest of the system that something happened in your component.

For Example, the msg types for a  login form component could look like this:

    type MsgIn
        = EmailFieldChanged String
        | PasswordFieldChanged String
        | SubmitbuttonClicked

    type MsgOut
        = FormWasSubmitted
            { email : String
            , password : String
            }

From the system's perspective, this is all you need to care about. This
is the public API for the component.

The normal output tuple from the `init` and `update` functions are replaced with
a 3-Tuple:

    ( Model, List MsgOut, Cmd MsgIn )

## UI Component

A UI-component is very similar in concept to a Browser.element program.

@docs UI

## Service Component

The service component does not have any view function. Remember `Platform.worker`?

@docs Service

## Layout Component

A layout component can render other components using their PID as a reference.
The difference comparing to a UI component is the view function.

@docs Layout

## Helpers for the output 3-Tuple

There is no native elm module for a Tuple with three arguments.

@docs mapFirst
    , mapSecond
    , mapThird
    , andThen
    , addOutMsg
    , addCmd
    , toCmd
    , toCmdWithDelay

## Helper for Queue

Sometimes you'd want to put messages in a queue. Maybe your
component is in a state where it can't process them at this point, eg. an InitState.

@docs Queue
    , emptyQueue
    , addToQueue
    , runQueue
-}
import Html exposing (Html)
import Webbhuset.Internal.PID as PID
import Webbhuset.Component.SystemEvent exposing (SystemEvent)
import Task
import Process


{-| A PID is an identifier for a Process.

-}
type alias PID =
    PID.PID


{-| Service Component Type

-}
type alias Service model msgIn msgOut =
    { init : PID -> ( model, List msgOut, Cmd msgIn )
    , update : msgIn -> model -> ( model, List msgOut, Cmd msgIn )
    , onSystem : SystemEvent -> Maybe msgIn
    , subs : model -> Sub msgIn
    }


{-| UI Component Type

- **init**: Is called everytime the component is instantiated.
- **update**: When the component recieves a message.
- **view**: Is called when the app needs to re-render.
- **onSystem**: Called when a system event happens. See Webbhuset.Component.SystemEvent.
- **subs**: Normal Elm Subscriptions, called after update.

-}
type alias UI model msgIn msgOut =
    { init : PID -> ( model, List msgOut, Cmd msgIn )
    , update : msgIn -> model -> ( model, List msgOut, Cmd msgIn )
    , view : model -> Html msgIn
    , onSystem : SystemEvent -> Maybe msgIn
    , subs : model -> Sub msgIn
    }


{-| Layout Component Type

The `view` function of a layout component:

    view : (MsgIn -> msg) -> Model -> (PID -> Html msg) -> Html msg
    view toSelf model renderPID =
        div
            []
            [ renderPID model.child
            , button [ onClick (toSelf ButtonWasClicked) ] [ text "Button!" ]
            ]

The `view` function has three arguments:

- `toSelf` is used to wrap all event-handlers from Html.Events
- `renderPID` is used to render other components.

As you can see, the output type of the `view` function is `Html msg`. This is
necessary to allow components to be composed. What would the return type be on
`renderPID` if they were not mapped to the same type?
-}
type alias Layout model msgIn msgOut msg =
    { init : PID -> ( model, List msgOut, Cmd msgIn )
    , update : msgIn -> model -> ( model, List msgOut, Cmd msgIn )
    , view : (msgIn -> msg) -> model -> (PID -> Html msg) -> Html msg
    , onSystem : SystemEvent -> Maybe msgIn
    , subs : model -> Sub msgIn
    }


{-| Store messages in a queue.

-}
type Queue msgIn =
    Queue (List msgIn)

{-| Run a series of updates on the model

The msgOut's and Cmd's will be composed using `System.batch` and 
`Cmd.batch`.

    ( model, [], Cmd.none )
        |> Component.andThen doSomethingWithModel
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

    ( model, [], Cmd.none )
        |> Component.addOutMsg SomeOutMsg
-}
addOutMsg : msg -> ( x, List msg, y ) -> ( x, List msg, y )
addOutMsg msg ( x, list, y ) =
    ( x, msg :: list, y )


{-| Add a Cmd to the output 3-Tuple.

    ( model, [], Cmd.none )
        |> Component.addCmd cmd
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

    { model
        | queue = Component.emptyQueue
    }
-}
emptyQueue : Queue msgIn
emptyQueue =
    Queue []


{-| Run the `update` function on all messages in the queue
and compose all output.

    ( model, [], Cmd.none )
        |> Component.runQueue queue update
-}
runQueue :
    Queue msgIn
    -> (msgIn -> model -> ( model, List msgOut, Cmd msgIn ))
    -> ( model, List msgOut, Cmd msgIn )
    -> ( model, List msgOut, Cmd msgIn )
runQueue (Queue queuedMsgs) update initial =
    List.foldr
        (\qMsg triplet -> andThen (update qMsg) triplet)
        initial
        queuedMsgs


{-| Add a msg to the queue

    { model
        | queue = Component.addToQueue msgIn model.queue
    }
-}
addToQueue : msgIn -> Queue msgIn -> Queue msgIn
addToQueue msg (Queue queue) =
    msg :: queue
        |> Queue
