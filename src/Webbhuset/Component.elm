module Webbhuset.Component exposing
    ( Layout
    , Service
    , UI
    , PID
    , addCmd
    , addOutMsg
    , addToQueue
    , andThen
    , mapFirst
    , mapSecond
    , runQueue
    )

{-| Build Components

@docs PID

## UI component

@docs UI

## Service Component

@docs Service

## Layout Component

@docs Layout

## Helpers for the output Tuple

There is no native elm module for a Tuple with three entries.

@docs mapFirst
    , mapSecond
    , andThen
    , addOutMsg
    , addCmd

## Helper for Queue

@docs addToQueue
    , runQueue
-}
import Html exposing (Html)
import Html.Lazy as Html
import Webbhuset.Internal.PID as PID


{-| A PID is an identifier for a Process.

-}
type alias PID =
    PID.PID


{-| Service Component Type

A Service Component does not have any view function.
-}
type alias Service model msgIn msgOut =
    { init : PID -> ( model, List msgOut, Cmd msgIn )
    , update : msgIn -> model -> ( model, List msgOut, Cmd msgIn )
    , kill : model -> List msgOut
    , subs : model -> Sub msgIn
    }


{-| UI Component Type

A UI Component is similar to a Browser.element program.
-}
type alias UI model msgIn msgOut =
    { init : PID -> ( model, List msgOut, Cmd msgIn )
    , update : msgIn -> model -> ( model, List msgOut, Cmd msgIn )
    , view : model -> Html msgIn
    , kill : model -> List msgOut
    , subs : model -> Sub msgIn
    }


{-| Layout Component Type

A Layout Component can render other components using
their PID.
It differs from a UI component only on its `view` function.
-}
type alias Layout model msgIn msgOut msg =
    { init : PID -> ( model, List msgOut, Cmd msgIn )
    , update : msgIn -> model -> ( model, List msgOut, Cmd msgIn )
    , view : (msgIn -> msg) -> model -> (PID -> Html msg) -> Html msg
    , kill : model -> List msgOut
    , subs : model -> Sub msgIn
    }


{-| Run a series of updates on the model

The msgOut's and Cmd's will be appended using `System.batch` and 
`Cmd.batch`.
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


{-| Map the Model

-}
mapFirst : (a -> b) -> ( a, x, y ) -> ( b, x, y )
mapFirst fn ( a, x, y ) =
    ( fn a, x, y )


{-| Map msgOut

-}
mapSecond : (a -> b) -> ( x, a, y ) -> ( x, b, y )
mapSecond fn ( x, a, y ) =
    ( x, fn a, y )


{-| Add an out message to the list.

-}
addOutMsg : msg -> ( x, List msg, y ) -> ( x, List msg, y )
addOutMsg msg ( x, list, y ) =
    ( x, msg :: list, y )


{-| Add a Cmd to the list.

-}
addCmd : Cmd msg -> ( x, y, Cmd msg ) -> ( x, y, Cmd msg )
addCmd cmd1 ( x, y, cmd0 ) =
    ( x, y, Cmd.batch [ cmd0, cmd1 ] )


{-| Run the queue and compose all output.

-}
runQueue :
    List msgIn
    -> (msgIn -> model -> ( model, List msgOut, Cmd msgIn ))
    -> ( model, List msgOut, Cmd msgIn )
    -> ( model, List msgOut, Cmd msgIn )
runQueue queuedMsgs update initial =
    List.foldr
        (\qMsg triplet -> andThen (update qMsg) triplet)
        initial
        queuedMsgs


{-| Add a msg to the queue

-}
addToQueue : msgIn -> List msgIn -> List msgIn
addToQueue msg queue =
    msg :: queue
