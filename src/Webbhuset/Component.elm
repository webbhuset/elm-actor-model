module Webbhuset.Component
    exposing
        ( UI
        , Service
        , Layout
        , PID
        , andThen
        , mapFirst
        , mapSecond
        , addOutMsg
        , addCmd
        )

import Webbhuset.ActorSystem as System
import Webbhuset.Internal.PID as PID
import Html exposing (Html)
import Html.Lazy as Html
import Browser


type alias PID = PID.PID -- Just for convenience


type alias Service model msgIn msgOut =
    { init : PID -> (model, List msgOut, Cmd msgIn)
    , recv : msgIn -> model -> (model, List msgOut, Cmd msgIn)
    , kill : model -> List msgOut
    , subs : model -> Sub msgIn
    }


type alias UI model msgIn msgOut =
    { init : PID -> (model, List msgOut, Cmd msgIn)
    , recv : msgIn -> model -> (model, List msgOut, Cmd msgIn)
    , view : model -> Html msgIn
    , kill : model -> List msgOut
    , subs : model -> Sub msgIn
    }


type alias Layout model msgIn msgOut msg =
    { init : PID -> (model, List msgOut, Cmd msgIn)
    , recv : msgIn -> model -> (model, List msgOut, Cmd msgIn)
    , view : (msgIn -> msg) -> model -> (PID -> Html msg) -> Html msg
    , kill : model -> List msgOut
    , subs : model -> Sub msgIn
    }


andThen : (model -> ( model, List msgOut, Cmd msgIn ))
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


mapFirst : (a -> b) -> ( a, x, y ) -> ( b, x, y )
mapFirst fn ( a, x, y ) =
    ( fn a, x, y )


mapSecond : (a -> b) -> ( x, a, y ) -> ( x, b, y )
mapSecond fn ( x, a, y ) =
    ( x, fn a, y )


addOutMsg : msg -> ( x, List msg, y ) -> ( x, List msg, y )
addOutMsg msg ( x, list, y ) =
    ( x, msg :: list, y )


addCmd : Cmd msg -> ( x, y, Cmd msg ) -> ( x, y, Cmd msg )
addCmd cmd1 ( x, y, cmd0 ) =
    ( x, y, Cmd.batch [ cmd0, cmd1 ] )
