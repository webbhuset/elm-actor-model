module Webbhuset.Component
    exposing
        ( Container
        , UI
        , Service
        , Layout
        , PID
        , pidToID
        , wrapLayout
        , wrapUI
        , wrapService
        , applyModel
        , andThen
        , mapFirst
        , mapSecond
        , addOutMsg
        , addCmd
        )

import Webbhuset.ActorSystem as System
import Webbhuset.Ctrl as Ctrl
import Html exposing (Html)
import Html.Lazy as Html
import Browser


type alias PID = System.PID


pidToID : PID -> String
pidToID =
    System.pidToID


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


type alias Container model process msg =
    { init : PID -> (process, msg)
    , recv : model -> msg -> PID -> (process, msg)
    , view : model -> PID -> (PID -> Html msg) -> Html msg
    , kill : model -> PID -> msg
    , subs : model -> PID -> Sub msg
    }


applyModel : Container model process msg -> model -> System.AppliedContainer process msg
applyModel container model =
    { init = container.init
    , recv = container.recv model
    , view = container.view model
    , kill = container.kill model
    , subs = container.subs model
    }



wrapLayout :
    (model -> process)
    -> (msgIn -> msgTo)
    -> (System.Msg component msgTo -> Maybe msgIn)
    -> (msgOut -> System.Msg component msgTo)
    -> Layout model msgIn msgOut (System.Msg component msgTo)
    -> Container model process (System.Msg component msgTo)
wrapLayout toProcess toSelf fromGlobal toGlobal impl =
    { init = \pid ->
        impl.init pid
            |> wrapTriple toSelf toGlobal pid
            |> Tuple.mapFirst toProcess
    , recv = wrapRecv toProcess toSelf fromGlobal toGlobal impl.recv
    , view = \s pid ->
        impl.view
            ( toSelf
                >> Ctrl.msgTo
                >> Ctrl.sendToPID pid
            )
            s
    , kill = wrapKill toGlobal impl.kill
    , subs = wrapSub toSelf impl
    }


wrapKill toGlobal impl model pid =
    impl model
        |> List.map toGlobal
        |> Ctrl.batch


wrapUI :
    (model -> process)
    -> (msgIn -> msgTo)
    -> (System.Msg component msgTo -> Maybe msgIn)
    -> (msgOut -> System.Msg component msgTo)
    -> UI model msgIn msgOut
    -> Container model process (System.Msg component msgTo)
wrapUI toProcess toSelf fromGlobal toGlobal impl =
    { init = \pid ->
        impl.init pid
            |> wrapTriple toSelf toGlobal pid
            |> Tuple.mapFirst toProcess
    , recv = wrapRecv toProcess toSelf fromGlobal toGlobal impl.recv
    , view = \s pid _ ->
        Html.lazy impl.view s
            |> Html.map
                ( toSelf
                    >> Ctrl.msgTo
                    >> Ctrl.sendToPID pid
                )
    , kill = wrapKill toGlobal impl.kill
    , subs = wrapSub toSelf impl
    }


wrapService :
    (model -> process)
    -> (msgIn -> msgTo)
    -> (System.Msg component msgTo -> Maybe msgIn)
    -> (msgOut -> System.Msg component msgTo)
    -> Service model msgIn msgOut
    -> Container model process (System.Msg component msgTo)
wrapService toProcess toSelf fromGlobal toGlobal impl =
    { init = \pid ->
        impl.init pid
            |> wrapTriple toSelf toGlobal pid
            |> Tuple.mapFirst toProcess
    , recv = wrapRecv toProcess toSelf fromGlobal toGlobal impl.recv
    , view = \_ _ _ -> Html.text ""
    , kill = wrapKill toGlobal impl.kill
    , subs = wrapSub toSelf impl
    }


wrapSub :
    (msgIn -> msgTo)
    -> { a | subs : model -> Sub msgIn }
    -> model
    -> PID
    -> Sub (System.Msg component msgTo)
wrapSub toSelf impl model pid =
    let
        sub =
            impl.subs model
    in
    if sub == Sub.none then
        Sub.none
    else
        Sub.map
            ( toSelf
                >> Ctrl.msgTo
                >> Ctrl.sendToPID pid
            )
            sub


wrapTriple :
    (msgIn -> msgTo)
    -> (msgOut -> System.Msg component msgTo)
    -> PID
    -> ( model, List msgOut, Cmd msgIn )
    -> ( model, System.Msg component msgTo )
wrapTriple toSelf toGlobal pid (model, msgsOut, cmd) =
    let
        msgCmd =
            if cmd == Cmd.none then
                Ctrl.none
            else
                Cmd.map
                    ( toSelf
                        >> Ctrl.msgTo
                        >> Ctrl.sendToPID pid
                    )
                    cmd
                    |> Ctrl.cmd
        msg =
            List.map toGlobal msgsOut
                |> (::) msgCmd
                |> Ctrl.batch
    in
    ( model
    , msg
    )


wrapRecv :
    ( model -> process )
    -> (msgIn -> msg)
    -> (System.Msg component msg -> Maybe msgIn)
    -> (msgOut -> System.Msg component msg)
    -> (msgIn -> model -> ( model, List msgOut, Cmd msgIn ))
    -> ( model -> System.Msg component msg -> PID -> ( process, System.Msg component msg ))
wrapRecv toProcess toSelf fromGlobal toGlobal recv model msg pid =
    case fromGlobal msg of
        Just msgIn ->
            recv msgIn model
                |> wrapTriple toSelf toGlobal pid
                |> Tuple.mapFirst toProcess

        Nothing ->
            ( toProcess model, Ctrl.none )



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
