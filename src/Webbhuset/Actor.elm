module Webbhuset.Actor exposing
    ( Actor
    , PID
    , applyModel
    , fromLayout
    , fromService
    , fromUI
    )

import Browser
import Html exposing (Html)
import Html.Lazy as Html
import Webbhuset.ActorSystem as System
import Webbhuset.Component as Component
import Webbhuset.Internal.Control as Control exposing (Control(..))
import Webbhuset.Internal.PID as PID


type alias PID =
    PID.PID


type alias Actor model process msg =
    { init : PID -> ( process, msg )
    , update : model -> msg -> PID -> ( process, msg )
    , view : model -> PID -> (PID -> Html msg) -> Html msg
    , kill : model -> PID -> msg
    , subs : model -> PID -> Sub msg
    }


applyModel : Actor model process msg -> model -> System.AppliedActor process msg
applyModel actor model =
    { init = actor.init
    , update = actor.update model
    , view = actor.view model
    , kill = actor.kill model
    , subs = actor.subs model
    }


fromLayout :
    (model -> process)
    -> (msgIn -> msgTo)
    -> (System.Msg component msgTo -> Maybe msgIn)
    -> (PID -> msgOut -> System.Msg component msgTo)
    -> Component.Layout model msgIn msgOut (System.Msg component msgTo)
    -> Actor model process (System.Msg component msgTo)
fromLayout toProcess toSelf fromGlobal toGlobal impl =
    { init =
        \pid ->
            impl.init pid
                |> wrapTriple toSelf toGlobal pid
                |> Tuple.mapFirst toProcess
    , update = wrapRecv toProcess toSelf fromGlobal toGlobal impl.update
    , view =
        \s pid ->
            impl.view
                (toSelf
                    >> System.msgTo
                    >> System.sendToPID pid
                )
                s
    , kill = wrapKill toGlobal impl.kill
    , subs = wrapSub toSelf impl
    }


wrapKill toGlobal impl model pid =
    impl model
        |> List.map (toGlobal pid)
        |> System.batch


fromUI :
    (model -> process)
    -> (msgIn -> msgTo)
    -> (System.Msg component msgTo -> Maybe msgIn)
    -> (PID -> msgOut -> System.Msg component msgTo)
    -> Component.UI model msgIn msgOut
    -> Actor model process (System.Msg component msgTo)
fromUI toProcess toSelf fromGlobal toGlobal impl =
    { init =
        \pid ->
            impl.init pid
                |> wrapTriple toSelf toGlobal pid
                |> Tuple.mapFirst toProcess
    , update = wrapRecv toProcess toSelf fromGlobal toGlobal impl.update
    , view = \s pid _ -> Html.lazy4 wrapView impl.view s toSelf pid
    , kill = wrapKill toGlobal impl.kill
    , subs = wrapSub toSelf impl
    }


wrapView : (model -> Html msgIn) -> model -> (msgIn -> msgTo) -> PID -> Html (System.Msg actor msgTo)
wrapView view model toSelf pid =
    view model
        |> Html.map
            (toSelf
                >> System.msgTo
                >> System.sendToPID pid
            )


fromService :
    (model -> process)
    -> (msgIn -> msgTo)
    -> (System.Msg component msgTo -> Maybe msgIn)
    -> (PID -> msgOut -> System.Msg component msgTo)
    -> Component.Service model msgIn msgOut
    -> Actor model process (System.Msg component msgTo)
fromService toProcess toSelf fromGlobal toGlobal impl =
    { init =
        \pid ->
            impl.init pid
                |> wrapTriple toSelf toGlobal pid
                |> Tuple.mapFirst toProcess
    , update = wrapRecv toProcess toSelf fromGlobal toGlobal impl.update
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
            (toSelf
                >> System.msgTo
                >> System.sendToPID pid
            )
            sub


wrapTriple :
    (msgIn -> msgTo)
    -> (PID -> msgOut -> System.Msg component msgTo)
    -> PID
    -> ( model, List msgOut, Cmd msgIn )
    -> ( model, System.Msg component msgTo )
wrapTriple toSelf toGlobal pid ( model, msgsOut, cmd ) =
    let
        msgCmd =
            if cmd == Cmd.none then
                System.none

            else
                Cmd.map
                    (toSelf
                        >> System.msgTo
                        >> System.sendToPID pid
                    )
                    cmd
                    |> Control.Cmd
                    |> System.Ctrl

        msg =
            List.map (toGlobal pid) msgsOut
                |> (::) msgCmd
                |> System.batch
    in
    ( model
    , msg
    )


wrapRecv :
    (model -> process)
    -> (msgIn -> msg)
    -> (System.Msg component msg -> Maybe msgIn)
    -> (PID -> msgOut -> System.Msg component msg)
    -> (msgIn -> model -> ( model, List msgOut, Cmd msgIn ))
    -> (model -> System.Msg component msg -> PID -> ( process, System.Msg component msg ))
wrapRecv toProcess toSelf fromGlobal toGlobal update model msg pid =
    case fromGlobal msg of
        Just msgIn ->
            update msgIn model
                |> wrapTriple toSelf toGlobal pid
                |> Tuple.mapFirst toProcess

        Nothing ->
            ( toProcess model, System.none )
