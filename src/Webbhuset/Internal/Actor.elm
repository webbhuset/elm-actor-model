module Webbhuset.Internal.Actor exposing (..)

import Webbhuset.Internal.Msg as Msg
import Webbhuset.Internal.PID as PID exposing (PID)
import Webbhuset.Component.SystemEvent as SystemEvent exposing (SystemEvent)


type alias Args name compModel appModel msgIn msgOut appMsg =
    { wrapModel : compModel -> appModel
    , wrapMsg : msgIn -> appMsg
    , mapIn : appMsg -> Maybe msgIn
    , mapOut : PID -> msgOut -> SysMsg name appMsg
    }


type alias SysMsg name appMsg =
    Msg.Msg name appMsg


wrapSystem : (msgIn -> appMsg)
    -> (SystemEvent -> SystemEvent.Handling msgIn)
    -> SystemEvent
    -> PID
    -> SystemEvent.Handling (SysMsg name appMsg)
wrapSystem toSelf onSystem event pid =
    onSystem event
        |> SystemEvent.mapHandling
            (toSelf
                >> Msg.AppMsg
                >> Msg.SendToPID pid
                >> Msg.Ctrl
            )


wrapSub :
    (msgIn -> appMsg)
    -> (compModel -> Sub msgIn)
    -> compModel
    -> PID
    -> Sub (SysMsg name appMsg)
wrapSub toSelf subs model pid =
    let
        sub =
            subs model
    in
    if sub == Sub.none then
        Sub.none

    else
        Sub.map
            (toSelf
                >> Msg.AppMsg
                >> Msg.SendToPID pid
                >> Msg.Ctrl
            )
            sub


wrapInit : Args name compModel appModel msgIn msgOut appMsg
    -> (PID -> ( compModel, List msgOut, Cmd msgIn ))
    -> PID
    -> ( appModel, SysMsg name appMsg )
wrapInit args implInit pid =
    implInit pid
        |> wrapTriple args pid
        |> Tuple.mapFirst args.wrapModel


wrapTriple : Args name compModel appModel msgIn msgOut appMsg
    -> PID
    -> ( compModel, List msgOut, Cmd msgIn )
    -> ( compModel, SysMsg name appMsg )
wrapTriple args pid ( model, msgsOut, cmd ) =
    let
        msgCmd =
            if cmd == Cmd.none then
                Msg.None

            else
                Cmd.map
                    (args.wrapMsg
                        >> Msg.AppMsg
                        >> Msg.SendToPID pid
                        >> Msg.Ctrl
                    )
                    cmd
                    |> Msg.Cmd
                    |> Msg.Ctrl

        msg =
            List.map (args.mapOut pid) msgsOut
                |> (::) msgCmd
                |> Msg.Batch
                |> Msg.Ctrl
                |> Msg.Context pid
    in
    ( model
    , msg
    )


wrapUpdate : Args name compModel appModel msgIn msgOut msg
    -> (msgIn -> compModel -> ( compModel, List msgOut, Cmd msgIn ))
    -> (compModel -> SysMsg name msg -> PID -> ( appModel, SysMsg name msg ))
wrapUpdate args update model msg pid =
    case msg of
        Msg.AppMsg appMsg ->
            case args.mapIn appMsg of
                Just msgIn ->
                    update msgIn model
                        |> wrapTriple args pid
                        |> Tuple.mapFirst args.wrapModel

                Nothing ->
                    ( args.wrapModel model, Msg.UnmappedMsg appMsg )

        _ ->
            ( args.wrapModel model, Msg.None )
