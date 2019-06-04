module Webbhuset.Actor exposing
    ( Actor
    , PID
    , Args
    , fromLayout
    , fromService
    , fromUI
    , wrapSystem
    , wrapSub
    , wrapInit
    , wrapUpdate
    , sendTo
    )

{-|

# Actor

When a component is incorporated in a system it becomes an Actor. The actor module
implements the connections to the other components in the system.
In practice that means mapping and sending the component's out messages to other
actors in the system.

Here is an example of wrapping a login form component to an actor in a system.


This is the global model for the System:
```
module AppModel exposing (..)

type AppModel
    = LoginFormModel LoginForm.Model
    | OtherComponent ...
```

This is the global appMsg type for the System:
```
module AppMsg exposing (..)

type AppMsg
    = FormMsg LoginForm.MsgIn
    | AuthServiceMsg AuthService.MsgIn
    | OtherComponent ...

```

This is the login form actor:
```
module Actor.LoginForm exposing (..)

import Webbhuset.ActorSystem as System
import Webbhuset.Actor as Actor exposing (Actor)
import Component.LoginForm as LoginForm
import Component.AuthService as AuthService
import AppMsg exposing (AppMsg)
import AppModel exposing (AppModel)


actor : Actor LoginForm.Model AppModel AppMsg
actor =
    Actor.fromUI
        { wrapModel = AppModel.LoginFormModel
        , wrapMsg = AppMsg.FormMsg
        , mapIn = mapFormIn
        , mapOut = mapFormOut
        }
        LoginForm.component


mapFormIn : AppMsg -> Maybe LoginForm.MsgIn
mapFormIn appMsg =
    case appMsg of
        AppMsg.FormMsg formMsg ->
            Just formMsg

        _ ->
            Nothing


mapFormOut : PID -> LoginForm.MsgOut -> System.SysMsg name AppMsg
mapFormOut self formMsg =
    case formMsg of
        LoginForm.Submit user password ->
            AuthService.Login user password self
                |> AppMsg.AuthServiceMsg
                |> System.toAppMsg
                |> System.sendToSingleton AuthService

```

@docs PID

## Create Actors from Components

@docs fromUI, fromService, fromLayout

## For package authors.

You probably don't need this when you are using the actor model.
These are useful if you need to create support for a different output type.

@docs Actor
    , Args
    , wrapSystem
    , wrapSub
    , wrapInit
    , wrapUpdate
    , sendTo

-}
import Html exposing (Html)
import Html.Lazy as Html
import Webbhuset.Component as Component
import Webbhuset.PID as PID
import Webbhuset.ActorSystem as System
import Webbhuset.Component.SystemEvent as SystemEvent exposing (SystemEvent)
import Webbhuset.Internal.Msg as Msg

type alias SysMsg name appMsg =
    System.SysMsg name appMsg

{-| A PID is an identifier for a Process.

-}
type alias PID =
    PID.PID


{-| An actor is acomponent where the types are wrapped
to fit the System types.

-}
type alias Actor compModel appModel msg =
    System.Actor compModel appModel (Html msg) msg


{-| Args

-}
type alias Args name compModel appModel msgIn msgOut appMsg =
    { wrapModel : compModel -> appModel
    , wrapMsg : msgIn -> appMsg
    , mapIn : appMsg -> Maybe msgIn
    , mapOut : PID -> msgOut -> SysMsg name appMsg
    }



{-| Create an actor from a Layout Component

-}
fromLayout :
    { wrapModel : compModel -> appModel
    , wrapMsg : msgIn -> appMsg
    , mapIn : appMsg -> Maybe msgIn
    , mapOut : PID -> msgOut -> SysMsg name appMsg
    }
    -> Component.Layout compModel msgIn msgOut (SysMsg name appMsg)
    -> Actor compModel appModel (SysMsg name appMsg)
fromLayout args component =
    { init = wrapInit args component.init
    , update = wrapUpdate args component.update
    , view = layoutView args component.view
    , onSystem = wrapSystem args.wrapMsg component.onSystem
    , subs = wrapSub args.wrapMsg component.subs
    }


layoutView : Args name compModel appModel msgIn msgOut appMsg
    -> ((msgIn -> SysMsg name appMsg)
        -> compModel
        -> (PID -> Html (SysMsg name appMsg))
        -> Html (SysMsg name appMsg)
        )
    -> compModel
    -> PID
    -> (PID -> Maybe (Html (SysMsg name appMsg)))
    -> Html (SysMsg name appMsg)
layoutView args view model pid renderPID =
    view
        (sendTo args.wrapMsg pid)
        model
        (renderPID >> Maybe.withDefault (Html.text ""))


{-| Create an actor from a UI Component


-}
fromUI :
    { wrapModel : compModel -> appModel
    , wrapMsg : msgIn -> appMsg
    , mapIn : appMsg -> Maybe msgIn
    , mapOut : PID -> msgOut -> SysMsg name appMsg
    }
    -> Component.UI compModel msgIn msgOut
    -> Actor compModel appModel (SysMsg name appMsg)
fromUI args component =
    { init = wrapInit args component.init
    , update = wrapUpdate args component.update
    , view = uiView args component.view
    , onSystem = wrapSystem args.wrapMsg component.onSystem
    , subs = wrapSub args.wrapMsg component.subs
    }


uiView : Args name compModel appModel msgIn msgOut appMsg
    -> (compModel -> Html msgIn)
    -> compModel
    -> PID
    -> renderPID
    -> Html (SysMsg name appMsg)
uiView args view model pid _ =
    Html.lazy4 uiView_ view model args.wrapMsg pid


uiView_ : (compModel -> Html msgIn) -> compModel -> (msgIn -> appMsg) -> PID -> Html (SysMsg actor appMsg)
uiView_ view model toSelf pid =
    view model
        |> Html.map
            (sendTo toSelf pid)


{-| Create an actor from a Service Component

-}
fromService :
    { wrapModel : compModel -> appModel
    , wrapMsg : msgIn -> appMsg
    , mapIn : appMsg -> Maybe msgIn
    , mapOut : PID -> msgOut -> SysMsg name appMsg
    }
    -> Component.Service compModel msgIn msgOut
    -> Actor compModel appModel (SysMsg name appMsg)
fromService args component =
    { init = wrapInit args component.init
    , update = wrapUpdate args component.update
    , view = serviceView
    , onSystem = wrapSystem args.wrapMsg component.onSystem
    , subs = wrapSub args.wrapMsg component.subs
    }


serviceView : a -> b -> c -> Html msg
serviceView _ _ _ =
    Html.text ""


{-| Convert a component `onSystem` field to an actor `onSystem` field
-}
wrapSystem : (msgIn -> appMsg)
    -> (SystemEvent -> SystemEvent.Handling msgIn)
    -> SystemEvent
    -> PID
    -> SystemEvent.Handling (SysMsg name appMsg)
wrapSystem toSelf onSystem event pid =
    onSystem event
        |> SystemEvent.mapHandling (sendTo toSelf pid)


{-| Convert a component `subs` field to an actor `subs` field
-}
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
            (sendTo toSelf pid)
            sub


{-| Convert a component `init` field to an actor `init` field
-}
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
                    (sendTo args.wrapMsg pid)
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


{-| Convert a component `update` field to an actor `update` field
-}
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
                    ( args.wrapModel model, Msg.UnmappedMsg pid appMsg )

        _ ->
            ( args.wrapModel model, Msg.None )

{-| Send to pid.
-}
sendTo : (msgIn -> appMsg) -> PID -> msgIn -> SysMsg name appMsg
sendTo wrapper pid =
    wrapper
        >> Msg.AppMsg
        >> Msg.SendToPID pid
        >> Msg.Ctrl
