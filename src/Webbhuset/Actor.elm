module Webbhuset.Actor exposing
    ( Actor
    , PID
    , fromLayout
    , fromService
    , fromUI
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

## Bootstrap

@docs Actor

-}
import Browser
import Html exposing (Html)
import Html.Lazy as Html
import Webbhuset.Component as Component
import Webbhuset.Internal.Msg as Msg
import Webbhuset.Internal.PID as PID
import Webbhuset.ActorSystem as System
import Webbhuset.Internal.Actor exposing (Args, wrapSystem, wrapSub, wrapInit, wrapUpdate)

type alias SysMsg name appMsg =
    Msg.Msg name appMsg

{-| A PID is an identifier for a Process.

-}
type alias PID =
    PID.PID


{-| An actor is acomponent where the types are wrapped
to fit the System types.

-}
type alias Actor compModel appModel msg =
    System.Actor compModel appModel (Html msg) msg


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
        (args.wrapMsg
            >> Msg.AppMsg
            >> Msg.SendToPID pid
            >> Msg.Ctrl
        )
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
            (toSelf
                >> Msg.AppMsg
                >> Msg.SendToPID pid
                >> Msg.Ctrl
            )


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


