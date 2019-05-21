module Webbhuset.ElmUI.Actor exposing
    ( Actor
    , PID
    , fromLayout
    , fromService
    , fromUI
    )

{-|

# Actor for Elm UI

This module has an identical API to Webbhuset.Actor
but with the view functions returning `Element msg`.

Check Webbhuset.Actor for more info.

@docs PID

## Create Actors from Components

@docs fromUI, fromService, fromLayout

## Bootstrap

@docs Actor

-}
import Element exposing (Element)
import Element.Lazy as Element
import Webbhuset.ElmUI.Component as Component
import Webbhuset.PID as PID
import Webbhuset.ActorSystem as System
import Webbhuset.Actor exposing (Args, wrapSystem, wrapSub, wrapInit, wrapUpdate, sendTo)

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
    System.Actor compModel appModel (Element msg) msg


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
        -> (PID -> Element (SysMsg name appMsg))
        -> Element (SysMsg name appMsg)
        )
    -> compModel
    -> PID
    -> (PID -> Maybe (Element (SysMsg name appMsg)))
    -> Element (SysMsg name appMsg)
layoutView args view model pid renderPID =
    view
        (sendTo args.wrapMsg pid)
        model
        (renderPID >> Maybe.withDefault (Element.none))



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
    -> (compModel -> Element msgIn)
    -> compModel
    -> PID
    -> renderPID
    -> Element (SysMsg name appMsg)
uiView args view model pid _ =
    Element.lazy4 uiView_ view model args.wrapMsg pid


uiView_ : (compModel -> Element msgIn) -> compModel -> (msgIn -> appMsg) -> PID -> Element (SysMsg actor appMsg)
uiView_ view model toSelf pid =
    view model
        |> Element.map (sendTo toSelf pid)


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


serviceView : a -> b -> c -> Element msg
serviceView _ _ _ =
    Element.none


