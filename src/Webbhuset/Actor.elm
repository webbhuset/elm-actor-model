module Webbhuset.Actor exposing
    ( Actor
    , PID
    , applyModel
    , fromLayout
    , fromService
    , fromUI
    )

{-|

@docs PID

## Create Actors from Components

@docs fromUI, fromService, fromLayout

## Bootstrap

@docs Actor, applyModel

-}
import Browser
import Html exposing (Html)
import Html.Lazy as Html
import Webbhuset.ActorSystem as System exposing (SysMsg)
import Webbhuset.Component as Component
import Webbhuset.Internal.Control as Control exposing (Control(..))
import Webbhuset.Internal.PID as PID

{-| A PID is an identifier for a Process.

-}
type alias PID =
    PID.PID


{-| An actor is acomponent where the types are wrapped
to fit the System types.

-}
type alias Actor compModel appModel msg =
    { init : PID -> ( appModel, msg )
    , update : compModel -> msg -> PID -> ( appModel, msg )
    , view : compModel -> PID -> (PID -> Html msg) -> Html msg
    , kill : compModel -> PID -> msg
    , subs : compModel -> PID -> Sub msg
    }


{-| Apply the compModel to an actor.

-}
applyModel : Actor compModel appModel msg -> compModel -> System.AppliedActor appModel msg
applyModel actor model =
    { init = actor.init
    , update = actor.update model
    , view = actor.view model
    , kill = actor.kill model
    , subs = actor.subs model
    }


{-| Create an actor from a Layout Component

-}
fromLayout :
    { wrapModel : compModel -> appModel
    , wrapMsg : msgIn -> appMsg
    , fromApp : SysMsg name appMsg -> Maybe msgIn
    , toApp : PID -> msgOut -> SysMsg name appMsg
    }
    -> Component.Layout compModel msgIn msgOut (SysMsg name appMsg)
    -> Actor compModel appModel (SysMsg name appMsg)
fromLayout args component =
    { init =
        \pid ->
            component.init pid
                |> wrapTriple args.wrapMsg args.toApp pid
                |> Tuple.mapFirst args.wrapModel
    , update = wrapRecv args.wrapModel args.wrapMsg args.fromApp args.toApp component.update
    , view =
        \s pid ->
            component.view
                (args.wrapMsg
                    >> System.toAppMsg
                    >> System.sendToPID pid
                )
                s
    , kill = wrapKill args.toApp component.kill
    , subs = wrapSub args.wrapMsg component
    }


wrapKill toGlobal impl model pid =
    impl model
        |> List.map (toGlobal pid)
        |> System.batch


{-| Create an actor from a UI Component


-}
fromUI :
    { wrapModel : compModel -> appModel
    , wrapMsg : msgIn -> appMsg
    , fromApp : SysMsg name appMsg -> Maybe msgIn
    , toApp : PID -> msgOut -> SysMsg name appMsg
    }
    -> Component.UI compModel msgIn msgOut
    -> Actor compModel appModel (SysMsg name appMsg)
fromUI args component =
    { init =
        \pid ->
            component.init pid
                |> wrapTriple args.wrapMsg args.toApp pid
                |> Tuple.mapFirst args.wrapModel
    , update = wrapRecv args.wrapModel args.wrapMsg args.fromApp args.toApp component.update
    , view = \s pid _ -> Html.lazy4 wrapView component.view s args.wrapMsg pid
    , kill = wrapKill args.toApp component.kill
    , subs = wrapSub args.wrapMsg component
    }


wrapView : (compModel -> Html msgIn) -> compModel -> (msgIn -> appMsg) -> PID -> Html (SysMsg actor appMsg)
wrapView view model toSelf pid =
    view model
        |> Html.map
            (toSelf
                >> System.toAppMsg
                >> System.sendToPID pid
            )


{-| Create an actor from a Service Component

-}
fromService :
    { wrapModel : compModel -> appModel
    , wrapMsg : msgIn -> appMsg
    , fromApp : SysMsg name appMsg -> Maybe msgIn
    , toApp : PID -> msgOut -> SysMsg name appMsg
    }
    -> Component.Service compModel msgIn msgOut
    -> Actor compModel appModel (SysMsg name appMsg)
fromService args impl =
    { init =
        \pid ->
            impl.init pid
                |> wrapTriple args.wrapMsg args.toApp pid
                |> Tuple.mapFirst args.wrapModel
    , update = wrapRecv args.wrapModel args.wrapMsg args.fromApp args.toApp impl.update
    , view = \_ _ _ -> Html.text ""
    , kill = wrapKill args.toApp impl.kill
    , subs = wrapSub args.wrapMsg impl
    }


wrapSub :
    (msgIn -> appMsg)
    -> { a | subs : compModel -> Sub msgIn }
    -> compModel
    -> PID
    -> Sub (SysMsg name appMsg)
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
                >> System.toAppMsg
                >> System.sendToPID pid
            )
            sub


wrapTriple :
    (msgIn -> appMsg)
    -> (PID -> msgOut -> SysMsg name appMsg)
    -> PID
    -> ( compModel, List msgOut, Cmd msgIn )
    -> ( compModel, SysMsg name appMsg )
wrapTriple toSelf toGlobal pid ( model, msgsOut, cmd ) =
    let
        msgCmd =
            if cmd == Cmd.none then
                System.none

            else
                Cmd.map
                    (toSelf
                        >> System.toAppMsg
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
    (compModel -> appModel)
    -> (msgIn -> msg)
    -> (SysMsg name msg -> Maybe msgIn)
    -> (PID -> msgOut -> SysMsg name msg)
    -> (msgIn -> compModel -> ( compModel, List msgOut, Cmd msgIn ))
    -> (compModel -> SysMsg name msg -> PID -> ( appModel, SysMsg name msg ))
wrapRecv toProcess toSelf fromGlobal toGlobal update model msg pid =
    case fromGlobal msg of
        Just msgIn ->
            update msgIn model
                |> wrapTriple toSelf toGlobal pid
                |> Tuple.mapFirst toProcess

        Nothing ->
            ( toProcess model, System.none )
