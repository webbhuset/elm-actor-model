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

    -- This is the global model for the System.
    module AppModel exposing (..)

    type AppModel
        = LoginFormModel LoginForm.Model
        | OtherComponet ...

    -- This is the global appMsg type for the System.
    module AppMsg exposing (..)

    type AppMsg
        = FormMsg LoginForm.MsgIn
        | AuthServiceMsg AuthService.MsgIn
        | OtherComponent ...

    -- in Actor.LoginForm

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
import Webbhuset.Internal.Msg as Msg exposing (Control(..))
import Webbhuset.Internal.PID as PID

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
    { init : PID -> ( appModel, msg )
    , update : compModel -> msg -> PID -> ( appModel, msg )
    , view : compModel -> PID -> (PID -> Html msg) -> Html msg
    , kill : compModel -> PID -> msg
    , subs : compModel -> PID -> Sub msg
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
    { init =
        \pid ->
            component.init pid
                |> wrapTriple args.wrapMsg args.mapOut pid
                |> Tuple.mapFirst args.wrapModel
    , update = wrapRecv args.wrapModel args.wrapMsg args.mapIn args.mapOut component.update
    , view =
        \model pid ->
            component.view
                (args.wrapMsg
                    >> Msg.AppMsg
                    >> Msg.SendToPID pid
                    >> Msg.Ctrl
                )
                model
    , kill = wrapKill args.mapOut component.kill
    , subs = wrapSub args.wrapMsg component
    }


wrapKill toGlobal impl model pid =
    impl model
        |> List.map (toGlobal pid)
        |> Msg.Batch
        |> Msg.Ctrl


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
    { init =
        \pid ->
            component.init pid
                |> wrapTriple args.wrapMsg args.mapOut pid
                |> Tuple.mapFirst args.wrapModel
    , update = wrapRecv args.wrapModel args.wrapMsg args.mapIn args.mapOut component.update
    , view = \model pid _ -> Html.lazy4 wrapView component.view model args.wrapMsg pid
    , kill = wrapKill args.mapOut component.kill
    , subs = wrapSub args.wrapMsg component
    }


wrapView : (compModel -> Html msgIn) -> compModel -> (msgIn -> appMsg) -> PID -> Html (SysMsg actor appMsg)
wrapView view model toSelf pid =
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
fromService args impl =
    { init =
        \pid ->
            impl.init pid
                |> wrapTriple args.wrapMsg args.mapOut pid
                |> Tuple.mapFirst args.wrapModel
    , update = wrapRecv args.wrapModel args.wrapMsg args.mapIn args.mapOut impl.update
    , view = \_ _ _ -> Html.text ""
    , kill = wrapKill args.mapOut impl.kill
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
                >> Msg.AppMsg
                >> Msg.SendToPID pid
                >> Msg.Ctrl
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
                Msg.None

            else
                Cmd.map
                    (toSelf
                        >> Msg.AppMsg
                        >> Msg.SendToPID pid
                        >> Msg.Ctrl
                    )
                    cmd
                    |> Msg.Cmd
                    |> Msg.Ctrl

        msg =
            List.map (toGlobal pid) msgsOut
                |> (::) msgCmd
                |> Msg.Batch
                |> Msg.Ctrl
    in
    ( model
    , msg
    )


wrapRecv :
    (compModel -> appModel)
    -> (msgIn -> msg)
    -> (msg -> Maybe msgIn)
    -> (PID -> msgOut -> SysMsg name msg)
    -> (msgIn -> compModel -> ( compModel, List msgOut, Cmd msgIn ))
    -> (compModel -> SysMsg name msg -> PID -> ( appModel, SysMsg name msg ))
wrapRecv toProcess toSelf fromGlobal toGlobal update model msg pid =
    case msg of
        Msg.AppMsg appMsg ->
            case fromGlobal appMsg of
                Just msgIn ->
                    update msgIn model
                        |> wrapTriple toSelf toGlobal pid
                        |> Tuple.mapFirst toProcess

                Nothing ->
                    ( toProcess model, Msg.UnmappedMsg appMsg )

        _ ->
            ( toProcess model, Msg.None )
