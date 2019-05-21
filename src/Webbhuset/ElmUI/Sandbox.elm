module Webbhuset.ElmUI.Sandbox exposing
    ( SandboxProgram
    , TestCase
    , ui
    , layout
    , service
    , sendMsg
    , delay
    , spawnChild
    )


import Html exposing (Html)
import Element exposing (Element)
import Webbhuset.ElmUI.Component as Component
import Webbhuset.Component.Sandbox as Sandbox exposing (Msg, Action)
import Webbhuset.ActorSystem as System
import Webbhuset.PID exposing (PID)


{-| Sandbox Program

-}
type alias SandboxProgram model msgIn msgOut =
    Sandbox.SandboxProgram model msgIn msgOut


{-| Test Case

-}
type alias TestCase msgIn msgOut =
    Sandbox.TestCase msgIn msgOut


{-| Layout Component

-}
ui :
    { title : String
    , component : Component.UI model msgIn msgOut
    , cases : List (TestCase msgIn msgOut)
    , stringifyMsgIn : msgIn -> String
    , stringifyMsgOut : msgOut -> String
    , wrapView : Element msgIn -> Html msgIn
    }
    -> SandboxProgram model msgIn msgOut
ui =
    Sandbox.ui


{-| Layout Component

-}
layout :
    { title : String
    , component : Component.Layout model msgIn msgOut (Msg msgIn msgOut)
    , cases : List (TestCase msgIn msgOut)
    , stringifyMsgIn : msgIn -> String
    , stringifyMsgOut : msgOut -> String
    , wrapView : (msgIn -> Msg msgIn msgOut) -> Element (Msg msgIn msgOut) -> Html (Msg msgIn msgOut)
    }
    -> SandboxProgram model msgIn msgOut
layout ({ component } as args) =
    Sandbox.layout
        { title = args.title
        , component =
            { init = component.init
            , update = component.update
            , onSystem = component.onSystem
            , subs = component.subs
            , view = \toSelf model renderPID ->
                (component.view
                    toSelf
                    model
                    (Element.paragraph [] << List.singleton << Element.html << renderPID)
                )
                    |> args.wrapView toSelf
            }
        , cases = args.cases
        , stringifyMsgIn = args.stringifyMsgIn
        , stringifyMsgOut = args.stringifyMsgOut
        , wrapView = \_ o -> o
        }


{-| Service Component

-}
service :
    { title : String
    , component : Component.Service model msgIn msgOut
    , cases : List (TestCase msgIn msgOut)
    , view : model -> Html msgIn
    , stringifyMsgIn : msgIn -> String
    , stringifyMsgOut : msgOut -> String
    }
    -> SandboxProgram model msgIn msgOut
service =
    Sandbox.service


{-| Spawn child

-}
spawnChild : String -> (PID -> msgIn) -> Action msgIn
spawnChild =
    Sandbox.spawnChild



{-| Delay

-}
delay : Float -> Action msgIn -> Action msgIn
delay =
    Sandbox.delay


{-| Send a message

-}
sendMsg : msgIn -> Action msgIn
sendMsg =
    Sandbox.sendMsg
