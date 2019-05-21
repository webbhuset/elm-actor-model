module Webbhuset.Component.Sandbox exposing
    ( SandboxProgram
    , TestCase
    , Action
    , Msg
    , ui
    , layout
    , service
    , sendMsg
    , delay
    , spawnChild
    )

{-|

# Sandbox

The sandbox module is helpful when developing components. It lets you
run the component using `elm reactor` outside the system and define several test cases.

@docs SandboxProgram


# Create Sandbox

Wrap a component in a sandbox application.

This will render each test case and log all messages.
Create a test file with a `main` function where you declare all
test cases.

A sandbox module example for `YourComponent`:

    import YourComponent exposing (Model, MsgIn, MsgOut)

    main : SandboxProgram Model MsgIn MsgOut
    main =
        Sandbox.ui
            { title = "Title of your component"
            , component = YourComponent.component
            , cases =
                [ testCase1
                , testCase2
                ]
            , stringifyMsgIn = Debug.toString -- Or roll your own if you want prettier messages.
            , stringifyMsgOut = Debug.toString
            , wrapView = identity
            }


@docs ui, layout, service

# Create a Test Case

A Test Case is just a record with a title and description together
with a list of Actions you want to perform on your sandboxed component.
You can also the component's map out messages to actions to simulate the outside system.

@docs TestCase

    testCase1 : Sandbox.TestCase MsgIn MsgOut
    testCase1 =
        { title = "Test Case Title"
        , desc = "Test Case Description"
        , init =
            [ Sandbox.sendMsg YourComponent.Hello
            , Sandbox.spawnChild "Child Title" YourComponent.ReceiveChildPID
            ]
        , onMsgOut = \msgOut ->
            case msgOut of
                YourComponent.ObserveSomething id ->
                    [ YourComponent.RecevieDataFor id "Some data"
                        |> Sandbox.sendMsg
                        |> Sandbox.delay 1000
                    ]

## Actions

@docs Action, sendMsg, spawnChild, delay

@docs Msg

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Webbhuset.Actor as Actor exposing (Actor)
import Webbhuset.ActorSystem as System
import Webbhuset.Component as Component
import Webbhuset.Component.SystemEvent as SystemEvent exposing (SystemEvent)
import Webbhuset.Component.Sandbox.Navigation as Navigation
import Webbhuset.Component.Sandbox.LoremIpsum as LoremIpsum
import Webbhuset.Component.Sandbox.Layout as Layout
import Webbhuset.Internal.PID exposing (PID(..))
import Webbhuset.PID as PID
import Browser.Navigation as Nav
import Browser
import Markdown
import Url exposing (Url)
import List.Extra as List


{-| The Application model

-}
type alias Model m =
    System.Model ActorName (Process m)


{-| The Program type of your main function.

-}
type alias SandboxProgram model msgIn msgOut =
    Program () (Model model) (Msg msgIn msgOut)


{-| A test case for the Component

-}
type alias TestCase msgIn msgOut =
    { title : String
    , desc : String
    , init : List (Action msgIn)
    , onMsgOut : msgOut -> List (Action msgIn)
    }


{-| An action to perform on your sandboxed component.

-}
type alias Action msgIn =
    Layout.Action msgIn

{-| Spawn a child component and send the PID to your component.

You can provide a String which will be displayed when the child
component is rendered (using `renderPID` in your layout component).


    Sandbox.spawnChild "Hello child" YourComponent.ReceiveChild

-}
spawnChild : String -> (PID -> msgIn) -> Action msgIn
spawnChild =
    Layout.SpawnChild



{-| Perform a delayed action on your sandboxed component. Delay in
milliseconds.

    Sandbox.sendMsg YourComponent.SomeMessage
        |> Sandbox.delay 1000

-}
delay : Float -> Action msgIn -> Action msgIn
delay =
    Layout.Delay


{-| Send a message to you sandboxed component

    Sandbox.sendMsg YourComponent.SomeMessage

-}
sendMsg : msgIn -> Action msgIn
sendMsg =
    Layout.SendMsg


{-| Sandbox a UI Component

-}
ui :
    { title : String
    , component : Component.UI model msgIn msgOut
    , cases : List (TestCase msgIn msgOut)
    , stringifyMsgIn : msgIn -> String
    , stringifyMsgOut : msgOut -> String
    , wrapView : Html msgIn -> Html msgIn
    }
    -> SandboxProgram model msgIn msgOut
ui ({ component } as args) =
    Actor.fromUI
        { wrapModel = P_Component
        , wrapMsg = ComponentMsg
        , mapIn = testedMapIn
        , mapOut = testedMapOut args.stringifyMsgOut
        }
        { init = component.init
        , update = component.update
        , onSystem = component.onSystem
        , subs = component.subs
        , view = component.view >> args.wrapView
        }
        |> toApplication
            { title = args.title
            , cases = args.cases
            , stringifyMsgIn = args.stringifyMsgIn
            , stringifyMsgOut = args.stringifyMsgOut
            }


{-| Sandbox a Layout Component

-}
layout :
    { title : String
    , component : Component.Layout model msgIn msgOut (Msg msgIn msgOut)
    , cases : List (TestCase msgIn msgOut)
    , stringifyMsgIn : msgIn -> String
    , stringifyMsgOut : msgOut -> String
    , wrapView : (msgIn -> Msg msgIn msgOut) -> Html (Msg msgIn msgOut) -> Html (Msg msgIn msgOut)
    }
    -> SandboxProgram model msgIn msgOut
layout ({ component } as args) =
    Actor.fromLayout
        { wrapModel = P_Component
        , wrapMsg = ComponentMsg
        , mapIn = testedMapIn
        , mapOut = testedMapOut args.stringifyMsgOut
        }
        { component
            | view = \toSelf model renderPID ->
                (component.view toSelf model renderPID)
                    |> args.wrapView toSelf
        }
        |> toApplication
            { title = args.title
            , cases = args.cases
            , stringifyMsgIn = args.stringifyMsgIn
            , stringifyMsgOut = args.stringifyMsgOut
            }


{-| Sandbox a Service Component

You need to provied a `view` function which renders the model of
your service component.

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
service args =
    Actor.fromUI
        { wrapModel = P_Component
        , wrapMsg = ComponentMsg
        , mapIn = testedMapIn
        , mapOut = testedMapOut args.stringifyMsgOut
        }
        { init = args.component.init
        , update = args.component.update
        , onSystem = args.component.onSystem
        , subs = args.component.subs
        , view = args.view
        }
        |> toApplication
            { title = args.title
            , cases = args.cases
            , stringifyMsgIn = args.stringifyMsgIn
            , stringifyMsgOut = args.stringifyMsgOut
            }



toApplication : Args i o
    -> Actor model (Process model) (Msg i o)
    -> SandboxProgram model i o
toApplication args testedActor =
    System.application
        { init = initApp
        , spawn = spawn args testedActor
        , apply = applyModel args testedActor
        , view = Html.div []
        , onUrlRequest =
            \urlRequest ->
                case urlRequest of
                    Browser.Internal url ->
                        Url.toString url
                            |> Navigation.Push
                            |> NavMsg
                            |> System.toAppMsg
                            |> System.sendToSingleton Navigation

                    Browser.External str ->
                        str
                            |> Navigation.Load
                            |> NavMsg
                            |> System.toAppMsg
                            |> System.sendToSingleton Navigation
        , onUrlChange =
            Layout.UrlChanged
                >> LayoutMsg
                >> System.toAppMsg
                >> System.sendToSingleton LayoutActor
        }


initApp : () -> Url -> Nav.Key -> Msg msgIn msgOut
initApp _ url key =
    System.batch
        [ System.withSingletonPID LayoutActor System.addView
        , Navigation.Init key url
            |> NavMsg
            |> System.toAppMsg
            |> System.sendToSingleton Navigation
        , Layout.UrlChanged url
            |> LayoutMsg
            |> System.toAppMsg
            |> System.sendToSingleton LayoutActor
        ]


testedMapIn : AppMsg i o -> Maybe i
testedMapIn appMsg =
    case appMsg of
        ComponentMsg msg ->
            Just msg

        _ ->
            Nothing


testedMapOut : (msgOut -> String) -> PID -> msgOut -> Msg msgIn msgOut
testedMapOut toString pid componentMsg =
    componentMsg
        |> Layout.HandleMsgOut pid
        |> LayoutMsg
        |> System.toAppMsg
        |> System.sendToSingleton LayoutActor



-- SYSTEM


type ActorName
    = LayoutActor
    | TestedActor
    | Navigation
    | LoremIpsum


type Process model
    = P_Layout Layout.Model
    | P_Component model
    | P_Nav Navigation.Model
    | P_LoremIpsum LoremIpsum.Model

{-| Sadbox Msg
-}
type alias Msg msgIn msgOut =
    System.SysMsg ActorName (AppMsg msgIn msgOut)


type AppMsg msgIn msgOut
    = LayoutMsg (Layout.MsgIn msgIn msgOut)
    | ComponentMsg msgIn
    | NavMsg Navigation.MsgIn
    | LoremIpsumMsg LoremIpsum.MsgIn


spawn : Args i o
    -> Actor model (Process model) (Msg i o)
    -> ActorName
    -> PID
    -> ( Process model, Msg i o )
spawn args tested name =
    case name of
        LayoutActor ->
            .init (layoutActor args)

        TestedActor ->
            tested.init

        Navigation ->
            navigation.init

        LoremIpsum ->
            loremIpsum.init


applyModel :
    Args i o
    -> Actor model (Process model) (Msg i o)
    -> Process model
    -> System.AppliedActor (Process model) (Html (Msg i o)) (Msg i o)
applyModel args testedActor process =
    case process of
        P_Layout model ->
            System.applyModel (layoutActor args) model

        P_Component model ->
            System.applyModel testedActor model

        P_Nav model ->
            System.applyModel navigation model

        P_LoremIpsum model ->
            System.applyModel loremIpsum model

-- Lorem Ipsum

loremIpsum : Actor LoremIpsum.Model (Process model) (Msg msgIn msgOut)
loremIpsum =
    Actor.fromUI
        { wrapModel = P_LoremIpsum
        , wrapMsg = LoremIpsumMsg
        , mapIn = loremIpsumMapIn
        , mapOut = \_ _ -> System.none
        }
        LoremIpsum.component


loremIpsumMapIn : AppMsg i o -> Maybe LoremIpsum.MsgIn
loremIpsumMapIn appMsg =
    case appMsg of
        LoremIpsumMsg msg ->
            Just msg

        _ ->
            Nothing

-- Navigation

navigation : Actor Navigation.Model (Process model) (Msg msgIn msgOut)
navigation =
    Actor.fromService
        { wrapModel = P_Nav
        , wrapMsg = NavMsg
        , mapIn = navigationMapIn
        , mapOut = navigationMapOut
        }
        Navigation.component


navigationMapIn : AppMsg i o -> Maybe Navigation.MsgIn
navigationMapIn appMsg =
    case appMsg of
        NavMsg msg ->
            Just msg

        _ ->
            Nothing


navigationMapOut : PID -> Navigation.MsgOut -> Msg msgIn msgOut
navigationMapOut self componentMsg =
    case componentMsg of
        Navigation.NoOut ->
            System.none

-- Test Runner Actor

type alias Args i o =
    { title : String
    , cases : List (TestCase i o)
    , stringifyMsgIn : i -> String
    , stringifyMsgOut : o -> String
    }


layoutActor : Args i o -> Actor Layout.Model (Process model) (Msg i o)
layoutActor args =
    Actor.fromLayout
        { wrapModel = P_Layout
        , wrapMsg = LayoutMsg
        , mapIn = layoutMapIn
        , mapOut = layoutMapOut args.stringifyMsgIn
        }
        (Layout.component
            { cases =
                List.indexedMap Tuple.pair args.cases
                    |> Dict.fromList
            , stringifyMsgOut = args.stringifyMsgOut
            , title = args.title
            }
        )


layoutMapIn : AppMsg i o -> Maybe (Layout.MsgIn i o)
layoutMapIn appMsg =
    case appMsg of
        LayoutMsg msg ->
            Just msg

        _ ->
            Nothing


layoutMapOut : (msgIn -> String) -> PID -> Layout.MsgOut msgIn msgOut -> Msg msgIn msgOut
layoutMapOut toString p componentMsg =
    case componentMsg of
        Layout.Spawn replyPID reply ->
             reply
                >> LayoutMsg
                >> System.toAppMsg
                >> System.sendToPID replyPID
                |> System.spawn TestedActor

        Layout.SetPageTitle title ->
            System.setDocumentTitle title

        Layout.PerformAction subject action ->
            case action of
                Layout.SendMsg msg ->
                    System.batch
                        [ toString msg
                            |> Layout.InMessage
                            |> Layout.LogMsg subject
                            |> LayoutMsg
                            |> System.toAppMsg
                            |> System.sendToSingleton LayoutActor
                        , ComponentMsg msg
                            |> System.toAppMsg
                            |> System.sendToPID subject
                        ]

                Layout.Delay _ _ ->
                    System.none -- handled in sandbox component

                Layout.SpawnChild title reply ->
                    System.spawn LoremIpsum
                        (\newPid ->
                            System.batch
                                [ reply newPid
                                    |> ComponentMsg
                                    |> System.toAppMsg
                                    |> System.sendToPID subject
                                , LoremIpsum.SetText title
                                    |> LoremIpsumMsg
                                    |> System.toAppMsg
                                    |> System.sendToPID newPid
                                , reply newPid
                                    |> toString
                                    |> Layout.InMessage
                                    |> Layout.LogMsg subject
                                    |> LayoutMsg
                                    |> System.toAppMsg
                                    |> System.sendToSingleton LayoutActor
                                ]
                        )

        Layout.NavigateToHref href ->
            Navigation.Push href
                |> NavMsg
                |> System.toAppMsg
                |> System.sendToSingleton Navigation


