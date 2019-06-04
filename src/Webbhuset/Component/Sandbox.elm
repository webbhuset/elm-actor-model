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
    , pass
    , fail
    , timeout
    , mockPID
    , checkPID
    , assertPID
    )

{-|

# Sandbox

The sandbox module is helpful when developing components. It lets you
run the component using `elm reactor` outside the system and define several test cases.

If you want to run your sandbox on a CI you can add #markdown to the URL. This
will output the test results as a markdown string inside a pre element:

    <pre id="markdown-output"> Results here </pre>

This way you can complile the sandbox, run it in a headless Chrome and dump
the DOM. From the DOM you can extract the results.

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

Test cases defines scenarios for the requirements of your component.

A Test Case is just a record with a title and description together
with a list of Actions you want to perform on your sandboxed component.
You can also map the component's out messages to actions to simulate the outside system.

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

## Assertions

Sometimes it is useful to test your expectations or requirements on a component.

You can express them using assertions. Assertions have three states: waiting, pass
or fail. The state of a test case is visible in the sandbox UI.

In this example we expect that `GoodMsg` is sent by the component within 1s.

    testGoodMsg : Sandbox.TestCase MsgIn MsgOut
    testGoodMsg =
        { title = "Good messages are good"
        , desc = "`GoodMsg` must be sent within 1 second. No other messages are allowed."
        , init =
            [ Sandbox.timeout 1000
            , Sandbox.sendMsg YourComponent.SomeInput
            ]
        , onMsgOut = \msgOut ->
            case msgOut of
                YourComponent.GoodMsg ->
                    [ Sandbox.pass
                    ]

                YourComponent.BadMsg ->
                    [ Sandbox.fail "I don't like bad messages"
                    ]


@docs pass, fail, timeout

## Assert PIDs

@docs mockPID, checkPID, assertPID

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
type alias Model m o =
    System.Model ActorName (Process m o)


{-| The Program type of your main function.

-}
type alias SandboxProgram model msgIn msgOut =
    Program () (Model model msgOut) (Msg msgIn msgOut)


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
type alias Action msgIn
    = Layout.Action msgIn

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


{-| Flag test case as passed.

    Sandbox.pass
-}
pass : Action msgIn
pass =
    Layout.Pass


{-| Flag test case as failed. You can supply a message explaining what
went wrong.

    Sandbox.fail "Didn't receive some important out msg"
-}
fail : String -> Action msgIn
fail reason =
    Layout.Fail reason


{-| Set a timeout in milliseconds. This will cause the test to automatically fail
after the timeout if the test havn't been flagged as passed by then.

    Sandbox.timeout 1000
-}
timeout : Float -> Action msgIn
timeout t =
    "Timeout: " ++ (String.fromFloat t) ++ "ms"
        |> Layout.Timeout
        |> Layout.Delay t


{-| Create a mock PID for testing purposes.

    mockPID "form-component"

-}
mockPID : String -> PID
mockPID label =
    PID
        { isSingleton = False
        , prefix = label
        , key = 0
        , spawnedBy = 0
        }


{-| Check that a mock pid matches an expected label.

This will return `True`

    mockPID "form-component"
        |> checkPID "form-component"

-}
checkPID : String -> PID -> Bool
checkPID label (PID { prefix } ) =
    label == prefix


{-| Assert that a PID matches a label.

This will result in action `pass`

    mockPID "form-component"
        |> assertPID "form-component"

This will result in the action
`fail "PID form-component does not match expectation other-component"`

    mockPID "form-component"
        |> assertPID "other-component"

-}
assertPID : String -> PID -> Action msgIn
assertPID expected (PID { prefix }) =
    if prefix == expected then
        pass
    else
        "PID \"" ++ prefix ++ "\"does not match expectation \"" ++ expected
            |> fail


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
    -> Actor model (Process model o) (Msg i o)
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
                            |> System.sendToSingleton Navigation

                    Browser.External str ->
                        str
                            |> Navigation.Load
                            |> NavMsg
                            |> System.sendToSingleton Navigation
        , onUrlChange =
            Layout.UrlChanged
                >> LayoutMsg
                >> System.sendToSingleton LayoutActor
        , onDebug = always System.none
        }


initApp : () -> Url -> Nav.Key -> Msg msgIn msgOut
initApp _ url key =
    System.batch
        [ System.withSingletonPID LayoutActor System.addView
        , Navigation.Init key url
            |> NavMsg
            |> System.sendToSingleton Navigation
        , Layout.UrlChanged url
            |> LayoutMsg
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
        |> System.sendToSingleton LayoutActor



-- SYSTEM


type ActorName
    = LayoutActor
    | TestedActor
    | Navigation
    | LoremIpsum


type Process model o
    = P_Layout (Layout.Model o)
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
    -> Actor model (Process model o) (Msg i o)
    -> ActorName
    -> PID
    -> ( Process model o, Msg i o )
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
    -> Actor model (Process model o) (Msg i o)
    -> Process model o
    -> System.AppliedActor (Process model o) (Html (Msg i o)) (Msg i o)
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

loremIpsum : Actor LoremIpsum.Model (Process model o) (Msg i o)
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

navigation : Actor Navigation.Model (Process model o) (Msg i msgOut)
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


navigationMapOut : PID -> Navigation.MsgOut -> Msg i msgOut
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


layoutActor : Args i o -> Actor (Layout.Model o) (Process model o) (Msg i o)
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
                            |> System.sendToSingleton LayoutActor
                        , ComponentMsg msg
                            |> System.sendToPID subject
                        ]

                Layout.Delay _ _ ->
                    System.none -- handled in sandbox component

                Layout.Pass ->
                    System.none -- handled in sandbox component

                Layout.Fail _ ->
                    System.none -- handled in sandbox component

                Layout.Timeout _ ->
                    System.none -- handled in sandbox component

                Layout.SpawnChild title reply ->
                    System.spawn LoremIpsum
                        (\newPid ->
                            System.batch
                                [ reply newPid
                                    |> ComponentMsg
                                    |> System.sendToPID subject
                                , LoremIpsum.SetText title
                                    |> LoremIpsumMsg
                                    |> System.sendToPID newPid
                                , reply newPid
                                    |> toString
                                    |> Layout.InMessage
                                    |> Layout.LogMsg subject
                                    |> LayoutMsg
                                    |> System.sendToSingleton LayoutActor
                                ]
                        )

        Layout.NavigateToHref href ->
            Navigation.Push href
                |> NavMsg
                |> System.sendToSingleton Navigation


