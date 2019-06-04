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
type Action msgIn
    = SendMsg msgIn
    | SpawnChild String (PID -> msgIn)
    | Delay Float (Action msgIn)
    | Pass
    | Fail String
    | Timeout String

{-| Spawn a child component and send the PID to your component.

You can provide a String which will be displayed when the child
component is rendered (using `renderPID` in your layout component).


    Sandbox.spawnChild "Hello child" YourComponent.ReceiveChild

-}
spawnChild : String -> (PID -> msgIn) -> Action msgIn
spawnChild =
    SpawnChild



{-| Perform a delayed action on your sandboxed component. Delay in
milliseconds.

    Sandbox.sendMsg YourComponent.SomeMessage
        |> Sandbox.delay 1000

-}
delay : Float -> Action msgIn -> Action msgIn
delay =
    Delay


{-| Send a message to you sandboxed component

    Sandbox.sendMsg YourComponent.SomeMessage

-}
sendMsg : msgIn -> Action msgIn
sendMsg =
    SendMsg


{-| Flag test case as passed.

    Sandbox.pass
-}
pass : Action msgIn
pass =
    Pass


{-| Flag test case as failed. You can supply a message explaining what
went wrong.

    Sandbox.fail "Didn't receive some important out msg"
-}
fail : String -> Action msgIn
fail reason =
    Fail reason


{-| Set a timeout in milliseconds. This will cause the test to automatically fail
after the timeout if the test havn't been flagged as passed by then.

    Sandbox.timeout 1000
-}
timeout : Float -> Action msgIn
timeout t =
    "Timeout: " ++ (String.fromFloat t) ++ "ms"
        |> Timeout
        |> Delay t


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
            UrlChanged
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
        , UrlChanged url
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
        |> HandleMsgOut pid
        |> LayoutMsg
        |> System.sendToSingleton LayoutActor



-- SYSTEM


type ActorName
    = LayoutActor
    | TestedActor
    | Navigation
    | LoremIpsum


type Process model o
    = P_Layout (LayoutModel o)
    | P_Component model
    | P_Nav Navigation.Model
    | P_LoremIpsum LoremIpsum.Model

{-| Sadbox Msg
-}
type alias Msg msgIn msgOut =
    System.SysMsg ActorName (AppMsg msgIn msgOut)


type AppMsg msgIn msgOut
    = LayoutMsg (MsgIn msgIn msgOut)
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

loremIpsum : Actor LoremIpsum.Model (Process model o) (Msg msgIn msgOut)
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

navigation : Actor Navigation.Model (Process model o) (Msg msgIn msgOut)
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


layoutActor : Args i o -> Actor (LayoutModel o) (Process model o) (Msg i o)
layoutActor args =
    Actor.fromLayout
        { wrapModel = P_Layout
        , wrapMsg = LayoutMsg
        , mapIn = layoutMapIn
        , mapOut = layoutMapOut args.stringifyMsgIn
        }
        (layoutComponent
            { cases =
                List.indexedMap Tuple.pair args.cases
                    |> Dict.fromList
            , stringifyMsgOut = args.stringifyMsgOut
            , title = args.title
            }
        )


layoutMapIn : AppMsg i o -> Maybe (MsgIn i o)
layoutMapIn appMsg =
    case appMsg of
        LayoutMsg msg ->
            Just msg

        _ ->
            Nothing


layoutMapOut : (msgIn -> String) -> PID -> MsgOut msgIn msgOut -> Msg msgIn msgOut
layoutMapOut toString p componentMsg =
    case componentMsg of
        Spawn replyPID reply ->
             reply
                >> LayoutMsg
                >> System.sendToPID replyPID
                |> System.spawn TestedActor

        SetPageTitle title ->
            System.setDocumentTitle title

        PerformAction subject action ->
            case action of
                SendMsg msg ->
                    System.batch
                        [ toString msg
                            |> InMessage
                            |> LogMsg subject
                            |> LayoutMsg
                            |> System.sendToSingleton LayoutActor
                        , ComponentMsg msg
                            |> System.sendToPID subject
                        ]

                Delay _ _ ->
                    System.none -- handled in sandbox component

                Pass ->
                    System.none -- handled in sandbox component

                Fail _ ->
                    System.none -- handled in sandbox component

                Timeout _ ->
                    System.none -- handled in sandbox component

                SpawnChild title reply ->
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
                                    |> InMessage
                                    |> LogMsg subject
                                    |> LayoutMsg
                                    |> System.sendToSingleton LayoutActor
                                ]
                        )

        NavigateToHref href ->
            Navigation.Push href
                |> NavMsg
                |> System.sendToSingleton Navigation


-- Layout Component

type alias Config msgIn msgOut =
    { cases : Dict Int (TestCase msgIn msgOut)
    , stringifyMsgOut : msgOut -> String
    , title : String
    }


layoutComponent : Config i o -> Component.Layout (LayoutModel o) (MsgIn i o) (MsgOut i o) msg
layoutComponent config =
    { init = init config
    , update = update config
    , view = view config
    , onSystem = always SystemEvent.default
    , subs = always Sub.none
    }



type alias Child =
    { pid : PID
    }


type Message
    = InMessage String
    | OutMessage String

type TestResult
    = Waiting
    | TestFail String
    | TestPass


type alias LayoutModel msgOut =
    { pid : PID
    , pids : Dict Int Child
    , testResult : Dict String TestResult
    , initMsgsQueue : Dict String (List msgOut)
    , messages : Dict String (List Message)
    , displayCase : Maybe Int
    , cardMode : Maybe Int
    , title : String
    , currentUrl : Maybe Url
    }



--
-- Message Types
--


type MsgIn msgIn msgOut
    = NewPID Int PID
    | ReInit Int
    | SetTitle String
    | LogMsg PID Message
    | UrlChanged Url
    | NavigateTo String
    | RunAction PID (Action msgIn)
    | HandleMsgOut PID msgOut


type MsgOut msgIn msgOut
    = Spawn PID (PID -> MsgIn msgIn msgOut)
    | SetPageTitle String
    | PerformAction PID (Action msgIn)
    | NavigateToHref String



--
-- Component
--


init : Config i o -> PID -> ( LayoutModel o, List (MsgOut i o), Cmd (MsgIn i o) )
init config pid =
    ( { pid = pid
      , pids = Dict.empty
      , testResult = Dict.empty
      , initMsgsQueue = Dict.empty
      , displayCase = Nothing
      , cardMode = Nothing
      , messages = Dict.empty
      , currentUrl = Nothing
      , title = config.title
      }
    , config.cases
        |> Dict.keys
        |> List.map
            (\idx ->
                Spawn pid (NewPID idx)
            )
    , Cmd.none
    )


update : Config i o -> (MsgIn i o) -> LayoutModel o -> ( LayoutModel o, List (MsgOut i o), Cmd (MsgIn i o) )
update config msgIn model =
    case msgIn of
        NewPID idx pid ->
            let
                queuedMsgOut =
                    Dict.get (PID.toString pid) model.initMsgsQueue
                        |> Maybe.withDefault []

                ( testResults, outMsgs, cmds ) =
                    Dict.get idx config.cases
                        |> Maybe.map
                            (\testCase ->
                                List.concatMap testCase.onMsgOut queuedMsgOut
                                    ++ testCase.init
                                    |> runActions model.testResult pid
                            )
                        |> Maybe.withDefault ( model.testResult, [], Cmd.none )
            in
            ( { model
                | pids = Dict.insert idx (Child pid) model.pids
                , initMsgsQueue = Dict.remove (PID.toString pid) model.initMsgsQueue
                , testResult = testResults
              }
            , outMsgs
            , cmds
            )

        RunAction pid action ->
            let
                ( testResults, msgs, cmds ) =
                    runActions model.testResult pid [ action ]
            in
            ( { model | testResult = testResults }
            , msgs
            , cmds
            )

        HandleMsgOut pid msgOut ->
            let
                message =
                    config.stringifyMsgOut msgOut
                        |> OutMessage

                maybeIdx =
                    findIdxFromPID pid model.pids

                ( testResults, outMsgs, cmds ) =
                    maybeIdx
                        |> Maybe.andThen (\idx -> Dict.get idx config.cases)
                        |> Maybe.map
                            (\test ->
                                test.onMsgOut msgOut
                                    |> runActions model.testResult pid
                            )
                        |> Maybe.withDefault ( model.testResult, [], Cmd.none )

                m2 =
                    case maybeIdx of
                        Just _ ->
                            model

                        Nothing ->
                            { model
                                | initMsgsQueue =
                                    Dict.update
                                        (PID.toString pid)
                                        (\maybeMsgs ->
                                            case maybeMsgs of
                                                Just msgs ->
                                                    Just <| msgs ++ [ msgOut ]

                                                Nothing ->
                                                    Just [ msgOut ]
                                        )
                                        model.initMsgsQueue
                            }
            in
            ( { m2
                | messages = logMessage pid message m2.messages
                , testResult = testResults
              }
            , outMsgs
            , cmds
            )

        ReInit idx ->
            ( model
            , [ Spawn model.pid (NewPID idx)
              ]
            , Cmd.none
            )

        SetTitle t ->
            ( { model | title = t }
            , []
            , Cmd.none
            )

        LogMsg pid message ->
            ( { model
                | messages = logMessage pid message model.messages
              }
            , []
            , Cmd.none
            )

        UrlChanged url ->
            let
                ( path, query ) =
                    parseUrl url

                strParam key default =
                    Dict.get key query
                        |> Maybe.withDefault default
            in
            case path of
                [ "testcase", n ] ->
                    let
                        idx =
                            String.toInt n

                        title =
                            idx
                                |> Maybe.andThen (\i -> Dict.get i config.cases)
                                |> Maybe.map .title
                                |> Maybe.withDefault model.title
                    in
                    ( { model
                            | displayCase = idx
                            , cardMode = Nothing
                            , currentUrl = Just url
                      }
                    , [ SetPageTitle title ]
                    , Cmd.none
                    )

                [ "cardmode", n ] ->
                    let
                        cols =
                            String.toInt n
                                |> Maybe.map (clamp 1 8)
                    in
                    ( { model
                            | displayCase = Nothing
                            , cardMode = cols
                            , currentUrl = Just url
                      }
                    , [ SetPageTitle model.title ]
                    , Cmd.none
                    )

                _ ->
                    ( { model
                            | displayCase = Nothing
                            , currentUrl = Just url
                            , cardMode = Nothing
                      }
                    , [ SetPageTitle model.title ]
                    , Cmd.none
                    )

        NavigateTo href ->
            ( model
            , [ NavigateToHref href ]
            , Cmd.none
            )

logMessage : PID -> Message -> Dict String (List Message) -> Dict String (List Message)
logMessage pid message dict =
    Dict.update
        (PID.toString pid)
        (\mbMsg ->
            case mbMsg of
                Just messages ->
                    message
                        :: messages
                        |> Just

                Nothing ->
                    [ message ]
                        |> Just
        )
        dict


parseUrl : Url -> ( List String, Dict String String ) 
parseUrl url =
    let
        toPath pathStr =
            pathStr
                |> String.split "/"
                |> List.map String.trim

        toQuery queryStr =
            queryStr
                |> String.split "&"
                |> List.filterMap
                    (\paramStr ->
                        case String.split "=" paramStr of
                            [ key, val ] ->
                                Just
                                    ( String.trim key
                                    , String.trim val
                                    )
                            _ ->
                                Nothing

                    )
                |> Dict.fromList
    in
    url.fragment
        |> Maybe.map
            (String.split "?"
                >> (\ls ->
                    case ls of
                        [ pathStr, queryStr ] ->
                            ( toPath pathStr
                            , toQuery queryStr
                            )

                        [ pathStr ] ->
                            ( toPath pathStr
                            , Dict.empty
                            )

                        _ ->
                            ( [], Dict.empty )
                    )
            )
        |> Maybe.withDefault ( [], Dict.empty )


buildHref : List String -> Dict String String -> String
buildHref path query =
    let
        queryString =
            query
                |> Dict.toList
                |> List.map (\( k, v ) -> k ++ "=" ++ v)
                |> String.join "&"

        pathString =
            String.join "/" path
    in
    if Dict.isEmpty query then
        "#" ++ pathString
    else
        "#" ++ pathString ++ "?" ++ queryString



findIdxFromPID : PID -> Dict Int Child -> Maybe Int
findIdxFromPID pid dict =
    dict
        |> Dict.toList
        |> List.find (\( _, child ) -> child.pid == pid)
        |> Maybe.map Tuple.first


runActions : Dict String TestResult
    -> PID
    -> List (Action i)
    -> ( Dict String TestResult, List (MsgOut i o), Cmd (MsgIn i o) )
runActions testResults pid actions =
    actions
        |> List.foldl
            (\action ( results, msgs, cmds ) ->
                case action of
                    Delay delay_ delayedAction ->
                        ( results
                        , msgs
                        , cmds
                            ++ [ Component.toCmdWithDelay
                                    delay_
                                    (delayedAction
                                        |> RunAction pid
                                    )
                                ]
                        )

                    Pass ->
                        ( case Dict.get (PID.toString pid) results of
                            Just (TestFail _)->
                                results

                            _ ->
                                Dict.insert (PID.toString pid) TestPass results
                        , msgs
                        , cmds
                        )

                    Fail reason ->
                        ( case Dict.get (PID.toString pid) results of
                            Just (TestFail _)->
                                results

                            _ ->
                                Dict.insert (PID.toString pid) (TestFail reason) results
                        , msgs
                        , cmds
                        )

                    Timeout reason ->
                        ( case Dict.get (PID.toString pid) results of
                            Just TestPass ->
                                results

                            Just (TestFail _)->
                                results

                            _ ->
                                Dict.insert (PID.toString pid) (TestFail reason) results
                        , msgs
                        , cmds
                        )

                    _ ->
                        ( results
                        , msgs ++ [ PerformAction pid action ]
                        , cmds
                        )
            )
            ( testResults, [], [] )
        |> Component.mapThird Cmd.batch


testResult : PID -> Dict String TestResult -> TestResult
testResult pid dict =
    Dict.get (PID.toString pid) dict
        |> Maybe.withDefault Waiting


-- VIEW


type alias ColorConfig =
    { bgColor : String
    , testCaseBgColor : String
    , componentBgColor : String
    }


defaultColors : ColorConfig
defaultColors =
    { bgColor = "#888"
    , testCaseBgColor = "#fff"
    , componentBgColor = "transparent"
    }


colorsFromQueryParams : Dict String String -> ColorConfig
colorsFromQueryParams queryParams =
    let
        default =
            defaultColors
    in
    { bgColor =
        Dict.get "bgColor" queryParams
            |> Maybe.withDefault default.bgColor
    , testCaseBgColor =
        Dict.get "testCaseBgColor" queryParams
            |> Maybe.withDefault default.testCaseBgColor
    , componentBgColor =
        Dict.get "componentBgColor" queryParams
            |> Maybe.withDefault default.componentBgColor
    }


view : Config msgIn msgOut -> ((MsgIn msgIn msgOut) -> msg) -> LayoutModel msgOut -> (PID -> Html msg) -> Html msg
view config toSelf model renderPID =
    let
        ( currentPath, queryParams ) =
            model.currentUrl
                |> Maybe.map parseUrl
                |> Maybe.withDefault ( [], Dict.empty )

        color =
            colorsFromQueryParams queryParams

        fullScreenMode =
            let
                resolve maybe fn =
                    maybe
                        |> Maybe.andThen fn
            in
            resolve (Dict.get "fullscreen" queryParams) <|\_ ->
            resolve model.displayCase <| \caseIdx ->
            resolve (Dict.get caseIdx config.cases) <| \testCase ->
            resolve (Dict.get caseIdx model.pids) <| \child ->
                Just ( caseIdx, testCase, child.pid )

        ( passed, failed, waiting ) =
            config.cases
                |> Dict.keys
                |> List.map
                    (\idx ->
                        Dict.get idx model.pids
                            |> Maybe.map (\child -> testResult child.pid model.testResult)
                            |> Maybe.withDefault Waiting
                    )
                |> List.foldl
                    (\result ( p, f, w ) ->
                        case result of
                            Waiting ->
                                ( p, f, w + 1 )

                            TestPass ->
                                ( p + 1, f, w )

                            TestFail _ ->
                                ( p, f + 1, w )
                    )
                    ( 0, 0, 0 )

    in
    case (currentPath, fullScreenMode) of
        ( [ "markdown" ], _ ) ->
            renderCli config model { pass = passed, fail = failed, wait = waiting }
                |> Html.text
                |> List.singleton
                |> Html.pre
                    [ HA.id "markdown-output"
                    ]

        ( _, Just ( caseIdx, testCase, pid ) ) ->
            renderPID pid

        _ ->
            Html.div
                [ HA.class "ams-pagewrap"
                , HA.style "background" color.bgColor
                ]
                [ Html.node "style"
                    []
                    [ css
                        |> String.replace "{{bgColor}}" color.bgColor
                        |> String.replace "{{testCaseBg}}" color.testCaseBgColor
                        |> String.replace "{{componentBg}}" color.componentBgColor
                        |> Html.text
                    ]
                , pageHeader toSelf model color
                , Html.hr [ HA.class "ams-hr" ] []
                , Html.div
                    [ HA.class "ams-above-cases"
                    ]
                    [ Html.div
                        [ HA.class "ams-test-summary"
                        ]
                        [ Html.span [ HA.class "ams-test-summary--pass" ] [ Html.text <| "pass: " ++ (String.fromInt passed) ]
                        , Html.span [ HA.class "ams-test-summary--fail" ] [ Html.text <| " fail: " ++ (String.fromInt failed) ]
                        , Html.span [ HA.class "ams-test-summary--wait" ] [ Html.text <| " wait: " ++ (String.fromInt waiting) ]
                        ]
                    , testCaseSelectBox config toSelf model
                    ]
                , renderCases config toSelf renderPID model
                ]


renderCli : Config i o -> LayoutModel o -> { pass : Int, fail : Int, wait : Int } -> String
renderCli config model summary =
    config.cases
        |> Dict.toList
        |> List.map
            (\( idx, test ) ->
                Dict.get idx model.pids
                    |> Maybe.map
                        (\child ->
                            let
                                result =
                                    testResult child.pid model.testResult
                            in
                            case result of
                                Waiting ->
                                    [ ( "title", test.title )
                                    , ( "status","wait" )
                                    ]

                                TestPass ->
                                    [ ( "title", test.title )
                                    , ( "status", "pass" )
                                    ]

                                TestFail reason ->
                                        [ ( "title", test.title )
                                        , ( "status", "fail" )
                                        , ( "reason", reason )
                                        ]
                        )
                    |> Maybe.withDefault ([ ( "title", test.title ) ])
                    |> List.map
                        (\( k, v ) ->
                            "- " ++ k ++ ": " ++ v
                        )
                    |> String.join "\n"
            )
        |> List.indexedMap
            (\i t ->
                "## Test " ++ (String.fromInt i) ++ "\n" ++ t
            )
        |> String.join "\n\n"
        |> (\tests ->
                "# "
                ++ config.title
                ++ "\n\nSummary:"
                ++ "\n- total pass: " ++ (String.fromInt summary.pass)
                ++ "\n- total fail: " ++ (String.fromInt summary.fail)
                ++ "\n- total wait: " ++ (String.fromInt summary.wait)
                ++ "\n\n"
                ++ tests
           )


renderCases config toSelf renderPID model =
    let
        testCases =
            config.cases
                |> Dict.toList

        pids =
            model.pids
                |> Dict.values
                |> List.map .pid
    in
    case model.cardMode of
        Just columnCount ->
            let
                width =
                    "calc("
                        ++ (100 / (toFloat columnCount) |> String.fromFloat)
                        ++ "% - "
                        ++ ((toFloat columnCount - 1) / (toFloat columnCount) |> String.fromFloat)
                        ++ "rem)"

            in
            List.greedyGroupsOf columnCount pids
                |> List.map
                    (\row ->
                        Html.div
                            [ HA.class "ams-card__row"
                            ]
                            (List.map
                                (\col ->
                                    Html.div
                                        [ HA.style "width" width
                                        , HA.class "ams-card__cell"
                                        ]
                                        [ renderPID col ]
                                )
                                row
                            )
                    )
                |> Html.div
                    [ HA.class "ams-card"
                    ]

        Nothing ->
            Html.div
                []
                ( model.displayCase
                    |> Maybe.andThen
                        (\idx ->
                            Dict.get idx config.cases
                                |> Maybe.map
                                    (\testCase ->
                                        let
                                            child =
                                                Dict.get idx model.pids
                                        in
                                        Maybe.map (renderChild model toSelf renderPID idx testCase) child
                                            |> Maybe.withDefault (Html.text "")
                                            |> List.singleton
                                    )
                        )
                    |> Maybe.withDefault
                        ( testCases
                            |> List.map
                                (\( idx, testCase ) ->
                                    let
                                        child =
                                            Dict.get idx model.pids
                                    in
                                    Maybe.map (renderChild model toSelf renderPID idx testCase) child
                                        |> Maybe.withDefault (Html.text "")
                                )
                        )
                )


pageHeader : (MsgIn msgIn msgOut -> msg) -> LayoutModel msgOut -> ColorConfig -> Html msg
pageHeader toSelf model color =
    let
        ( currentPath, queryParams ) =
            model.currentUrl
                |> Maybe.map parseUrl
                |> Maybe.withDefault ( [], Dict.empty )

        colorHref key val =
            buildHref
                currentPath
                (Dict.insert key val queryParams)

        resetColors =
            queryParams
                |> Dict.remove "bgColor"
                |> Dict.remove "testCaseBgColor"
                |> Dict.remove "componentBgColor"
                |> buildHref currentPath
                |> NavigateTo
                |> toSelf
    in
    Html.div
        [ HA.class "ams-page-header"
        ]
        [ Html.h1
            [ HA.class "ams-pagetitle"
            ]
            [ Html.text model.title
            ]
        , Html.div
            [ HA.class "ams-colortoolbar"
            ]
            [ colorInput
                "body-bg"
                "Body Bg"
                color.bgColor
                (colorHref "bgColor"
                    >> NavigateTo
                    >> toSelf
                )
            , colorInput
                "body-bg"
                "Test case Bg"
                color.testCaseBgColor
                (colorHref "testCaseBgColor"
                    >> NavigateTo
                    >> toSelf
                )
            , colorInput
                "body-bg"
                "Component Bg"
                color.componentBgColor
                (colorHref "componentBgColor"
                    >> NavigateTo
                    >> toSelf
                )
            , Html.button
                [ Events.onClick resetColors
                , HA.class "ams-button--alt"
                , HA.style "margin-left" "16px"
                ]
                [ Html.text "Reset colors"
                ]
            ]
        ]



fullscreenToggle : (MsgIn msgIn msgOut -> msg) -> LayoutModel msgOut -> Int -> Html msg
fullscreenToggle toSelf model testCaseIdx =
    let
        ( currentPath, queryParams ) =
            model.currentUrl
                |> Maybe.map parseUrl
                |> Maybe.withDefault ( [], Dict.empty )

        isSelected =
            Dict.get "fullscreen" queryParams
                |> Maybe.map (always True)
                |> Maybe.withDefault False

        onClick =
            Dict.insert "fullscreen" "on" queryParams
                |> buildHref [ "testcase", String.fromInt testCaseIdx ]
                |> NavigateTo
                |> toSelf
                |> Events.onClick
    in
    Html.button
        [ HA.class "ams-button ams-fullscreen-toggle"
        , onClick
        ]
        [ Html.text "View fullscreen"
        ]


testCaseSelectBox : Config msgIn msgOut -> (MsgIn msgIn msgOut -> msg) -> LayoutModel msgOut -> Html msg
testCaseSelectBox config toSelf model =
    let
        testCases =
            config.cases
                |> Dict.toList

        cardLayouts =
            List.range 2 8
                |> List.map (\i -> ( i, (String.fromInt i) ++ " Columns"))
                |> (::) ( 1, "1 Column" )

        ( currentPath, queryParams ) =
            model.currentUrl
                |> Maybe.map parseUrl
                |> Maybe.withDefault ( [], Dict.empty )
    in
    Html.div
        [ HA.class "ams-select-testcase"
        ]
        [ Html.select
            [ Events.onInput (toSelf << NavigateTo)
            ]
            (cardLayouts
                |> List.map
                    (\( cols, label ) ->
                        Html.option
                            [ HA.value <| buildHref [ "cardmode", String.fromInt cols ] queryParams
                            , if model.cardMode == Just cols then
                                HA.selected True
                              else
                                HA.selected False
                            ]
                            [ Html.text label
                            ]
                    )
                |> (::)
                    ( Html.option
                        [ HA.value <| buildHref [] Dict.empty
                        ]
                        [ Html.text "-- Show UI Cards --"
                        ]
                    )
            )
        , Html.select
            [ Events.onInput (toSelf << NavigateTo)
            ]
            (testCases
                |> List.map
                    (\( idx, testCase ) ->
                        let
                            result =
                                Dict.get idx model.pids
                                    |> Maybe.map
                                        (\child ->
                                            testResult child.pid model.testResult
                                        )
                                    |> Maybe.withDefault Waiting
                        in
                        Html.option
                            [ HA.value <| buildHref [ "testcase", String.fromInt idx ] queryParams
                            , if model.displayCase == Just idx then
                                HA.selected True
                              else
                                HA.selected False
                            , HA.style
                                "color"
                                (case result of
                                    Waiting -> ""
                                    TestPass -> "#009911"
                                    TestFail _ -> "#aa0000"
                                )
                            ]
                            [ Html.text testCase.title
                            ]
                    )
                |> (::)
                    ( Html.option
                        [ HA.value <| buildHref [] queryParams
                        ]
                        [ Html.text "-- Show all test cases --"
                        ]
                    )
            )
        ]


colorInput : String -> String -> String -> (String -> msg) -> Html msg
colorInput htmlID label color toMsg =
    Html.div
        [ HA.class "ams-colorinput__row"
        ]
        [ Html.input
            [ HA.type_ "color"
            , Events.onInput toMsg
            -- , HA.value color
            , HA.id htmlID
            , HA.class "ams-colorinput__input"
            ]
            []
        , Html.label
            [ HA.for htmlID
            , HA.class "ams-colorinput__label"
            ]
            [ Html.text label
            ]
        ]


renderChild : LayoutModel o -> ((MsgIn i o) -> msg) -> (PID -> Html msg) -> Int -> TestCase i o -> Child -> Html msg
renderChild model toSelf renderPID idx testCase child =
    let
        result =
            testResult child.pid model.testResult
    in
    Html.div
        [ HA.class "ams-testcase"
        ]
        [ Html.div
            [ HA.class 
                ( "ams-testcase__header"
                    ++ ( case result of
                            Waiting -> " ams-testcase__header--waiting"
                            TestPass -> " ams-testcase__header--pass"
                            TestFail _ -> " ams-testcase__header--fail"
                       )
                )
            ]
            [ Html.div
                [ HA.class "ams-testcase__result"
                ]
                [ case result of
                    Waiting ->
                        Html.text "Waiting..."

                    TestPass ->
                        Html.text "Pass"

                    TestFail reason ->
                        Html.text reason
                ]
            , Html.div
                [ HA.class
                    ("ams-testcase__toolbar"
                    )
                ]
                [ child.pid
                    |> (\(PID { key }) -> "PID: " ++ String.fromInt key)
                    |> Html.text
                    |> List.singleton
                    |> Html.span [ HA.class "ams-testcase__pidLabel" ]
                , fullscreenToggle toSelf model idx
                , Html.button
                    [ Events.onClick (toSelf <| ReInit idx)
                    , HA.class "ams-button"
                    ]
                    [ Html.text "Reset test"
                    ]
                ]
            ]
        , Html.div
            [ HA.class "ams-testcase__content"
            ]
            [ Html.h2 
                [ HA.class "ams-testcase__title"
                ]
                [ Html.text testCase.title
                ]
            , Markdown.toHtml
                [ HA.class "ams-testcase__desc"
                ]
                testCase.desc
            , section
                [ heading 5 "Component view:"
                , Html.div
                    [ HA.class "ams-testcase__componentview"
                    ]
                    [ renderPID child.pid
                    ]
                ]
            , section
                [ heading 5 "Message log:"
                , Html.div
                    [ HA.class "ams-messagelog"
                    ]
                    (model.messages
                        |> Dict.get (PID.toString child.pid)
                        |> Maybe.map
                            (List.reverse
                                >> List.map
                                    (\message ->
                                        case message of
                                            InMessage inMsg ->
                                                "> "
                                                    ++ inMsg
                                                    |> Html.text
                                                    |> List.singleton
                                                    |> Html.span
                                                        [ HA.class "ams-messagelog__inmsg"
                                                        ]

                                            OutMessage outMsg ->
                                                "  -> "
                                                    ++ outMsg
                                                    |> Html.text
                                                    |> List.singleton
                                                    |> Html.span
                                                        [ HA.class "ams-messagelog__outmsg"
                                                        ]
                                    )
                            )
                        |> Maybe.withDefault []
                    )
                ]
            ]
        ]


section : List (Html msg) -> Html msg
section =
    Html.div
        [ HA.class "ams-section"
        ]


heading : Int -> String -> Html msg
heading lvl txt =
    let
        elem =
            case lvl of
                1 -> Html.h1
                2 -> Html.h2
                3 -> Html.h3
                4 -> Html.h4
                5 -> Html.h5
                _ -> Html.h5
    in
    elem
        [ HA.class "ams-heading"
        ]
        [ Html.text txt
        ]


css : String
css =
    """
    html, body {
        margin: 0;
        padding: 0;
        font-size: 16px;
    }
    body {
        background: {{bgColor}};
    }


    /** Page **/

    .ams-pagewrap {
        max-width: 1400px;
        margin: 0 auto;
        padding: 0 1rem;
    }
    .ams-section {
        margin: 3rem 0;
    }
    .ams-page-header {
        display: flex;
        flex-direction: row;
        justify-content: center;
        align-items: center;
        flex-wrap: wrap;
        margin: 1rem 0 0 0;
    }
    .ams-pagetitle {
        font-family: sans-serif;
        color: #fff;
        margin: 0;
        flex: 1 1 auto;
    }

    .ams-color-settings {
        flex: 0 0 auto;
    }

    .ams-select-testcase {
        display: flex;
        flex-direction: row;
        align-items: center;
        justify-content: flex-end;
    }

    .ams-select-testcase select {
    }

    .ams-heading {
        font-family: sans-serif;
        margin: 0;
        color: #333;
    }

    h1.ams-heading { font-size: 2.5rem; margin-top: 2.5rem; }
    h2.ams-heading { font-size: 1.8rem; margin-top: 1.8rem; }
    h3.ams-heading { font-size: 1.5rem; margin-top: 1.5rem; }
    h4.ams-heading { font-size: 1.25rem; margin-top: 1.25rem; }
    h5.ams-heading { font-size: 0.8rem; margin-top: 0.8rem; }

    .ams-hr {
        border-color: #fff;
    }

    .ams-button, 
    .ams-button--alt {
        -webkit-appearance: none;
        background: #888;
        padding: 0.25rem 1rem;
        border: none;
        color: #fff;
        border-radius: 2px;
        line-height: 1rem;
        transition: background 0.1s ease-out;
        cursor: pointer;
        outline: none;
        font-family: sans-serif;
    }

    .ams-button--alt {
        background: #555;
    }

    .ams-button--alt:hover {
        background: #000;
    }

    .ams-button:hover {
        background: #555;
    }

    .ams-colortoolbar {
        display: flex;
        flex-direction: row;
        align-items: center;
        justify-content: flex-end;
        flex-wrap: wrap;
    }

    .ams-colorinput__row {
        display: flex;
        flex-direction: row;
        align-items: center;
    }

    @media all and (max-width: 650px) {
        .ams-colorinput__row {
            width: 100%;
        }
    }

    .ams-colorinput__label {
        padding: 0 1rem;
        line-height: 1;
        font-size: 0.8rem;
        color: #fff;
        font-family: monospace;
    }
    .ams-above-cases {
        display: flex;
        justify-content: space-between;
    }

    .ams-test-summary {
        font-family: monospace;
    }

    .ams-test-summary--pass {
        color: #005511;
    }

    .ams-test-summary--fail {
        color: #aa0000;
    }

    /** Test Case **/
    .ams-testcase {
        border-radius: 4px;
        margin: 1rem 0;
        background: {{testCaseBg}};
        position: relative;
    }

    .ams-testcase__content {
        border-radius: 4px;
        background: {{testCaseBg}};
        position: relative;
        padding: 1rem;
    }

    .ams-testcase__content > *:last-child {
        margin-bottom: 0;
    }
    .ams-testcase__title {
        font-family: sans-serif;
        color: #333;
        font-size: 1.5rem;
        margin: 0;
    }
    .ams-testcase__desc {
        font-family: sans-serif;
        max-width: 50rem;
    }
    .ams-testcase__desc code {
        background-color: #eee;
        padding: 1px 5px;
    }
    .ams-testcase__componentview {
        border: 1px solid #e7e7e7;
        border-radius: 4px;
        background: {{componentBg}};
    }

    .ams-testcase__header {
        display: flex;
        flex-direction: row;
        justify-content: space-between;
        align-items: center;
        border-bottom: 1px solid #f6f6f6;
        padding: 0.5rem 0;
        background: #f6f6f6;
        border-top-left-radius: 4px;
        border-top-right-radius: 4px;
    }
    .ams-testcase__header--pass {
        background: #44f655;
    }
    .ams-testcase__header--fail {
        background: #f65555;
    }
    .ams-testcase__result {
        margin-left: 1rem;
        font-family: monospace;
    }

    .ams-testcase__toolbar {
        display: flex;
        flex-direction: row;
        justify-content: flex-end;
        align-items: center;
    }
    .ams-testcase__toolbar > * {
        margin-right: 1rem;
    }
    .ams-testcase__pidLabel {
        font-family: monospace;
        font-size: 0.8rem;
        color: #333;
    }

    /** Card Mode **/
    .ams-card__row {
        display: flex;
        flex-direction: row;
        justify-content: space-between;
        margin-top: 1rem;
    }

    .ams-card__cell {
        position: relative;
    }

    /** Message Log **/
    .ams-messagelog {
        border-radius: 4px;
        background: #eee;
        padding: 0.5rem 1rem;
        min-height: 16px;
        margin: 0;
        overflow: auto;
    }
    .ams-messagelog__inmsg {
        font-family: monospace;
        color: #c15858;
        display: block;
        white-space: pre;
    }
    .ams-messagelog__outmsg {
        font-family: monospace;
        color: #3075b7;
        display: block;
        white-space: pre;
    }
"""
