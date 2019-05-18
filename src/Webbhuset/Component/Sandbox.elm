module Webbhuset.Component.Sandbox exposing
    ( SandboxProgram
    , TestCase
    , ui
    , layout
    , service
    , sendMsg
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

    module YourComponentSandbox exposing (..)

    import YourComponent

    main : SandboxProgram YourComponent.Model YourComponent.MsgIn
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
            }


@docs ui, layout, service

# Create a Test Case

A Test Case is just a record with a title and description together
with a list of Actions you want to perform on your sandboxed component.

@docs TestCase

    testCase1 : Sandbox.TestCase YourComponent.MsgIn
    testCase1 =
        Sandbox.TestCase
            "Test Case Title"
            "Test Case Description"
            [ Sandbox.sendMsg YourComponent.Hello
            , Sandbox.spawnChild "Child Title" YourComponent.ReceiveChildPID
            ]

## Actions

@docs sendMsg, spawnChild

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Webbhuset.Actor as Actor exposing (Actor)
import Webbhuset.ActorSystem as System
import Webbhuset.Component as Component
import Webbhuset.Component.Navigation as Navigation
import Webbhuset.Component.LoremIpsum as LoremIpsum
import Webbhuset.Internal.PID exposing (PID(..))
import Webbhuset.PID as PID
import Browser.Navigation as Nav
import Browser
import Url exposing (Url)


{-| The Application model

-}
type alias Model m msgIn =
    System.Model ActorName (Process m msgIn)


{-| The Program type of your main function.

-}
type alias SandboxProgram model msgIn =
    Program () (Model model msgIn) (Msg msgIn)


{-| A test case for the Component

-}
type alias TestCase msgIn =
    { title : String
    , desc : String
    , init : List (Action msgIn)
    }


type Action msgIn
    = SendMsg msgIn
    | SpawnLorem String (PID -> msgIn)


{-| Spawn a child component and send the PID to your component.

You can provide a String which will be displayed when the child
component is rendered (using `renderPID` in your layout component).


    Sandbox.spawnChild "Hello child" YourComponent.ReceiveChild

-}
spawnChild : String -> (PID -> msgIn) -> Action msgIn
spawnChild =
    SpawnLorem


{-| Send a message to you sandboxed component

-}
sendMsg : msgIn -> Action msgIn
sendMsg =
    SendMsg


{-| Sandbox a UI Component

-}
ui :
    { title : String
    , component : Component.UI model msgIn msgOut
    , cases : List (TestCase msgIn)
    , stringifyMsgIn : msgIn -> String
    , stringifyMsgOut : msgOut -> String
    }
    -> SandboxProgram model msgIn
ui args =
    Actor.fromUI
        { wrapModel = P_Component
        , wrapMsg = ComponentMsg
        , mapIn = testedMapIn
        , mapOut = testedMapOut args.stringifyMsgOut
        }
        args.component
        |> toApplication args


{-| Sandbox a Layout Component

-}
layout :
    { title : String
    , component : Component.Layout model msgIn msgOut (Msg msgIn)
    , cases : List (TestCase msgIn)
    , stringifyMsgIn : msgIn -> String
    , stringifyMsgOut : msgOut -> String
    }
    -> SandboxProgram model msgIn
layout args =
    Actor.fromLayout
        { wrapModel = P_Component
        , wrapMsg = ComponentMsg
        , mapIn = testedMapIn
        , mapOut = testedMapOut args.stringifyMsgOut
        }
        args.component
        |> toApplication args


{-| Sandbox a Service Component

You need to provied a `view` function which renders the model of
your service component.

-}
service :
    { title : String
    , component : Component.Service model msgIn msgOut
    , cases : List (TestCase msgIn)
    , view : model -> Html msgIn
    , stringifyMsgIn : msgIn -> String
    , stringifyMsgOut : msgOut -> String
    }
    -> SandboxProgram model msgIn
service args =
    Actor.fromUI
        { wrapModel = P_Component
        , wrapMsg = ComponentMsg
        , mapIn = testedMapIn
        , mapOut = testedMapOut args.stringifyMsgOut
        }
        { init = args.component.init
        , update = args.component.update
        , kill = args.component.kill
        , subs = args.component.subs
        , view = args.view
        }
        |> toApplication args


toApplication args testedActor =
    System.application
        { init = initApp args.title
        , spawn = spawn args.stringifyMsgIn args.cases testedActor
        , apply = applyModel args.stringifyMsgIn args.cases testedActor
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
            UrlChanged
                >> DevMsg
                >> System.toAppMsg
                >> System.sendToSingleton DevActor
        }


initApp : String -> () -> Url -> Nav.Key -> Msg msgIn
initApp title _ url key =
    System.batch
        [ System.withSingletonPID DevActor System.addView
        , Navigation.Init key url
            |> NavMsg
            |> System.toAppMsg
            |> System.sendToSingleton Navigation
        , SetTitle title
              |> DevMsg
              |> System.toAppMsg
              |> System.sendToSingleton DevActor
        , UrlChanged url
            |> DevMsg
            |> System.toAppMsg
            |> System.sendToSingleton DevActor
        ]


testedMapIn : AppMsg msgIn -> Maybe msgIn
testedMapIn appMsg =
    case appMsg of
        ComponentMsg msg ->
            Just msg

        _ ->
            Nothing


testedMapOut : (msgOut -> String) -> PID -> msgOut -> Msg msgIn
testedMapOut toString pid componentMsg =
    componentMsg
        |> toString
        |> OutMessage
        |> AddMsg pid
        |> DevMsg
        |> System.toAppMsg
        |> System.sendToSingleton DevActor



-- SYSTEM


type ActorName
    = DevActor
    | TestedActor
    | Navigation
    | LoremIpsum


type Process model msgIn
    = P_Dev (DevModel msgIn)
    | P_Component model
    | P_Nav Navigation.Model
    | P_LoremIpsum LoremIpsum.Model


type alias Msg msgIn =
    System.SysMsg ActorName (AppMsg msgIn)


type AppMsg msgIn
    = DevMsg MsgIn
    | ComponentMsg msgIn
    | NavMsg Navigation.MsgIn
    | LoremIpsumMsg LoremIpsum.MsgIn


spawn : (msgIn -> String)
    -> List (TestCase msgIn)
    -> Actor model (Process model msgIn) (Msg msgIn)
    -> ActorName
    -> PID
    -> ( Process model msgIn, Msg msgIn )
spawn toString cases tested name =
    case name of
        DevActor ->
            .init (actor toString cases)

        TestedActor ->
            tested.init

        Navigation ->
            navigation.init

        LoremIpsum ->
            loremIpsum.init


applyModel :
    (msgIn -> String)
    -> List (TestCase msgIn)
    -> Actor model (Process model msgIn) (Msg msgIn)
    -> Process model msgIn
    -> System.AppliedActor (Process model msgIn) (Html (Msg msgIn)) (Msg msgIn)
applyModel toString cases testedActor process =
    case process of
        P_Dev model ->
            System.applyModel (actor toString cases) model

        P_Component model ->
            System.applyModel testedActor model

        P_Nav model ->
            System.applyModel navigation model

        P_LoremIpsum model ->
            System.applyModel loremIpsum model

-- Lorem Ipsum

loremIpsum : Actor LoremIpsum.Model (Process model msgIn) (Msg msgIn)
loremIpsum =
    Actor.fromUI
        { wrapModel = P_LoremIpsum
        , wrapMsg = LoremIpsumMsg
        , mapIn = loremIpsumMapIn
        , mapOut = \_ _ -> System.none
        }
        LoremIpsum.component


loremIpsumMapIn : AppMsg msgIn -> Maybe LoremIpsum.MsgIn
loremIpsumMapIn appMsg =
    case appMsg of
        LoremIpsumMsg msg ->
            Just msg

        _ ->
            Nothing

-- Navigation

navigation : Actor Navigation.Model (Process model msgIn) (Msg msgIn)
navigation =
    Actor.fromService
        { wrapModel = P_Nav
        , wrapMsg = NavMsg
        , mapIn = navigationMapIn
        , mapOut = navigationMapOut
        }
        Navigation.component


navigationMapIn : AppMsg msgIn -> Maybe Navigation.MsgIn
navigationMapIn appMsg =
    case appMsg of
        NavMsg msg ->
            Just msg

        _ ->
            Nothing


navigationMapOut : PID -> Navigation.MsgOut -> Msg msgIn
navigationMapOut self componentMsg =
    case componentMsg of
        Navigation.NoOut ->
            System.none

-- Test Runner Actor


actor : (msgIn -> String) -> List (TestCase msgIn) -> Actor (DevModel msgIn) (Process model msgIn) (Msg msgIn)
actor toString cases =
    Actor.fromLayout
        { wrapModel = P_Dev
        , wrapMsg = DevMsg
        , mapIn = mapIn
        , mapOut = mapOut toString
        }
        (component
            { cases =
                List.indexedMap Tuple.pair cases
                    |> Dict.fromList
            }
        )


mapIn : AppMsg msgIn -> Maybe MsgIn
mapIn appMsg =
    case appMsg of
        DevMsg msg ->
            Just msg

        _ ->
            Nothing


mapOut : (msgIn -> String) -> PID -> MsgOut msgIn -> Msg msgIn
mapOut toString p componentMsg =
    case componentMsg of
        Spawn replyPID reply ->
             reply
                >> DevMsg
                >> System.toAppMsg
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
                            |> AddMsg subject
                            |> DevMsg
                            |> System.toAppMsg
                            |> System.sendToSingleton DevActor
                        , ComponentMsg msg
                            |> System.toAppMsg
                            |> System.sendToPID subject
                        ]

                SpawnLorem title reply ->
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
                                    |> InMessage
                                    |> AddMsg subject
                                    |> DevMsg
                                    |> System.toAppMsg
                                    |> System.sendToSingleton DevActor
                                ]
                        )



-- Test Runner Component

type alias Config msgIn =
    { cases : Dict Int (TestCase msgIn)
    }


component : Config msgIn -> Component.Layout (DevModel msgIn) MsgIn (MsgOut msgIn) msg
component config =
    { init = init config
    , update = update config
    , view = view config
    , kill = kill
    , subs = always Sub.none
    }



type alias Child =
    { pid : PID
    }


type Message
    = InMessage String
    | OutMessage String


type alias DevModel msgIn =
    { pid : PID
    , cases : Dict Int (TestCase msgIn)
    , pids : Dict Int Child
    , messages : Dict String (List Message)
    , displayCase : Maybe Int
    , bgColor : String
    , title : String
    }



--
-- Message Types
--


type MsgIn
    = NewPID Int PID
    | ReInit Int
    | SetBg String
    | SetTitle String
    | AddMsg PID Message
    | UrlChanged Url


type MsgOut msgIn
    = Spawn PID (PID -> MsgIn)
    | SetPageTitle String
    | PerformAction PID (Action msgIn)



--
-- Component
--


init : Config msgIn -> PID -> ( DevModel msgIn, List (MsgOut msgIn), Cmd MsgIn )
init config pid =
    ( { pid = pid
      , cases = config.cases
      , pids = Dict.empty
      , displayCase = Nothing
      , messages = Dict.empty
      , bgColor = "#fff"
      , title = ""
      }
    , config.cases
        |> Dict.keys
        |> List.map
            (\idx ->
                Spawn pid (NewPID idx)
            )
    , Cmd.none
    )


kill : DevModel msgIn -> List (MsgOut msgIn)
kill model =
    []


update : Config msgIn -> MsgIn -> DevModel msgIn -> ( DevModel msgIn, List (MsgOut msgIn), Cmd MsgIn )
update config msgIn model =
    case msgIn of
        NewPID idx pid ->
            ( { model | pids = Dict.insert idx (Child pid) model.pids }
            , Dict.get idx config.cases
                |> Maybe.map (\testCase -> List.map (PerformAction pid) testCase.init)
                |> Maybe.withDefault []
            , Cmd.none
            )

        ReInit idx ->
            ( model
            , [ Spawn model.pid (NewPID idx)
              ]
            , Cmd.none
            )

        SetBg col ->
            ( { model | bgColor = col }
            , []
            , Cmd.none
            )

        SetTitle t ->
            ( { model | title = t }
            , []
            , Cmd.none
            )

        AddMsg pid message ->
            ( { model
                | messages =
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
                        model.messages
              }
            , []
            , Cmd.none
            )

        UrlChanged url ->
            case Maybe.map (String.split "/") url.fragment of
                Just [ "testcase", n ] ->
                    let
                        idx =
                            String.toInt n

                        title =
                            idx
                                |> Maybe.andThen (\i -> Dict.get i model.cases)
                                |> Maybe.map .title
                                |> Maybe.withDefault model.title
                    in
                    ( { model | displayCase = idx }
                    , [ SetPageTitle title
                      ]
                    , Cmd.none
                    )

                _ ->
                    ( { model | displayCase = Nothing }
                    , [ SetPageTitle model.title
                      ]
                    , Cmd.none
                    )



-- VIEW


view : Config msgIn -> (MsgIn -> msg) -> DevModel msgIn -> (PID -> Html msg) -> Html msg
view config toSelf model renderPID =
    let
        testCases =
            model.cases
                |> Dict.toList
    in
    Html.div
        []
        [ Html.node "style" [] [ Html.text css ]
        , Html.h1 [] [ Html.text model.title ]
        , Html.div
            []
            [ Html.button [ Events.onClick (toSelf <| SetBg "#fff") ] [ Html.text "BG #fff" ]
            , Html.button [ Events.onClick (toSelf <| SetBg "#eee") ] [ Html.text "BG #eee" ]
            , Html.button [ Events.onClick (toSelf <| SetBg "#444") ] [ Html.text "BG #444" ]
            , Html.pre
                []
                [ Html.div
                    [ HA.style "color" "#061"
                    ]
                    [ Html.text "In Message"
                    ]
                , Html.div
                    [ HA.style "color" "#00a"
                    ]
                    [ Html.text "  -> Out Message"
                    ]
                ]
            ]
        , Html.div
            [
            ]
            (testCases
                |> List.map
                    (\( idx, testCase ) ->
                        Html.div
                            []
                            [ Html.a
                                [ HA.href ("#testcase/" ++ (String.fromInt idx))
                                ]
                                [ Html.text testCase.title
                                ]
                            ]
                    )
                |> (::)
                    ( Html.a
                        [ HA.href "#"
                        ]
                        [ Html.text " -- All test cases --"
                        ]
                    )
            )
        , Html.hr [] []
        , Html.div
            []
            ( model.displayCase
                |> Maybe.andThen
                    (\idx ->
                        Dict.get idx model.cases
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
        ]



renderChild : DevModel m -> (MsgIn -> msg) -> (PID -> Html msg) -> Int -> TestCase m -> Child -> Html msg
renderChild model toSelf renderPID idx testCase child =
    Html.div
        []
        [ Html.h3 [] [ Html.text testCase.title ]
        , Html.p [] [ Html.text testCase.desc ]
        , child.pid
            |> (\(PID _ p) -> "PID: " ++ String.fromInt p)
            |> Html.text
            |> List.singleton
            |> Html.p []
        , Html.button
            [ Events.onClick (toSelf <| ReInit idx)
            ]
            [ Html.text "Reset test"
            ]
        , Html.h4 [] [ Html.text "Component view:" ]
        , Html.div
            [ HA.style "margin" "2em 1em"
            , HA.style "background" model.bgColor
            ]
            [ renderPID child.pid
            ]
        , Html.h4 [] [ Html.text "Message log:" ]
        , Html.pre
            []
            (model.messages
                |> Dict.get (PID.toString child.pid)
                |> Maybe.map
                    (List.reverse
                        >> List.map
                            (\message ->
                                case message of
                                    InMessage inMsg ->
                                        inMsg
                                            |> Html.text
                                            |> List.singleton
                                            |> Html.div
                                                [ HA.style "margin-top" "0.5em"
                                                , HA.style "color" "#061"
                                                ]

                                    OutMessage outMsg ->
                                        "  -> "
                                            ++ outMsg
                                            |> Html.text
                                            |> List.singleton
                                            |> Html.div
                                                [ HA.style "margin-top" "0.1em"
                                                , HA.style "color" "#00a"
                                                ]
                            )
                    )
                |> Maybe.withDefault []
            )
        , Html.hr [] []
        ]


css : String
css =
    """
body {
    background: #eee;
}
"""
