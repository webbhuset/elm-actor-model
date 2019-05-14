module Webbhuset.Component.Sandbox exposing
    ( SandboxProgram
    , TestCase
    , ui
    )

{-|

# Sandbox

The sandbox module is helpful when developing components. It lets you
run the component using `elm reactor` outside the system and define several test cases.

@docs SandboxProgram

@docs TestCase

## Sandbox UI Components

@docs ui

You can also use the ui sandbox on a Service component.
Just add a `view` function.

    { init = service.init
    , update = service.update
    , kill = service.kill
    , subs = service.subs
    , view = view
    }

    view : Model -> Html MsgIn
    view model =
        Html.text "Service Component"
-}

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Webbhuset.Actor as Actor exposing (Actor)
import Webbhuset.ActorSystem as System
import Webbhuset.Component as Component
import Webbhuset.Component.Navigation as Navigation
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

Each test case contains a list of in-messages that will be sent to the component
when the sandbox is started.
-}
type alias TestCase msgIn =
    { title : String
    , desc : String
    , init : List msgIn
    }



{-| Spawnable componentes
-}
type Spawnable
    = LoremIpsum


{-| Wrap a component in a sandbox application.

This will render each test case and log all messages.
Create a test file with a `main` function where you declare all
test cases.

    import Component.Form as Form
    import Webbhuset.Component.Sandbox as Sandbox exposing (SandboxProgram)

    main : SandboxProgram Form.Model Form.MsgIn
    main =
        Sandbox.ui
            Form.component
            "Form Component"
            [ Sandbox.TestCase
                "Empty form"
                "Submit-button should be disabled"
                [
                ]
            , Sandbox.TestCase
                "Form with Errors"
                "You should see an error message"
                [ Form.ErrorMsg "Error message"
                ]
            ]

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
    let
        testedActor =
            Actor.fromUI
                { wrapModel = P_Component
                , wrapMsg = ComponentMsg
                , mapIn = testedMapIn
                , mapOut = testedMapOut args.stringifyMsgOut
                }
                args.component
    in
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


type Process model msgIn
    = P_Dev (DevModel msgIn)
    | P_Component model
    | P_Nav Navigation.Model


type alias Msg msgIn =
    System.SysMsg ActorName (AppMsg msgIn)


type AppMsg msgIn
    = DevMsg MsgIn
    | ComponentMsg msgIn
    | NavMsg Navigation.MsgIn


spawn : (msgIn -> String)
    -> List (TestCase msgIn)
    -> Actor model (Process model msgIn) (Msg msgIn)
    -> ActorName
    -> PID
    -> ( Process model msgIn, Msg msgIn )
spawn toString tests tested name =
    case name of
        DevActor ->
            .init (actor toString tests)

        TestedActor ->
            tested.init

        Navigation ->
            navigation.init


applyModel :
    (msgIn -> String)
    -> List (TestCase msgIn)
    -> Actor model (Process model msgIn) (Msg msgIn)
    -> Process model msgIn
    -> System.AppliedActor (Process model msgIn) (Html (Msg msgIn)) (Msg msgIn)
applyModel toString tests testedActor process =
    case process of
        P_Dev model ->
            System.applyModel (actor toString tests) model

        P_Component model ->
            System.applyModel testedActor model

        P_Nav model ->
            System.applyModel navigation model


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
actor toString tests =
    Actor.fromLayout
        { wrapModel = P_Dev
        , wrapMsg = DevMsg
        , mapIn = mapIn
        , mapOut = mapOut toString
        }
        (component tests)


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

        SendTo pid msg ->
            System.batch
                [ toString msg
                    |> InMessage
                    |> AddMsg pid
                    |> DevMsg
                    |> System.toAppMsg
                    |> System.sendToSingleton DevActor
                , ComponentMsg msg
                    |> System.toAppMsg
                    |> System.sendToPID pid
                ]

        SetPageTitle title ->
            System.setDocumentTitle title



-- Test Runner Component


component : List (TestCase msgIn) -> Component.Layout (DevModel msgIn) MsgIn (MsgOut msgIn) msg
component tests =
    { init = init tests
    , update = update
    , view = view
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
    | SendTo PID msgIn
    | SetPageTitle String



--
-- Component
--


init : List (TestCase msgIn) -> PID -> ( DevModel msgIn, List (MsgOut msgIn), Cmd MsgIn )
init cases pid =
    ( { pid = pid
      , cases =
            List.indexedMap (\idx test -> ( idx, test )) cases
                |> Dict.fromList
      , pids = Dict.empty
      , displayCase = Nothing
      , messages = Dict.empty
      , bgColor = "#fff"
      , title = ""
      }
    , cases
        |> List.indexedMap
            (\idx child ->
                Spawn pid (NewPID idx)
            )
    , Cmd.none
    )


kill : DevModel msgIn -> List (MsgOut msgIn)
kill model =
    []


update : MsgIn -> DevModel msgIn -> ( DevModel msgIn, List (MsgOut msgIn), Cmd MsgIn )
update msgIn model =
    case msgIn of
        NewPID idx pid ->
            ( { model | pids = Dict.insert idx (Child pid) model.pids }
            , Dict.get idx model.cases
                |> Maybe.map
                    (\test ->
                        List.map (SendTo pid) test.init
                    )
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


view : (MsgIn -> msg) -> DevModel msgIn -> (PID -> Html msg) -> Html msg
view toSelf model renderPID =
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
