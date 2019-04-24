module Webbhuset.Component.Dev exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Webbhuset.Component as Component exposing (Container, PID)
import Webbhuset.ActorSystem as System
import Webbhuset.Ctrl as Ctrl
import Dict exposing (Dict)

type alias Model m msgIn = System.Model Component (Process m msgIn)


type alias TestProgram m msgIn =
    Program () (Model m msgIn) (Msg msgIn)


testUI : Component.UI m msgIn msgOut -> String -> List (TestCase msgIn) -> TestProgram m msgIn
testUI ui title cases =
    let
        tested =
            Component.wrapUI
                P_Component
                ComponentMsg
                testedMapIn
                testedMapOut
                ui
    in
    System.element
        { init = initApp title
        , spawn = spawn cases tested
        , apply = applyModel cases tested
        }


initApp : String -> Msg msgIn
initApp title =
    Ctrl.batch
        [ Ctrl.spawn DevComponent
            ( \pid ->
                Ctrl.batch
                    [ Ctrl.addView pid
                    , Ctrl.sendToPID pid
                        ( Ctrl.msgTo <| DevMsg <| SetTitle title )
                    ]
            )
        ]


testedMapIn : Msg msgIn -> Maybe msgIn
testedMapIn globalMsg =
    case globalMsg of
        Ctrl.MsgTo (ComponentMsg msg) ->
            Just msg

        _ ->
            Nothing


testedMapOut : msgOut -> Msg msgIn
testedMapOut componentMsg =
    let
        _ = Debug.log "MsgOut" componentMsg
    in
    Ctrl.none


-- SYSTEM

type Component
    = DevComponent
    | TestedComponent


type Process model msgIn
    = P_Dev (DevModel msgIn)
    | P_Component model


type alias Msg msgIn = System.Msg Component (MsgTo msgIn)


type MsgTo msgIn
    = DevMsg MsgIn
    | ComponentMsg msgIn


spawn : List (TestCase msgIn) -> Container model (Process model msgIn) (Msg msgIn) -> Component -> PID -> ( Process model msgIn, Msg msgIn )
spawn tests tested name =
    case name of
        DevComponent ->
            .init (container tests)

        TestedComponent ->
            tested.init


applyModel : List (TestCase msgIn) -> Container model (Process model msgIn) (Msg msgIn) -> Process model msgIn-> System.AppliedContainer (Process model msgIn) (Msg msgIn)
applyModel tests tested process =
    case process of
        P_Dev model ->
            Component.applyModel (container tests) model

        P_Component model ->
            Component.applyModel tested model


-- CONTAINER


container : List (TestCase msgIn) -> Container (DevModel msgIn) (Process model msgIn) (Msg msgIn)
container tests =
    Component.wrapLayout
        P_Dev
        DevMsg
        mapIn
        mapOut
        (component tests)


mapIn : Msg msgIn -> Maybe MsgIn
mapIn globalMsg =
    case globalMsg of
        Ctrl.MsgTo (DevMsg msg) ->
            Just msg

        _ ->
            Nothing


mapOut : MsgOut msgIn -> (Msg msgIn)
mapOut componentMsg =
    case componentMsg of
        Spawn replyPID reply ->
            Ctrl.spawn TestedComponent
                ( \pid -> Ctrl.sendToPID replyPID (Ctrl.msgTo (DevMsg <| reply pid)))

        SendTo pid msg ->
            Ctrl.sendToPID pid (Ctrl.msgTo (ComponentMsg msg))

-- COMPONENT

component : List (TestCase msgIn) -> Component.Layout (DevModel msgIn) MsgIn (MsgOut msgIn) msg
component tests =
    { init = init tests
    , recv = recv
    , view = view
    , kill = kill
    , subs = always Sub.none
    }


type alias TestCase msgIn =
    { title : String
    , desc : String
    , init : List msgIn
    }


type alias DevModel msgIn =
    { pid : PID
    , cases : Dict Int (TestCase msgIn)
    , pids : Dict Int PID
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


type MsgOut msgIn
    = Spawn PID (PID -> MsgIn)
    | SendTo PID msgIn

--
-- Component
--

init : List (TestCase msgIn) -> PID -> ( DevModel msgIn , List (MsgOut msgIn) , Cmd MsgIn )
init cases pid =
    (
        { pid = pid
        , cases =
            List.indexedMap (\idx test -> (idx, test)) cases
                |> Dict.fromList
        , pids = Dict.empty
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


recv : MsgIn -> DevModel msgIn -> ( DevModel msgIn , List (MsgOut msgIn), Cmd MsgIn )
recv msgIn model =
    case msgIn of
        NewPID idx pid ->
            ( { model | pids = Dict.insert idx pid model.pids }
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
            , [ ]
            , Cmd.none
            )

        SetTitle t ->
            ( { model | title = t }
            , [ ]
            , Cmd.none
            )

-- VIEW

view : (MsgIn -> msg) -> DevModel msgIn -> (PID -> Html msg) -> Html msg
view toSelf model renderPID =
    Html.div
        [
        ]
        [ Html.node "style" [] [ Html.text css ]
        , Html.h1 [] [ Html.text model.title ]
        , Html.div
            []
            [ Html.button [ Events.onClick (toSelf <| SetBg "#fff") ] [ Html.text "BG #fff" ]
            , Html.button [ Events.onClick (toSelf <| SetBg "#eee") ] [ Html.text "BG #eee" ]
            , Html.button [ Events.onClick (toSelf <| SetBg "#444") ] [ Html.text "BG #444" ]
            ]
        , Html.hr [] []
        , Html.div
            []
            ( model.cases
                |> Dict.toList
                |> List.map
                    (\(idx, testCase) ->
                        let
                            child =
                                Dict.get idx model.pids
                        in
                        Html.div
                            []
                            [ Html.h3 [] [ Html.text testCase.title ]
                            , Html.p [] [ Html.text testCase.desc ]
                            , Html.button
                                [ Events.onClick (toSelf <| ReInit idx)
                                ]
                                [ Html.text "Reset test"
                                ]
                            , Html.div
                                [ HA.style "margin" "2em 1em"
                                , HA.style "background" model.bgColor
                                ]
                                [ Maybe.map renderPID child
                                    |> Maybe.withDefault (Html.text "Not started")
                                ]
                            , Html.hr [] []
                            ]
                    )
            )

        ]


css : String
css = """
body {
    background: #eee;
}
"""
