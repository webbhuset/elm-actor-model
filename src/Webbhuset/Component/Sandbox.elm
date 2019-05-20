module Webbhuset.Component.Sandbox exposing
    ( SandboxProgram
    , TestCase
    , ui
    , layout
    , elmUILayout
    , service
    , sendMsg
    , sendMsgWithDelay
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


@docs ui, layout, service, elmUILayout

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

@docs sendMsg, spawnChild, sendMsgWithDelay

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Element exposing (Element)
import Webbhuset.Actor as Actor exposing (Actor)
import Webbhuset.ActorSystem as System
import Webbhuset.Component as Component
import Webbhuset.Component.ElmUI as ElmUI_Component
import Webbhuset.Actor.ElmUI as ElmUI_Actor
import Webbhuset.Component.Navigation as Navigation
import Webbhuset.Component.LoremIpsum as LoremIpsum
import Webbhuset.Internal.PID exposing (PID(..))
import Webbhuset.Internal.Component as InternalC
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
    | SendMsgWithDelay Float msgIn
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

    Sandbox.sendMsg YourComponent.SomeMessage

-}
sendMsg : msgIn -> Action msgIn
sendMsg =
    SendMsg


{-| Send a delayed message to you sandboxed component. Delay in
milli-seconds.

    Sandbox.sendMsgWithDelay 1000 YourComponent.SomeMessage

-}
sendMsgWithDelay : Float -> msgIn -> Action msgIn
sendMsgWithDelay =
    SendMsgWithDelay


{-| Sandbox a UI Component

-}
ui :
    { title : String
    , component : InternalC.UI model msgIn msgOut output
    , cases : List (TestCase msgIn)
    , stringifyMsgIn : msgIn -> String
    , stringifyMsgOut : msgOut -> String
    , wrapView : output -> Html msgIn
    }
    -> SandboxProgram model msgIn
ui ({ component } as args) =
    Actor.fromUI
        { wrapModel = P_Component
        , wrapMsg = ComponentMsg
        , mapIn = testedMapIn
        , mapOut = testedMapOut args.stringifyMsgOut
        }
        { init = component.init
        , update = component.update
        , kill = component.kill
        , subs = component.subs
        , view = component.view >> args.wrapView
        }
        |> toApplication args


{-| Sandbox a Layout Component

-}
layout :
    { title : String
    , component : Component.Layout model msgIn msgOut (Msg msgIn)
    , cases : List (TestCase msgIn)
    , stringifyMsgIn : msgIn -> String
    , stringifyMsgOut : msgOut -> String
    , wrapView : (msgIn -> Msg msgIn) -> Html (Msg msgIn) -> Html (Msg msgIn)
    }
    -> SandboxProgram model msgIn
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
        |> toApplication args


{-| Sandbox a Elm UI Layout Component

-}
elmUILayout :
    { title : String
    , component : ElmUI_Component.Layout model msgIn msgOut (Msg msgIn)
    , cases : List (TestCase msgIn)
    , stringifyMsgIn : msgIn -> String
    , stringifyMsgOut : msgOut -> String
    , wrapView : (msgIn -> Msg msgIn) -> Element (Msg msgIn) -> Html (Msg msgIn)
    }
    -> SandboxProgram model msgIn
elmUILayout ({ component } as args) =
    Actor.fromLayout
        { wrapModel = P_Component
        , wrapMsg = ComponentMsg
        , mapIn = testedMapIn
        , mapOut = testedMapOut args.stringifyMsgOut
        }
        { init = component.init
        , update = component.update
        , kill = component.kill
        , subs = component.subs
        , view = \toSelf model renderPID ->
            (component.view toSelf model (Element.paragraph [] << List.singleton << Element.html << renderPID))
                |> args.wrapView toSelf
        }
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
    = DevMsg (MsgIn msgIn)
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
            .init (sandboxActor toString cases)

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
            System.applyModel (sandboxActor toString cases) model

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


sandboxActor : (msgIn -> String) -> List (TestCase msgIn) -> Actor (DevModel msgIn) (Process model msgIn) (Msg msgIn)
sandboxActor toString cases =
    Actor.fromLayout
        { wrapModel = P_Dev
        , wrapMsg = DevMsg
        , mapIn = mapIn
        , mapOut = mapOut toString
        }
        (sandboxComponent
            { cases =
                List.indexedMap Tuple.pair cases
                    |> Dict.fromList
            }
        )


mapIn : AppMsg msgIn -> Maybe (MsgIn msgIn)
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

                SendMsgWithDelay delay msgIn ->
                    System.none -- handled in sandbox component

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

        NavigateToHref href ->
            Navigation.Push href
                |> NavMsg
                |> System.toAppMsg
                |> System.sendToSingleton Navigation



-- Test Runner Component

type alias Config msgIn =
    { cases : Dict Int (TestCase msgIn)
    }


sandboxComponent : Config msgIn -> Component.Layout (DevModel msgIn) (MsgIn msgIn) (MsgOut msgIn) msg
sandboxComponent config =
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
    , testCaseBgColor : String
    , componentViewBgColor : String
    , title : String
    , currentUrl : Maybe Url
    }



--
-- Message Types
--


type MsgIn msgIn
    = NewPID Int PID
    | ReInit Int
    | SetTitle String
    | AddMsg PID Message
    | UrlChanged Url
    | ForwardMsg (MsgOut msgIn)
    | BgColorPicked String
    | TestCaseBgColorPicked String
    | ComponentBgColorPicked String
    | NavigateTo String


type MsgOut msgIn
    = Spawn PID (PID -> MsgIn msgIn)
    | SetPageTitle String
    | PerformAction PID (Action msgIn)
    | NavigateToHref String



--
-- Component
--


init : Config msgIn -> PID -> ( DevModel msgIn, List (MsgOut msgIn), Cmd (MsgIn msgIn) )
init config pid =
    ( { pid = pid
      , cases = config.cases
      , pids = Dict.empty
      , displayCase = Nothing
      , messages = Dict.empty
      , bgColor = "#333"
      , testCaseBgColor = "#fff"
      , componentViewBgColor = "transparent"
      , title = ""
      , currentUrl = Nothing
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


update : Config msgIn -> (MsgIn msgIn) -> DevModel msgIn -> ( DevModel msgIn, List (MsgOut msgIn), Cmd (MsgIn msgIn) )
update config msgIn model =
    case msgIn of
        NewPID idx pid ->
            let
                (outMsgs, cmds) =
                    Dict.get idx config.cases
                        |> Maybe.map
                            (\testCase ->
                                testCase.init
                                    |> List.partition
                                        (\action ->
                                            case action of
                                                SendMsgWithDelay _ _ ->
                                                    False

                                                _ ->
                                                    True
                                        )
                                    |> Tuple.mapFirst (List.map (PerformAction pid))
                                    |> Tuple.mapSecond
                                        (List.filterMap
                                            (\action ->
                                                case action of
                                                    SendMsgWithDelay delay msg ->
                                                        Component.toCmdWithDelay
                                                            delay
                                                            (SendMsg msg
                                                                |> PerformAction pid
                                                                |> ForwardMsg
                                                            )
                                                            |> Just

                                                    _ ->
                                                        Nothing
                                            )
                                            >> Cmd.batch
                                        )
                            )
                        |> Maybe.withDefault ( [], Cmd.none )
            in
            ( { model | pids = Dict.insert idx (Child pid) model.pids }
            , outMsgs
            , cmds
            )

        ForwardMsg msgOut ->
            ( model
            , [ msgOut ]
            , Cmd.none
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
            let
                ( path, query ) =
                    parseUrl url

                strParam key default =
                    Dict.get key query
                        |> Maybe.withDefault default

                title =
                    idx
                        |> Maybe.andThen (\i -> Dict.get i model.cases)
                        |> Maybe.map .title
                        |> Maybe.withDefault model.title

            in
            case path of
                [ "testcase", n ] ->
                    ( { model
                            | displayCase = String.toInt n
                            , currentUrl = Just url
                      }
                    , [ SetPageTitle title ]
                    , Cmd.none
                    )

                _ ->
                    ( { model
                            | displayCase = Nothing
                            , currentUrl = Just url
                      }
                    , [ SetPageTitle model.title ]
                    , Cmd.none
                    )

        NavigateTo href ->
            ( model
            , [ NavigateToHref href ]
            , Cmd.none
            )


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




-- VIEW


type alias ColorConfig =
    { bgColor : String
    , testCaseBgColor : String
    , componentBgColor : String
    }


colorsFromQueryParams : Dict String String -> ColorConfig
colorsFromQueryParams queryParams =
    { bgColor =
        Dict.get "bgColor" queryParams
            |> Maybe.withDefault "#888"
    , testCaseBgColor =
        Dict.get "testCaseBgColor" queryParams
            |> Maybe.withDefault "#fff"
    , componentBgColor =
        Dict.get "componentBgColor" queryParams
            |> Maybe.withDefault "transparent"
    }


view : Config msgIn -> ((MsgIn msgIn) -> msg) -> DevModel msgIn -> (PID -> Html msg) -> Html msg
view config toSelf model renderPID =
    let
        ( currentPath, queryParams ) =
            model.currentUrl
                |> Maybe.map parseUrl
                |> Maybe.withDefault ( [], Dict.empty )

        testCases =
            model.cases
                |> Dict.toList

        color =
            colorsFromQueryParams queryParams


        fullScreenCase =
            let
                resolve maybe fn =
                    maybe
                        |> Maybe.andThen fn
            in
            resolve (Dict.get "fullscreen" queryParams) <|\_ -> 
            resolve model.displayCase <| \caseIdx ->
            resolve (Dict.get caseIdx model.cases) <| \testCase ->
            resolve (Dict.get caseIdx model.pids) <| \child ->
                Just ( caseIdx, testCase, child )
    in
    case fullScreenCase of
        Just ( idx, testCase, child ) ->
            renderPID child.pid

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
                , testCaseSelectBox toSelf model
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


pageHeader : (MsgIn -> msg) -> DevModel msgIn -> ColorConfig -> Html msg
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
    in
    Html.div
        [ HA.class "ams-page-header"
        ]
        [ Html.h1
            [ HA.class "ams-pagetitle"
            ]
            [ Html.text model.title ]
        , colorInput
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
        ]



fullscreenToggle : (MsgIn -> msg) -> DevModel msgIn -> Int -> Html msg
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


testCaseSelectBox : (MsgIn -> msg) -> DevModel msgIn -> Html msg
testCaseSelectBox toSelf model =
    let
        testCases =
            model.cases
                |> Dict.toList

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
            (testCases
                |> List.map
                    (\( idx, testCase ) ->
                        Html.option
                            [ HA.value <| buildHref [ "testcase", String.fromInt idx ] queryParams
                            , if model.displayCase == Just idx then
                                HA.selected True
                              else
                                HA.selected False
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


renderChild : DevModel m -> ((MsgIn msgIn) -> msg) -> (PID -> Html msg) -> Int -> TestCase m -> Child -> Html msg
renderChild model toSelf renderPID idx testCase child =
    Html.div
        [ HA.class "ams-testcase"
        ]
        [ Html.h2 
            [ HA.class "ams-testcase__title"
            ]
            [ Html.text testCase.title
            ]
        , Html.p
            [ HA.class "ams-testcase__desc"
            ]
            [ Html.text testCase.desc
            ]
        , Html.div
            [ HA.class "ams-cornerarea"
            ]
            [ child.pid
                |> (\(PID _ p) -> "PID: " ++ String.fromInt p)
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

    .ams-button {
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

    .ams-button:hover {
        background: #555;
        font-family: sans-serif;
    }

    .ams-colorinput__row {
    }
    .ams-colorinput__label {
        padding: 0 1rem;
        color: #fff;
        font-family: monospace;
    }

    /** Test Case **/
    .ams-testcase {
        border-radius: 4px;
        padding: 1rem;
        margin: 1rem 0;
        background: {{testCaseBg}};
        position: relative;
    }
    .ams-testcase > *:last-child {
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
    }
    .ams-testcase__componentview {
        border: 1px solid #e7e7e7;
        border-radius: 4px;
        background: {{componentBg}};
    }

    .ams-cornerarea {
        position: absolute;
        top: 0;
        right: 0;
        padding: 0.5rem 1rem;
    }
    .ams-cornerarea > * {
        margin-left: 1rem;
    }
    .ams-testcase__pidLabel {
        font-family: monospace;
        font-size: 0.8rem;
        color: #333;
    }

    /** Message Log **/
    .ams-messagelog {
        border-radius: 4px;
        background: #eee;
        padding: 0.5rem 1rem;
        min-height: 24;
        margin: 0;
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
