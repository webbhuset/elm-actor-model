module Webbhuset.Component.Sandbox.Layout exposing (..)

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


type alias Config msgIn msgOut =
    { cases : Dict Int (TestCase msgIn msgOut)
    , stringifyMsgOut : msgOut -> String
    , title : String
    }


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


component : Config i o -> Component.Layout (Model o) (MsgIn i o) (MsgOut i o) msg
component config =
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


type alias Model msgOut =
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


init : Config i o -> PID -> ( Model o, List (MsgOut i o), Cmd (MsgIn i o) )
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


update : Config i o -> (MsgIn i o) -> Model o -> ( Model o, List (MsgOut i o), Cmd (MsgIn i o) )
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


view : Config msgIn msgOut -> ((MsgIn msgIn msgOut) -> msg) -> Model msgOut -> (PID -> Html msg) -> Html msg
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


renderCli : Config i o -> Model o -> { pass : Int, fail : Int, wait : Int } -> String
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
                ++ "\n\nsandbox version: 3"
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


pageHeader : (MsgIn msgIn msgOut -> msg) -> Model msgOut -> ColorConfig -> Html msg
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



fullscreenToggle : (MsgIn msgIn msgOut -> msg) -> Model msgOut -> Int -> Html msg
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


testCaseSelectBox : Config msgIn msgOut -> (MsgIn msgIn msgOut -> msg) -> Model msgOut -> Html msg
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


renderChild : Model o -> ((MsgIn i o) -> msg) -> (PID -> Html msg) -> Int -> TestCase i o -> Child -> Html msg
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
