module Webbhuset.Component.Sandbox.Layout exposing (..)


import Dict exposing (Dict)
import Url exposing (Url)
import List.Extra as List
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Webbhuset.Component as Component
import Webbhuset.Component.SystemEvent as SystemEvent exposing (SystemEvent)
import Webbhuset.Internal.PID exposing (PID(..))
import Webbhuset.PID as PID
import Markdown

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


type Action msgIn
    = SendMsg msgIn
    | SpawnChild String (PID -> msgIn)
    | Delay Float (Action msgIn)


component : Config i o -> Component.Layout Model (MsgIn i o) (MsgOut i o) msg
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


type alias Model =
    { pid : PID
    , pids : Dict Int Child
    , messages : Dict String (List Message)
    , displayCase : Maybe Int
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


init : Config i o -> PID -> ( Model, List (MsgOut i o), Cmd (MsgIn i o) )
init config pid =
    ( { pid = pid
      , pids = Dict.empty
      , displayCase = Nothing
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


update : Config i o -> (MsgIn i o) -> Model -> ( Model, List (MsgOut i o), Cmd (MsgIn i o) )
update config msgIn model =
    case msgIn of
        NewPID idx pid ->
            let
                (outMsgs, cmds) =
                    Dict.get idx config.cases
                        |> Maybe.map (\testCase -> runActions pid testCase.init)
                        |> Maybe.withDefault ( [], Cmd.none )
            in
            ( { model | pids = Dict.insert idx (Child pid) model.pids }
            , outMsgs
            , cmds
            )

        RunAction pid action ->
            let
                ( msgs, cmds ) =
                    runActions pid [ action ]
            in
            ( model
            , msgs
            , cmds
            )

        HandleMsgOut pid msgOut ->
            let
                msgStr =
                    config.stringifyMsgOut msgOut
                        |> OutMessage

                ( outMsgs, cmds ) =
                    findIdxFromPID pid model.pids
                        |> Maybe.andThen (\idx -> Dict.get idx config.cases)
                        |> Maybe.map
                            (\test ->
                                test.onMsgOut msgOut
                                    |> runActions pid
                            )
                        |> Maybe.withDefault ( [], Cmd.none )
            in
            ( { model
                | messages =
                    Dict.update
                        (PID.toString pid)
                        (\mbMsg ->
                            case mbMsg of
                                Just messages ->
                                    msgStr
                                        :: messages
                                        |> Just

                                Nothing ->
                                    [ msgStr ]
                                        |> Just
                        )
                        model.messages
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



findIdxFromPID : PID -> Dict Int Child -> Maybe Int
findIdxFromPID pid dict =
    dict
        |> Dict.toList
        |> List.find (\( _, child ) -> child.pid == pid)
        |> Maybe.map Tuple.first


runActions : PID -> List (Action i) -> ( List (MsgOut i o), Cmd (MsgIn i o) )
runActions pid actions =
    actions
        |> List.foldl
            (\action ( msgs, cmds ) ->
                case action of
                    Delay delay_ delayedAction ->
                        ( msgs
                        , cmds
                            ++ [ Component.toCmdWithDelay
                                    delay_
                                    (delayedAction
                                        |> RunAction pid
                                    )
                                ]
                        )

                    _ ->
                        ( msgs ++ [ PerformAction pid action ]
                        , cmds
                        )
            )
            ( [], [] )
        |> Tuple.mapSecond Cmd.batch


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


view : Config msgIn msgOut -> ((MsgIn msgIn msgOut) -> msg) -> Model -> (PID -> Html msg) -> Html msg
view config toSelf model renderPID =
    let
        ( currentPath, queryParams ) =
            model.currentUrl
                |> Maybe.map parseUrl
                |> Maybe.withDefault ( [], Dict.empty )

        testCases =
            config.cases
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
            resolve (Dict.get caseIdx config.cases) <| \testCase ->
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
                , testCaseSelectBox config toSelf model
                , Html.div
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
                ]


pageHeader : (MsgIn msgIn msgOut -> msg) -> Model -> ColorConfig -> Html msg
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
            [ Html.text model.title ]
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



fullscreenToggle : (MsgIn msgIn msgOut -> msg) -> Model -> Int -> Html msg
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


testCaseSelectBox : Config msgIn msgOut -> (MsgIn msgIn msgOut -> msg) -> Model -> Html msg
testCaseSelectBox config toSelf model =
    let
        testCases =
            config.cases
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


renderChild : Model -> ((MsgIn i o) -> msg) -> (PID -> Html msg) -> Int -> TestCase i o -> Child -> Html msg
renderChild model toSelf renderPID idx testCase child =
    Html.div
        [ HA.class "ams-testcase"
        ]
        [ Html.div
            [ HA.class "ams-testcase__toolbar"
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
    }
    .ams-testcase__componentview {
        border: 1px solid #e7e7e7;
        border-radius: 4px;
        background: {{componentBg}};
    }

    .ams-testcase__toolbar {
        display: flex;
        flex-direction: row;
        justify-content: flex-end;
        align-items: center;
        border-bottom: 1px solid #f6f6f6;
        padding: 0.5rem 0;
        background: #f6f6f6;
        border-top-left-radius: 4px;
        border-top-right-radius: 4px;
    }
    .ams-testcase__toolbar > * {
        margin-right: 1rem;
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
