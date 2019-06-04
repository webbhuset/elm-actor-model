module Html.UIComponentSandbox exposing (..)

import Webbhuset.Component.Sandbox as Sandbox exposing (SandboxProgram)
import Webbhuset.PID as PID
import AssertExample1 as Example
import Html exposing (Html)
import Html.Attributes as HA


main : SandboxProgram Example.Model Example.MsgIn Example.MsgOut
main =
    Sandbox.ui
        { title = "Examples of Asserts"
        , component = Example.component
        , cases =
            [ test_click
            , test_timeout
            , test_timeout2
            , test_timeout3
            , test_pid
            ]
        , stringifyMsgIn = Debug.toString
        , stringifyMsgOut = Debug.toString
        , wrapView = view
        }

view : Html Example.MsgIn -> Html Example.MsgIn
view componentHtml =
    Html.div
        [ HA.class "component"
        ]
        [ Html.node "style" [] [ Html.text css ]
        , componentHtml
        ]


css : String
css =
    """
.component {
    font-family: monospace;
    padding: 1rem;
}
"""


test_click : Sandbox.TestCase Example.MsgIn Example.MsgOut
test_click =
    { title = "Assert out messages"
    , desc =
    """
Click the "Good Button" until the level becomes "Really good" for this test to pass.

Levels are:
- This sucks!!!
- Bad
- Not that bad
- Acceptable
- Good
- Really Good
- Too good

    """
    , init =
        [
        ]
    , onMsgOut = \outMsg ->
        case outMsg of
            Example.SomethingBad ->
                [ Sandbox.fail "You ruined it...."
                ]

            Example.SomethingGood 4 ->
                [ Sandbox.pass
                ]

            Example.SomethingGood 5 ->
                [ Sandbox.fail "Now it was too good, don't get carried away."
                ]

            _ ->
                [ ]
    }

test_timeout : Sandbox.TestCase Example.MsgIn Example.MsgOut
test_timeout =
    { title = "Timeout"
    , desc =
    """
Once you clicked "Good Button" you have one second to reach "Acceptable".
Try waiting and the test will fail.
    """
    , init =
        [
        ]
    , onMsgOut = \outMsg ->
        case outMsg of
            Example.SomethingBad ->
                [ Sandbox.fail "You ruined it...."
                ]

            Example.SomethingGood 1 ->
                [ Sandbox.timeout 1000
                ]

            Example.SomethingGood 2 ->
                [ Sandbox.pass
                ]

            _ ->
                [ Sandbox.fail "Don't get carried away."
                ]
    }


test_timeout2 : Sandbox.TestCase Example.MsgIn Example.MsgOut
test_timeout2 =
    { title = "Timeout on next update"
    , desc =
    """
If you set the timeout to zero the test will timeout on next animation frame.
This has to do with how Elm batches updates and synchronizes them with
`requestAniamtionFrame`.

Since `GoodButtonClicked` is sent directly from init this test passes.
    """
    , init =
        [ Sandbox.timeout 0
        , Sandbox.sendMsg Example.GoodButtonClicked
        , Sandbox.sendMsg Example.GoodButtonClicked
        ]
    , onMsgOut = \outMsg ->
        case outMsg of
            Example.SomethingBad ->
                [ Sandbox.fail "You ruined it...."
                ]

            Example.SomethingGood 2 ->
                [ Sandbox.pass
                ]

            _ ->
                [ ]
    }


test_timeout3 : Sandbox.TestCase Example.MsgIn Example.MsgOut
test_timeout3 =
    { title = "Timeout is unpredictable"
    , desc =
    """
Here, the timeout is set to 30ms. Two messages are sent in a sequence with
10ms delay for each. Sometimes this test passes and sometimes it fails, try
reloading the test multiple times. This is
due to Elm's internals (I think) trying to syncronize updates with request
animation frame.
    """
    , init =
        [ Sandbox.timeout 30
        , Sandbox.sendMsg Example.GoodButtonClicked
            |> Sandbox.delay 10
        ]
    , onMsgOut = \outMsg ->
        case outMsg of
            Example.SomethingBad ->
                [ Sandbox.fail "You ruined it...."
                ]

            Example.SomethingGood 1 ->
                [ Sandbox.sendMsg Example.GoodButtonClicked
                    |> Sandbox.delay 10
                ]

            Example.SomethingGood 2 ->
                [ Sandbox.pass
                ]

            _ ->
                [ ]
    }


test_pid : Sandbox.TestCase Example.MsgIn Example.MsgOut
test_pid =
    let
        fakePID =
            Sandbox.mockPID "a-component"

        fakePID2 =
            Sandbox.mockPID "another-component"
    in
    { title = "Assert PIDs"
    , desc =
    """
You can assert PIDs when using with the observer pattern.

Check that both observers receive the string "new thing".
    """
    , init =
        [ Sandbox.timeout 30
        , Example.ObserveSomething fakePID
            |> Sandbox.sendMsg
        , Example.ObserveSomething fakePID2
            |> Sandbox.sendMsg
        , Example.ChangeSomething "new thing"
            |> Sandbox.sendMsg
        ]
    , onMsgOut = \outMsg ->
        case outMsg of
            Example.SomethingWasChanged pids thing ->
                if List.any ((==) fakePID) pids
                    && List.any ((==) fakePID2) pids
                    && thing == "new thing"
                then
                    [ Sandbox.pass
                    ]
                else
                    []

            _ ->
                [ ]
    }
