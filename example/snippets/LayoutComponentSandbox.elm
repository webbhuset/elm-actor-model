module LayoutComponentSandbox exposing (..)

import Webbhuset.Component.Sandbox as Sandbox exposing (SandboxProgram, Action)
import Webbhuset.Component as Component
import Webbhuset.PID as PID
import Html exposing (Html)
import LayoutComponent as ComponentAlias


main : SandboxProgram ComponentAlias.Model ComponentAlias.MsgIn
main =
    Sandbox.layout
        { title = "Title of Component"
        , component = ComponentAlias.component
        , cases =
            [ test_init
            ]
        , stringifyMsgIn = Debug.toString -- Or roll your own if you want prettier messages.
        , stringifyMsgOut = Debug.toString
        }


{-| To test a layout component, convert it to a UI Component by changing the view function.

-}


test_init : Sandbox.TestCase ComponentAlias.MsgIn
test_init =
    Sandbox.TestCase
        "Test case title"
        """
# Describe test case here.

You can use Markdown
        """
        [ Sandbox.sendMsg ComponentAlias.NoIn -- A list of MsgIn to put the tested componet in the right state.
        , Sandbox.spawnLorem ComponentAlias.ReceiveChild
        ]


