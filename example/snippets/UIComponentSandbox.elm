module UIComponentSandbox exposing (..)

import Webbhuset.Component.Sandbox as Sandbox exposing (SandboxProgram)
import Webbhuset.PID as PID
import UIComponent as ComponentAlias


main : SandboxProgram ComponentAlias.Model ComponentAlias.MsgIn
main =
    Sandbox.ui
        { title = "Title of component"
        , component = ComponentAlias.component
        , cases =
            [ test_init
            ]
        , stringifyMsgIn = Debug.toString -- Or roll your own if you want prettier messages.
        , stringifyMsgOut = Debug.toString
        }



test_init : Sandbox.TestCase ComponentAlias.MsgIn
test_init =
    Sandbox.TestCase
        "Test case title"
        """
# Describe test case here.

You can use Markdown
        """
        [ ComponentAlias.NoIn -- A list of MsgIn to put the tested componet in the right state.
        ]
