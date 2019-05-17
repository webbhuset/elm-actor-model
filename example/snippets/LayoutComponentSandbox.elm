module LayoutComponentSandbox exposing (..)

import Webbhuset.Component.Sandbox as Sandbox exposing (SandboxProgram)
import Webbhuset.Component as Component
import Webbhuset.PID as PID
import Html exposing (Html)
import LayoutComponent as ComponentAlias


main : SandboxProgram ComponentAlias.Model ComponentAlias.MsgIn
main =
    Sandbox.ui
        { title = "Title of Component"
        , component = component
        , cases =
            [ test_init
            ]
        , stringifyMsgIn = Debug.toString -- Or roll your own if you want prettier messages.
        , stringifyMsgOut = Debug.toString
        }


{-| To test a layout component, convert it to a UI Component by changing the view function.

-}
component : Component.UI ComponentAlias.Model ComponentAlias.MsgIn ComponentAlias.MsgOut
component =
    let
        testedComponent =
            ComponentAlias.component
    in
    { init = testedComponent.init
    , update = testedComponent.update
    , kill = testedComponent.kill
    , subs = testedComponent.subs
    , view = \model -> testedComponent.view identity model (PID.toString >> Html.text)
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


