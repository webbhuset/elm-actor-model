module Html.UIComponentSandbox exposing (..)

import Webbhuset.Component.Sandbox as Sandbox exposing (SandboxProgram)
import Webbhuset.PID as PID
import Html.UIComponent as ComponentAlias
import Html exposing (Html)
import Html.Attributes as HA


main : SandboxProgram ComponentAlias.Model ComponentAlias.MsgIn
main =
    Sandbox.ui
        { title = "UI Component"
        , component = ComponentAlias.component
        , cases =
            [ test_init
            ]
        , stringifyMsgIn = Debug.toString -- Or roll your own if you want prettier messages.
        , stringifyMsgOut = Debug.toString
        , wrapView = view
        }

{-| You can wrap the output of your component.

This is useful when you want to add CSS style or some extra test buttons.
-}
view : Html ComponentAlias.MsgIn -> Html ComponentAlias.MsgIn
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
}
"""


test_init : Sandbox.TestCase ComponentAlias.MsgIn
test_init =
    Sandbox.TestCase
        "Test case title"
        """
# Describe test case here.

You can use Markdown
        """
        [ Sandbox.sendMsg ComponentAlias.NoIn -- A list of MsgIn to put the tested componet in the right state.
        ]
