module Html.LayoutComponentSandbox exposing (..)

import Webbhuset.Component.Sandbox as Sandbox exposing (SandboxProgram)
import Webbhuset.PID as PID
import Html exposing (Html)
import Html.Attributes as HA
import Html.LayoutComponent as ComponentAlias


main : SandboxProgram ComponentAlias.Model ComponentAlias.MsgIn
main =
    Sandbox.layout
        { title = "Layout Component"
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
view : (ComponentAlias.MsgIn -> msg) -> Html msg -> Html msg
view toSelf componentHtml =
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
        , Sandbox.spawnChild "Hello child" ComponentAlias.ReceiveChild
        ]


