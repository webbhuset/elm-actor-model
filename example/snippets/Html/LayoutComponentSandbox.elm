module Html.LayoutComponentSandbox exposing (..)

import Webbhuset.Component.Sandbox as Sandbox exposing (SandboxProgram)
import Webbhuset.PID as PID
import Html exposing (Html)
import Html.Attributes as HA
import Html.LayoutComponent as ComponentAlias


type alias Model = ComponentAlias.Model
type alias MsgIn = ComponentAlias.MsgIn
type alias MsgOut = ComponentAlias.MsgOut


main : SandboxProgram Model MsgIn MsgOut
main =
    Sandbox.layout
        { title = "Layout Component"
        , component = ComponentAlias.component
        , cases =
            [ test_init
            , test_init
            ]
        , stringifyMsgIn = Debug.toString -- Or roll your own if you want prettier messages.
        , stringifyMsgOut = Debug.toString
        , wrapView = view
        }


{-| You can wrap the output of your component.

This is useful when you want to add CSS style or some extra test buttons.
-}
view : (MsgIn -> msg) -> Html msg -> Html msg
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



test_init : Sandbox.TestCase MsgIn MsgOut
test_init =
    { title = "Test case title"
    , desc =
    """
# Describe test case here.

You can use Markdown
    """
    , init =
        [ Sandbox.sendMsg (ComponentAlias.Show "Hej")
            |> Sandbox.delay 500
        ]
    , onMsgOut = \msgOut ->
        case msgOut of
            ComponentAlias.SpawnRendererFor str ->
                [ Sandbox.spawnChild str ComponentAlias.ReceiveChild
                    |> Sandbox.delay 500
                ]
    }


