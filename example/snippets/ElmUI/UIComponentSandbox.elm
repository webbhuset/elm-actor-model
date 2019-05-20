module ElmUI.UIComponentSandbox exposing (..)

import Webbhuset.Component.Sandbox as Sandbox exposing (SandboxProgram)
import Webbhuset.PID as PID
import ElmUI.UIComponent as ComponentAlias
import Html exposing (Html)
import Element exposing (Element)


main : SandboxProgram ComponentAlias.Model ComponentAlias.MsgIn ComponentAlias.MsgOut
main =
    Sandbox.ui
        { title = "Elm UI UI Component"
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
view : Element ComponentAlias.MsgIn -> Html ComponentAlias.MsgIn
view componentHtml =
    componentHtml
        |> Element.layout []



test_init : Sandbox.TestCase ComponentAlias.MsgIn ComponentAlias.MsgOut
test_init =
    { title = "Test case title"
    , desc =
        """
# Describe test case here.

You can use Markdown
        """
    , init =
        [ Sandbox.sendMsg ComponentAlias.NoIn -- A list of MsgIn to put the tested componet in the right state.
        ]
    , onMsgOut = always []
    }
