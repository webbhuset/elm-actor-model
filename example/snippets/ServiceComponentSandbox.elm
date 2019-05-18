module ServiceComponentSandbox exposing (..)

import Webbhuset.Component.Sandbox as Sandbox exposing (SandboxProgram)
import Webbhuset.Component as Component
import Html exposing (Html)
import ServiceComponent as ComponentAlias


main : SandboxProgram ComponentAlias.Model ComponentAlias.MsgIn
main =
    Sandbox.service
        { title = "Title of Component"
        , component = ComponentAlias.component ()
        , cases =
            [ test_init
            ]
        , stringifyMsgIn = Debug.toString -- Or roll your own if you want prettier messages.
        , stringifyMsgOut = Debug.toString
        , view = view
        }



{-| Sometimes it is useful to render some internals of your
service component's model.

-}
view : ComponentAlias.Model -> Html ComponentAlias.MsgIn
view model =
    Html.text "Hello"



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


