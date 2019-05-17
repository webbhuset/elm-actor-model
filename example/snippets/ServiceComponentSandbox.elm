module ServiceComponentSandbox exposing (..)

import Webbhuset.Component.Sandbox as Sandbox exposing (SandboxProgram)
import Webbhuset.Component as Component
import Html exposing (Html)
import ServiceComponent as ComponentAlias


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


{-| To test a service component, convert it to a UI Component by adding a view function.

-}
component : Component.UI ComponentAlias.Model ComponentAlias.MsgIn ComponentAlias.MsgOut
component =
    let
        compiletimeConfig =
            ()

        testedComponent =
            ComponentAlias.component compiletimeConfig
    in
    { init = testedComponent.init
    , update = testedComponent.update
    , kill = testedComponent.kill
    , subs = testedComponent.subs
    , view = view
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


view : ComponentAlias.Model -> Html ComponentAlias.MsgIn
view model =
    Html.text "Hello"


