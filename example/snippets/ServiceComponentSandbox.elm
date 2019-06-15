module ServiceComponentSandbox exposing (..)

import Webbhuset.Component.Sandbox as Sandbox exposing (SandboxProgram)
import Webbhuset.Component as Component
import Html exposing (Html)
import ServiceComponent as ComponentAlias


type alias Model = ComponentAlias.Model
type alias MsgIn = ComponentAlias.MsgIn
type alias MsgOut = ComponentAlias.MsgOut


main : SandboxProgram Model MsgIn MsgOut
main =
    Sandbox.service
        { title = "Service Component"
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
view : Model -> Html MsgIn
view model =
    Html.text "Hello"



test_init : Sandbox.TestCase MsgIn MsgOut
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


