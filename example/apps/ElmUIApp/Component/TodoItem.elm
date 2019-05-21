module ElmUIApp.Component.TodoItem exposing
    ( MsgIn(..)
    , MsgOut(..)
    , Model
    , component
    )

import Element exposing (Element)
import Webbhuset.ElmUI.Component as Component exposing (PID)
import Webbhuset.Component.SystemEvent as SystemEvent exposing (SystemEvent)




type MsgIn
    = NoIn


type MsgOut
    = NoOut


type alias Model =
    { pid : PID
    }


--
-- Component
--

component : Component.UI Model MsgIn MsgOut
component =
    { init = init
    , update = update
    , view = view
    , onSystem = always SystemEvent.default
    , subs = subs
    }


init : PID -> ( Model , List MsgOut, Cmd MsgIn )
init pid =
    ( { pid = pid
      }
    , []
    , Cmd.none
    )


subs : Model -> Sub MsgIn
subs model =
    Sub.none


update : MsgIn -> Model -> ( Model, List MsgOut, Cmd MsgIn )
update msgIn model =
    case msgIn of
        NoIn ->
            ( model
            , []
            , Cmd.none
            )


view : Model -> Element MsgIn
view model =
    Element.column
        []
        [ Element.text "Empty Component"
        ]
