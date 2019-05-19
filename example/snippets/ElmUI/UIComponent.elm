module ElmUI.UIComponent exposing
    ( MsgIn(..)
    , MsgOut(..)
    , Model
    , component
    )

import Webbhuset.Component.ElmUI as Component exposing (PID)
import Element exposing (Element)




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
    , kill = kill
    , subs = subs
    }


init : PID -> ( Model , List MsgOut, Cmd MsgIn )
init pid =
    ( { pid = pid
      }
    , []
    , Cmd.none
    )


kill : Model -> List MsgOut
kill model =
    []


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
