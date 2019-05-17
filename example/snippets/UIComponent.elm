module UIComponent exposing
    ( MsgIn(..)
    , MsgOut(..)
    , Model
    , component
    )

import Html exposing (Html)
import Html.Attributes as HA
import Webbhuset.Component as Component exposing (PID)




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


view : Model -> Html MsgIn
view model =
    Html.div
        []
        [ Html.text "Empty Component"
        ]
