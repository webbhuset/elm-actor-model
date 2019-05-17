module LayoutComponent exposing
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

component : Component.Layout Model MsgIn MsgOut msg
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


view : (MsgIn -> msg) -> Model -> (PID -> Html msg) -> Html msg
view toSelf model renderPID =
    Html.div
        []
        [ Html.text "Layout Component"
        ]
