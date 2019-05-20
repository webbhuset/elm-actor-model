module Html.LayoutComponent exposing
    ( MsgIn(..)
    , MsgOut(..)
    , Model
    , component
    )

import Html exposing (Html)
import Html.Attributes as HA
import Webbhuset.Component as Component exposing (PID)


type MsgIn
    = Show String
    | ReceiveChild PID


type MsgOut
    = SpawnRendererFor String


type alias Model =
    { pid : PID
    , children : List PID
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
      , children = []
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
        Show str ->
            ( model
            , [ SpawnRendererFor str
              ]
            , Cmd.none
            )

        ReceiveChild pid ->
            ( { model | children = model.children ++ [ pid ] }
            , []
            , Cmd.none
            )


view : (MsgIn -> msg) -> Model -> (PID -> Html msg) -> Html msg
view toSelf model renderPID =
    Html.div
        []
        [ Html.h1 [] [ Html.text "Layout Component" ]
        , model.children
            |> List.map renderPID
            |> Html.div []
        ]
