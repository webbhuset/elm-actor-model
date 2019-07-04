module TodoApp.Component.TodoList exposing
    ( MsgIn(..)
    , MsgOut(..)
    , Model
    , component
    )

import Webbhuset.Component as Component exposing (PID)
import Webbhuset.Component.SystemEvent as SystemEvent exposing (SystemEvent)
import Html exposing (Html)


type MsgIn
    = NoIn
    | ReceiveChild PID


type MsgOut
    = NoOut


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
    , onSystem = always SystemEvent.default
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
        NoIn ->
            ( model
            , []
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
        [ Html.h2 [] [ Html.text "Todo list" ]
        , model.children
            |> List.map renderPID
            |> Html.div []
        ]
