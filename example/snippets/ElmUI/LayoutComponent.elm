module ElmUI.LayoutComponent exposing
    ( MsgIn(..)
    , MsgOut(..)
    , Model
    , component
    )

import Element exposing (Element)
import Webbhuset.Component.ElmUI as Component exposing (PID)
import Webbhuset.Component.SystemEvent as SystemEvent exposing (SystemEvent)


type MsgIn
    = NoIn
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
    , onSystem = onSystem
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


onSystem : SystemEvent -> Maybe MsgIn
onSystem event =
    Nothing


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


view : (MsgIn -> msg) -> Model -> (PID -> Element msg) -> Element msg
view toSelf model renderPID =
    Element.column
        []
        [ Element.el [] (Element.text "Layout Component" )
        , model.children
            |> List.map renderPID
            |> Element.column []
        ]
