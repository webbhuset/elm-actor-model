module Webbhuset.Component.Sandbox.LoremIpsum exposing
    ( MsgIn(..)
    , MsgOut(..)
    , Model
    , component
    )


import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Random
import Webbhuset.Component as Component exposing (PID)
import Webbhuset.Component.SystemEvent as SystemEvent exposing (SystemEvent)



type MsgIn
    = More
    | Less
    | SetText String


type MsgOut
    = NoOut


type alias Model =
    { pid : PID
    , count : Int
    , text : String
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
      , count = 10
      , text = ""
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
        More ->
            ( { model | count = model.count * 2 }
            , []
            , Cmd.none
            )

        Less ->
            ( { model | count = model.count // 2 }
            , []
            , Cmd.none
            )

        SetText str ->
            ( { model | text = str }
            , []
            , Cmd.none
            )


view : Model -> Html MsgIn
view model =
    Html.div
        [ HA.style "border" "solid 1px black"
        ]
        [ Html.button
            [ Events.onClick Less
            ]
            [ Html.text "Less"
            ]
        , Html.button
            [ Events.onClick More
            ]
            [ Html.text "More"
            ]
        , Html.div
            [
            ]
            [ List.repeat model.count model.text
                |> String.join " "
                |> Html.text
            ]
        ]
