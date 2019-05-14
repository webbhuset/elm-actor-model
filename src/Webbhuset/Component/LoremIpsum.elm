module Webbhuset.Component.LoremIpsum exposing
    ( MsgIn(..)
    , MsgOut(..)
    , Model
    , component
    )


import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Webbhuset.Component as Component exposing (PID)
import Random



type MsgIn
    = More
    | Less


type MsgOut
    = NoOut


type alias Model =
    { pid : PID
    , count : Int
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
      , count = 10
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


view : Model -> Html MsgIn
view model =
    Html.div
        []
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
            [ List.repeat model.count "Lorem"
                |> String.join " "
                |> Html.text
            ]
        ]
