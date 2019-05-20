module ElmUIApp.Main exposing (..)

import Html exposing (Html)
import Element exposing (Element)
import Webbhuset.ActorSystem as System
import ElmUIApp.Msg as Msg exposing (Msg)
import ElmUIApp.Bootstrap as Bootstrap
import ElmUIApp.ActorName as ActorName exposing (ActorName)

type alias Model =
    System.Model ActorName Bootstrap.Model


main : Program () Model Msg
main =
    System.element
        { spawn = Bootstrap.spawn
        , apply = Bootstrap.applyModel
        , init = init
        , view = view
        }


init : () -> Msg
init flags =
    [ System.withSingletonPID ActorName.TodoList System.addView
    ]
        |> System.batch


view : List (Element Msg) -> Html Msg
view actorOutput =
    Element.column
        [
        ]
        actorOutput
        |> Element.layout []
