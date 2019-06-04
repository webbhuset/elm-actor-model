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
        , onDebug = \error ->
            Debug.log "error" error
                |> always System.none
        }


init : () -> Msg
init flags =
    [ System.withSingletonPID ActorName.TodoList System.addView
    , System.spawnSingleton ActorName.TodoService
    , Msg.Dummy
        |> System.sendToSingleton ActorName.TodoService
    ]
        |> System.batch


view : List (Element Msg) -> Html Msg
view actorOutput =
    Element.column
        [
        ]
        actorOutput
        |> Element.layout []
