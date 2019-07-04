module TodoApp.Actor.TodoService exposing (..)

import Webbhuset.ActorSystem as System
import Webbhuset.Actor as Actor exposing (Actor)
import Webbhuset.PID as PID exposing (PID)
import TodoApp.Msg as Msg exposing (Msg)
import TodoApp.Component.TodoService as TodoService
import TodoApp.ActorName as ActorName


type alias Model =
    TodoService.Model

actor : (TodoService.Model -> appModel) -> Actor Model appModel Msg
actor toAppModel =
    Actor.fromService
        { wrapModel = toAppModel
        , wrapMsg = Msg.TodoService
        , mapIn = mapIn
        , mapOut = mapOut
        }
        (TodoService.component ())


mapIn : Msg.AppMsg -> Maybe TodoService.MsgIn
mapIn appMsg =
    case appMsg of
        Msg.TodoService msgIn ->
            Just msgIn

        _ ->
            Nothing


mapOut : PID -> TodoService.MsgOut -> Msg
mapOut self msgOut =
    case msgOut of
        TodoService.Init ->
            System.none
