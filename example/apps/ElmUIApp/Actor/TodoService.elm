module ElmUIApp.Actor.TodoService exposing (..)

import Webbhuset.ActorSystem as System
import Webbhuset.ElmUI.Actor as Actor exposing (Actor)
import Webbhuset.PID as PID exposing (PID)
import ElmUIApp.Msg as Msg exposing (Msg)
import ElmUIApp.Component.TodoService as TodoService
import ElmUIApp.ActorName as ActorName


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
