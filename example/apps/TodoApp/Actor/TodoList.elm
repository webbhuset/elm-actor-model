module TodoApp.Actor.TodoList exposing (..)

import Webbhuset.ActorSystem as System
import Webbhuset.Actor as Actor exposing (Actor)
import Webbhuset.PID exposing (PID)
import TodoApp.Msg as Msg exposing (Msg)
import TodoApp.Component.TodoList as TodoList


type alias Model =
    TodoList.Model

actor : (TodoList.Model -> appModel) -> Actor Model appModel Msg
actor toAppModel =
    Actor.fromLayout
        { wrapModel = toAppModel
        , wrapMsg = Msg.TodoList
        , mapIn = mapIn
        , mapOut = mapOut
        }
        TodoList.component


mapIn : Msg.AppMsg -> Maybe TodoList.MsgIn
mapIn appMsg =
    case appMsg of
        Msg.TodoList msgIn ->
            Just msgIn

        _ ->
            Nothing


mapOut : PID -> TodoList.MsgOut -> Msg
mapOut pid msgOut =
    System.none
