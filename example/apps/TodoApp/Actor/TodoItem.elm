module TodoApp.Actor.TodoItem exposing (..)

import Webbhuset.ActorSystem as System
import Webbhuset.Actor as Actor exposing (Actor)
import Webbhuset.PID exposing (PID)
import TodoApp.Msg as Msg exposing (Msg)
import TodoApp.Component.TodoItem as TodoItem


type alias Model =
    TodoItem.Model

actor : (TodoItem.Model -> appModel) -> Actor Model appModel Msg
actor toAppModel =
    Actor.fromUI
        { wrapModel = toAppModel
        , wrapMsg = Msg.TodoItem
        , mapIn = mapIn
        , mapOut = mapOut
        }
        TodoItem.component


mapIn : Msg.AppMsg -> Maybe TodoItem.MsgIn
mapIn appMsg =
    case appMsg of
        Msg.TodoItem msgIn ->
            Just msgIn

        _ ->
            Nothing


mapOut : PID -> TodoItem.MsgOut -> Msg
mapOut pid msgOut =
    System.none
