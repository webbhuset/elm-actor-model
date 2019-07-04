module TodoApp.Msg exposing (..)

import Webbhuset.ActorSystem as System
import TodoApp.ActorName exposing (ActorName)

import TodoApp.Component.TodoList as TodoList
import TodoApp.Component.TodoItem as TodoItem
import TodoApp.Component.TodoService as TodoService


type alias Msg =
    System.SysMsg ActorName AppMsg


type AppMsg
    = TodoList TodoList.MsgIn
    | TodoItem TodoItem.MsgIn
    | TodoService TodoService.MsgIn
    | Dummy
