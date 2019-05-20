module ElmUIApp.Msg exposing (..)

import Webbhuset.ActorSystem as System
import ElmUIApp.Actor exposing (Actor)

import ElmUIApp.Component.TodoList as TodoList
import ElmUIApp.Component.TodoItem as TodoItem
import ElmUIApp.Component.TodoService as TodoService


type alias Msg =
    System.SysMsg Actor AppMsg


type AppMsg
    = TodoList TodoList.MsgIn
    | TodoItem TodoItem.MsgIn
    | TodoService TodoService.MsgIn
