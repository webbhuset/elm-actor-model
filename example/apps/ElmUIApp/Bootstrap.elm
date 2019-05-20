module ElmUIApp.Bootstrap exposing (..)


import Webbhuset.ActorSystem as System
import ElmUIApp.Msg as Msg
import ElmUIApp.Actor as Actor

import ElmUIApp.Actor.TodoList as TodoList
import ElmUIApp.Actor.TodoItem as TodoItem
import ElmUIApp.Actor.TodoService as TodoService


type Model
    = TodoList TodoList.Model
    | TodoItem TodoItem.Model
    | TodoService TodoService.Model


actors =
    { todoList = TodoList.actor TodoList
    , todoItem = TodoItem.actor TodoItem
    , todoService = TodoService.actor TodoService
    }


spawn actor =
    case actor of
        Actor.TodoList ->
            actors.todoList.init

        Actor.TodoItem ->
            actors.todoItem.init

        Actor.TodoService ->
            actors.todoService.init


applyModel model =
    case model of
        TodoList m ->
            System.applyModel actors.todoList m

        TodoItem m ->
            System.applyModel actors.todoItem m

        TodoService m ->
            System.applyModel actors.todoService m
