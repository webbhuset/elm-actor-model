module TodoApp.Bootstrap exposing (..)


import Webbhuset.ActorSystem as System
import TodoApp.Msg as Msg
import TodoApp.ActorName as ActorName

import TodoApp.Actor.TodoList as TodoList
import TodoApp.Actor.TodoItem as TodoItem
import TodoApp.Actor.TodoService as TodoService


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
        ActorName.TodoList ->
            actors.todoList.init

        ActorName.TodoItem ->
            actors.todoItem.init

        ActorName.TodoService ->
            actors.todoService.init


applyModel model =
    case model of
        TodoList m ->
            System.applyModel actors.todoList m

        TodoItem m ->
            System.applyModel actors.todoItem m

        TodoService m ->
            System.applyModel actors.todoService m
