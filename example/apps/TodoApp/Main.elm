module TodoApp.Main exposing (..)

import Html exposing (Html)
import Webbhuset.ActorSystem as System
import TodoApp.Msg as Msg exposing (Msg)
import TodoApp.Bootstrap as Bootstrap
import TodoApp.ActorName as ActorName exposing (ActorName)

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



view : List (Html Msg) -> Html Msg
view actorOutput =
    Html.div
        [
        ]
        actorOutput



type alias State s a =
    s -> ( a, s )


return : a -> State s a
return a =
    \s -> ( a, s )


runState : s -> State s a -> ( a, s )
runState s st =
    st s


andThen : (a -> State s b) -> State s a -> State s b
andThen cont state =
    \s1 ->
        let
            ( a, s2 ) = state s1
        in
        cont a s2


try =
    return "prefix"
        |> andThen one
        |> andThen one
        |> runState 1


init2 =
    spawn "auth" "arga" <| \auth ->
    spawn "test" "argt" <| \test ->
    return
        { test = test
        , auth = auth
        }


runInit =
    init2
        |> runState ( 1, [] )


spawn name arg cont =
    spawn_ name arg
        |> andThen cont


spawn_ name arg ( p, ns ) =
    ( p
    , ( p + 1, ( p, name, arg) :: ns )
    )


one a s =
    ( a ++ (String.fromInt s)
    , s + 1
    )


type alias Init =
    { last : Int
    , list : List ( String, Int )
    }


{-
tspawn : Name -> (Int -> State Init a) -> State Init a -> State Init b
tspawn name cont state =
    

tspawn_ : State -> Name -> (Int -> a) -> State
tspawn_ ( p, l ) name cont =
    let
        _ = cont p
    in
    ( p + 1,
    , ( name, p ) :: list
    )


tspawn : Name -> (Int -> a) -> State
tspawn name cont =
    tspawn_ ( 1, [] )


tstart : a -> Writer a
tstart a =
    ( a, [] )


init2 flags system =
    tspawn "auth" <| \authPid ->
    tspawn "router" <| \routerPid ->
    tstart
        { auth = authPid
        , router = routerPid
        }

-}
