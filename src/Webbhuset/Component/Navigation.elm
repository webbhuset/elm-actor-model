module Webbhuset.Component.Navigation exposing
    ( MsgIn(..)
    , MsgOut(..)
    , Model
    , component
    )

{-|

@docs MsgIn
@docs MsgOut

@docs component, Model
-}

import Webbhuset.Component as Component exposing (PID)
import Browser.Navigation as Nav exposing (Key)
import Url exposing (Url)



{-| Message In
-}
type MsgIn
    = Init Key Url
    | Push String
    | Load String


{-| Message Out
-}
type MsgOut
    = NoOut



{-| Component Model. This component has an init and a running state.
-}
type Model
    = InitState InitModel
    | RunningState RunningModel


type alias InitModel =
    { pid : PID
    }


type alias RunningModel =
    { pid : PID
    , key : Key
    }

{-| Component Record
-}
component : Component.Service Model MsgIn MsgOut
component =
    { init = init
    , update = update
    , onSystem = always Nothing
    , subs = subs
    }


init : PID -> ( Model , List MsgOut, Cmd MsgIn )
init pid =
    ( { pid = pid
      }
        |> InitState
    , []
    , Cmd.none
    )


kill : Model -> List MsgOut
kill model =
    []


subs : Model -> Sub MsgIn
subs model =
    Sub.none


update : MsgIn -> Model -> ( Model, List MsgOut, Cmd MsgIn )
update msgIn model =
    case model of
        InitState initModel ->
            updateInit msgIn initModel

        RunningState runningModel ->
            updateRunning msgIn runningModel
                |> Component.mapFirst RunningState



updateInit : MsgIn -> InitModel -> ( Model, List MsgOut, Cmd MsgIn )
updateInit msgIn model =
    case msgIn of
        Init key url ->
            ( { pid = model.pid
              , key = key
              }
                |> RunningState
            , []
            , Cmd.none
            )

        Push href ->
            ( model
                |> InitState
            , []
            , Cmd.none
            )

        Load href ->
            ( model
                |> InitState
            , []
            , Cmd.none
            )


updateRunning : MsgIn -> RunningModel -> ( RunningModel, List MsgOut, Cmd MsgIn )
updateRunning msgIn model =
    case msgIn of
        Init key url ->
            ( model
            , []
            , Cmd.none
            )

        Push href ->
            ( model
            , []
            , Nav.pushUrl model.key href
            )

        Load href ->
            ( model
            , []
            , Cmd.none
            )
