module ServiceComponent exposing
    ( Config
    , MsgIn(..)
    , MsgOut(..)
    , Model
    , component
    )

{-|

@docs Config
@docs MsgIn
@docs MsgOut

@docs component, Model
-}

import Webbhuset.Component as Component exposing (PID)


{-| Component Config
-}
type alias Config =
    ()


{-| Message In
-}
type MsgIn
    = NoIn


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
    }

{-| Component Record
-}
component : Config -> Component.Service Model MsgIn MsgOut
component config =
    { init = init config
    , update = update config
    , kill = kill
    , subs = subs
    }


init : Config -> PID -> ( Model , List MsgOut, Cmd MsgIn )
init config pid =
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


update : Config -> MsgIn -> Model -> ( Model, List MsgOut, Cmd MsgIn )
update config msgIn model =
    case model of
        InitState initModel ->
            updateInit config msgIn initModel

        RunningState runningModel ->
            updateRunning config msgIn runningModel
                |> Component.mapFirst RunningState



updateInit : Config -> MsgIn -> InitModel -> ( Model, List MsgOut, Cmd MsgIn )
updateInit config msgIn model =
    case msgIn of
        NoIn ->
            ( model
                |> InitState
            , []
            , Cmd.none
            )


updateRunning : Config -> MsgIn -> RunningModel -> ( RunningModel, List MsgOut, Cmd MsgIn )
updateRunning config msgIn model =
    case msgIn of
        NoIn ->
            ( model
            , []
            , Cmd.none
            )


