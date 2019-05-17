module Webbhuset.ActorSystem exposing
    ( AppliedActor
    , Actor
    , Model
    , SysMsg
    , PID
    , addView
    , application
    , applyModel
    , batch
    , element
    , kill
    , toAppMsg
    , none
    , sendToPID
    , sendToSingleton
    , spawn
    , spawnSingleton
    , withSingletonPID
    )

{-|

@docs PID

## Build and Initialize the System

@docs element
    , application


## System Messages

@docs none
    , batch
    , toAppMsg

## Processes

Actors can be spawned (instantiated or started). A spawned actor is
called Process. A process is referenced by its PID.

Knowing an Actor's PID you can send messages to it or kill it.

@docs spawn
    , sendToPID
    , kill

## Singleton Processes

It can be helpful to treat some actors as a singleton process. They
are started on application init and will always be running.
Some examples are the Router actor or the Navigation. It does not make
much sense having multiple of them either.

A singleton process is just a normal actor that you can reference by
its name instead of its PID.

@docs spawnSingleton
    , sendToSingleton
    , withSingletonPID


## Program Output

You Elm program's Html output (view function) is controlled here.
You need to add at least one process to actually see anything more
than a blank page.

@docs addView

## Bootstrap

Don't worry about these for now.

@docs AppliedActor
    , Actor
    , Model
    , SysMsg
    , applyModel
-}

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html)
import List.Extra as List
import Random
import Url exposing (Url)
import Webbhuset.Internal.PID as PID exposing (PID(..))
import Webbhuset.Internal.Msg as Msg exposing (Msg(..), Control(..))


{-| A PID is an identifier for a Process.

-}
type alias PID =
    PID.PID



{-| Your Elm Program will have this as its Msg type.

-}
type alias SysMsg name appMsg =
    Msg name appMsg



{-| Don't send or do anything.

Similar concept to `Cmd.none`

-}
none : SysMsg name appMsg
none =
    None


{-| Wrapper for you app's msg type

-}
toAppMsg : appMsg -> SysMsg name appMsg
toAppMsg d =
    AppMsg d


{-| Batch control messages

Similar concept to `Cmd.batch`

-}
batch : List (SysMsg name appMsg) -> SysMsg name appMsg
batch list =
    Ctrl (Batch list)


{-| Send a message to a Process

-}
sendToPID : PID -> SysMsg name appMsg -> SysMsg name appMsg
sendToPID pid msg =
    if msg == None then
        msg

    else
        Ctrl (SendToPID pid msg)


{-| Send a message to a Singleton Process

-}
sendToSingleton : name -> SysMsg name appMsg -> SysMsg name appMsg
sendToSingleton name msg =
    if msg == None then
        msg

    else
        Ctrl (SendToSingleton name msg)


{-| Start an Actor. This will create a process. The PID will
be sent in a message using the provied message constructor.

    System.spawn
        ActorName.LoginForm
        (\pid ->
            PageLayout.SetContent pid
                |> System.toAppMsg
                |> System.sendToSingleton ActorName.PageLayout
        )
-}
spawn : name -> (PID -> SysMsg name appMsg) -> SysMsg name appMsg
spawn name replyMsg =
    Ctrl (Spawn name replyMsg)


{-| Spawn a singleton process.

-}
spawnSingleton : name -> SysMsg name appMsg
spawnSingleton name =
    Ctrl (SpawnSingleton name)


{-| Kill a process

-}
kill : PID -> SysMsg name appMsg
kill pid =
    Ctrl (Kill pid)


{-| Add a process to the global output.

-}
addView : PID -> SysMsg name appMsg
addView pid =
    Ctrl (AddView pid)


{-| Do something with a singleton PID.

Sometimes you want to send a singleton PID to a process.

For example, add the root layout component to the system output.

    init flags =
        [ System.withSingletonPID ActorName.PageLayout System.addView
        ]


Another example, you want to treat the site Header as a singleton
which makes it easier to send messages to it.


    System.withSingletonPID
        ActorName.Header
        (\pid ->
            PageLayout.SetHeader pid
                |> System.toAppMsg
                |> System.sendToSingleton ActorName.PageLayout
        )

-}
withSingletonPID : name -> (PID -> SysMsg name appMsg) -> SysMsg name appMsg
withSingletonPID name toMsg =
    Ctrl (WithSingletonPID name toMsg)


{-| The Global Model

-}
type Model name appModel =
    Model (ModelRecord name appModel)


type alias ModelRecord name appModel =
    { instances : Dict Int appModel
    , lastPID : Int
    , prefix : String
    , singleton : List ( name, PID )
    , views : List PID
    }


{-| An actor after the model has been applied

-}
type AppliedActor appModel output msg =
    AppliedActor
        { init : PID -> ( appModel, msg )
        , update : msg -> PID -> ( appModel, msg )
        , view : PID -> (PID -> output) -> output
        , kill : PID -> msg
        , subs : PID -> Sub msg
        }


type alias Impl name appModel output appMsg a =
    { a
        | spawn : name -> PID -> ( appModel, SysMsg name appMsg )
        , apply : appModel -> AppliedActor appModel output (SysMsg name appMsg)
        , emptyOutput : output
    }


{-| The implementation of a Browser.element program

-}
type alias ElementImpl flags name appModel output appMsg =
    { init : flags -> SysMsg name appMsg
    , spawn : name -> PID -> ( appModel, SysMsg name appMsg )
    , apply : appModel -> AppliedActor appModel output (SysMsg name appMsg)
    , view : List output -> Html (SysMsg name appMsg)
    , emptyOutput : output
    }


{-| The implementation of a Browser.application program

-}
type alias ApplicationImpl flags name appModel output appMsg =
    { init : flags -> Url -> Nav.Key -> SysMsg name appMsg
    , spawn : name -> PID -> ( appModel, SysMsg name appMsg )
    , apply : appModel -> AppliedActor appModel output (SysMsg name appMsg)
    , view : List output -> Html (SysMsg name appMsg)
    , emptyOutput : output
    , onUrlRequest : Browser.UrlRequest -> SysMsg name appMsg
    , onUrlChange : Url -> SysMsg name appMsg
    }


{-| Apply the compModel to an actor.

-}
applyModel : Actor compModel appModel output msg -> compModel -> AppliedActor appModel output msg
applyModel actor model =
    AppliedActor
        { init = actor.init
        , update = actor.update model
        , view = actor.view model
        , kill = actor.kill model
        , subs = actor.subs model
        }


{-| An actor is a component that is configured to be part of the system.

-}
type alias Actor compModel appModel output msg =
    { init : PID -> ( appModel, msg )
    , update : compModel -> msg -> PID -> ( appModel, msg )
    , view : compModel -> PID -> (PID -> output) -> output
    , kill : compModel -> PID -> msg
    , subs : compModel -> PID -> Sub msg
    }


{-| Create a [Browser.element] from your Actor System

[Browser.element]: https://package.elm-lang.org/packages/elm/browser/latest/Browser#element

-}
element :
    { init : flags -> SysMsg name appMsg
    , spawn : name -> PID -> ( appModel, SysMsg name appMsg )
    , apply : appModel -> AppliedActor appModel output (SysMsg name appMsg)
    , view : List output -> Html (SysMsg name appMsg)
    , emptyOutput : output
    }
    -> Program flags (Model name appModel) (SysMsg name appMsg)
element impl =
    Browser.element
        { init = initElement impl
        , update = update impl
        , subscriptions = subscriptions impl
        , view = impl.view << view impl
        }


{-| Create a [Browser.application] from your Actor System

[Browser.application]: https://package.elm-lang.org/packages/elm/browser/latest/Browser#application

-}
application :
    { init : flags -> Url -> Nav.Key -> SysMsg name appMsg
    , spawn : name -> PID -> ( appModel, SysMsg name appMsg )
    , apply : appModel -> AppliedActor appModel output (SysMsg name appMsg)
    , view : List output -> Html (SysMsg name appMsg)
    , emptyOutput : output
    , onUrlRequest : Browser.UrlRequest -> SysMsg name appMsg
    , onUrlChange : Url -> SysMsg name appMsg
    }
    -> Program flags (Model name appModel) (SysMsg name appMsg)
application impl =
    Browser.application
        { init = initApplication impl
        , update = update impl
        , subscriptions = subscriptions impl
        , view = view impl >> impl.view >> (\html -> { title = "", body = [ html ] })
        , onUrlRequest = impl.onUrlRequest
        , onUrlChange = impl.onUrlChange
        }


initElement : ElementImpl flags name appModel output appMsg -> flags -> ( Model name appModel, Cmd (SysMsg name appMsg) )
initElement impl flags =
    ( { instances = Dict.empty
      , lastPID = 100
      , prefix = ""
      , singleton = []
      , views = []
      }
          |> Model
    , Random.generate
          (Init (impl.init flags))
          prefixGenerator
    )


initApplication :
    ApplicationImpl flags name appModel output appMsg
    -> flags
    -> Url
    -> Nav.Key
    -> ( Model name appModel, Cmd (SysMsg name appMsg) )
initApplication impl flags url key =
    ( { instances = Dict.empty
      , lastPID = 100
      , prefix = ""
      , singleton = []
      , views = []
      }
        |> Model
    , Random.generate
        (Init (impl.init flags url key))
        prefixGenerator
    )


prefixGenerator : Random.Generator String
prefixGenerator =
    Random.int 0 60
        |> Random.list 16
        |> Random.map
            (List.map
                (\n ->
                    if n < 10 then
                        Char.fromCode (n + 48)

                    else if n < 35 then
                        Char.fromCode (n + 55)

                    else
                        Char.fromCode (n + 62)
                )
                >> String.fromList
            )


collectAppMsgs : SysMsg name appMsg -> List (SysMsg name appMsg)
collectAppMsgs msg =
    case msg of
        AppMsg _ ->
            [ msg ]

        Ctrl (Batch list) ->
            List.concatMap collectAppMsgs list

        _ ->
            []


composeSysMsg : SysMsg name appMsg -> SysMsg name appMsg -> SysMsg name appMsg
composeSysMsg msg1 msg2 =
    if msg1 == None then
        msg2
    else if msg2 == None then
        msg1
    else
        [ msg1, msg2 ]
            |> Batch
            |> Ctrl


update :
    Impl name appModel output appMsg a
    -> SysMsg name appMsg
    -> Model name appModel
    -> ( Model name appModel, Cmd (SysMsg name appMsg) )
update impl msg ((Model modelRecord) as model) =
    case msg of
        None ->
            ( model, Cmd.none )

        AppMsg _ ->
            ( model, Cmd.none )

        Init initMsg prefix ->
            { modelRecord | prefix = prefix }
                |> Model
                |> update impl initMsg

        UnmappedMsg appMsg ->
            ( model, Cmd.none )

        Ctrl ctrlMsg ->
            case ctrlMsg of
                Batch listOfMsgs ->
                    listOfMsgs
                        |> List.foldl
                            (\batchMsg previous ->
                                cmdAndThen (update impl batchMsg) previous
                            )
                            ( model, Cmd.none )

                Cmd cmd ->
                    ( model, cmd )

                SendToPID pid message ->
                    case getProcess pid modelRecord of
                        Just appModel ->
                            let
                                appMsgs =
                                    collectAppMsgs message

                                ( m2, newMsg ) =
                                    appMsgs
                                        |> List.foldl
                                            (\appMsg ( mod0, sysMsg0 ) ->
                                                let
                                                    (AppliedActor applied) =
                                                        impl.apply mod0

                                                    (mod1, sysMsg1) =
                                                        applied.update appMsg pid
                                                in
                                                ( mod1
                                                , composeSysMsg sysMsg0 sysMsg1
                                                )

                                            )
                                            (appModel, None)
                                        |> Tuple.mapFirst (updateInstanceIn modelRecord pid >> Model)
                            in
                            update impl newMsg m2

                        Nothing ->
                            ( model, Cmd.none )

                SendToSingleton name message ->
                    case findSingletonPID name modelRecord of
                        Just pid ->
                            update impl (sendToPID pid message) model

                        Nothing ->
                            update impl (spawnSingleton name) model
                                |> cmdAndThen (update impl msg)

                Spawn name replyMsg ->
                    let
                        ( m2, pid ) =
                            newPID modelRecord

                        ( m3, newMsg ) =
                            spawn_ impl name pid m2
                    in
                    update impl newMsg (Model m3)
                        |> cmdAndThen (update impl (replyMsg pid))

                Kill ((PID _ key) as pid) ->
                    case Dict.get key modelRecord.instances of
                        Just appModel ->
                            let
                                (AppliedActor applied) =
                                    impl.apply appModel

                                componentLastWords =
                                    applied.kill pid
                            in
                            { modelRecord | instances = Dict.remove key modelRecord.instances }
                                |> Model
                                |> update impl componentLastWords

                        Nothing ->
                            ( model, Cmd.none )

                SpawnSingleton name ->
                    let
                        ( m2, pid ) =
                            newPID modelRecord

                        ( m3, newMsg ) =
                            appendSingleton name pid m2
                                |> spawn_ impl name pid
                    in
                    update impl newMsg (Model m3)

                AddView pid ->
                    ( { modelRecord | views = pid :: modelRecord.views }
                        |> Model
                    , Cmd.none
                    )

                WithSingletonPID name makeMsg ->
                    case findSingletonPID name modelRecord of
                        Just pid ->
                            update impl (makeMsg pid) model

                        Nothing ->
                            update impl (spawnSingleton name) model
                                |> cmdAndThen (update impl msg)


spawn_ : Impl name appModel output appMsg a -> name -> PID -> ModelRecord name appModel -> ( ModelRecord name appModel, SysMsg name appMsg )
spawn_ impl name pid model =
    impl.spawn name pid
        |> Tuple.mapFirst (updateInstanceIn model pid)


newPID : ModelRecord name appModel -> ( ModelRecord name appModel, PID )
newPID model =
    model.lastPID
        |> PID model.prefix
        |> Tuple.pair { model | lastPID = 1 + model.lastPID }


getProcess : PID -> ModelRecord name appModel -> Maybe appModel
getProcess (PID _ pid) model =
    Dict.get pid model.instances


getInstanceFrom : ModelRecord name appModel -> PID -> Maybe appModel
getInstanceFrom model (PID _ pid) =
    Dict.get pid model.instances


updateInstanceIn : ModelRecord name appModel -> PID -> appModel -> ModelRecord name appModel
updateInstanceIn model (PID _ pid) appModel =
    { model | instances = Dict.insert pid appModel model.instances }


appendSingleton : name -> PID -> ModelRecord name appModel -> ModelRecord name appModel
appendSingleton name pid model =
    { model
        | singleton = ( name, pid ) :: model.singleton
    }


findSingletonPID : name -> ModelRecord name appModel -> Maybe PID
findSingletonPID name model =
    model.singleton
        |> List.find (\( a, _ ) -> a == name)
        |> Maybe.map Tuple.second


subscriptions : Impl name appModel output appMsg a -> Model name appModel -> Sub (SysMsg name appMsg)
subscriptions impl (Model model) =
    model.instances
        |> Dict.foldl
            (\pid appModel subs ->
                let
                    (AppliedActor applied) =
                        impl.apply appModel

                    sub =
                        applied.subs (PID model.prefix pid)
                in
                if sub == Sub.none then
                    subs

                else
                    sub :: subs
            )
            []
        |> Sub.batch


view : Impl name appModel output appMsg a -> Model name appModel -> List output
view impl (Model model) =
    model.views
        |> List.map
            (renderPID
                impl.emptyOutput
                (\p ->
                    let
                        (AppliedActor applied) =
                            impl.apply p
                    in
                    applied.view
                )
                model.instances
            )


renderPID : output -> (appModel -> PID -> (PID -> output) -> output) -> Dict Int appModel -> PID -> output
renderPID emptyOutput viewFn dict ((PID prefix pid) as p) =
    Dict.get pid dict
        |> Maybe.map
            (\appModel ->
                viewFn appModel p (renderPID emptyOutput viewFn dict)
            )
        |> Maybe.withDefault emptyOutput


cmdAndThen : (m -> ( m, Cmd msg )) -> ( m, Cmd msg ) -> ( m, Cmd msg )
cmdAndThen fn ( m0, cmd0 ) =
    let
        ( m1, cmd1 ) =
            fn m0
    in
    ( m1, Cmd.batch [ cmd0, cmd1 ] )
