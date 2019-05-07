module Webbhuset.ActorSystem exposing
    ( AppliedActor
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
    )

{-|

@docs PID

## Build and Initialize the System

@docs element
    , application

## System Messages

Use these to send messages between actors in your system.

@docs none
    , batch
    , kill
    , toAppMsg
    , sendToPID
    , sendToSingleton
    , spawn
    , spawnSingleton
    , addView

## Bootstrap

Don't worry about these for now.

@docs AppliedActor
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
import Webbhuset.Actor as Actor exposing (Actor)


{-| A PID is an identifier for a Process.

-}
type alias PID =
    PID.PID



-- Ctrl


{-| Your Elm Program will have this as its Msg type.

-}
type alias SysMsg name appMsg =
    Msg name appMsg



{-| Don't send anything.

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


{-| Spawn a process. The PID will be send as a reply msg.

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
type AppliedActor appModel msg =
    AppliedActor
        { init : PID -> ( appModel, msg )
        , update : msg -> PID -> ( appModel, msg )
        , view : PID -> (PID -> Html msg) -> Html msg
        , kill : PID -> msg
        , subs : PID -> Sub msg
        }


type alias Impl name appModel appMsg a =
    { a
        | spawn : name -> PID -> ( appModel, SysMsg name appMsg )
        , apply : appModel -> AppliedActor appModel (SysMsg name appMsg)
    }


{-| The implementation of a Browser.element program

-}
type alias ElementImpl flags name appModel appMsg =
    { init : flags -> SysMsg name appMsg
    , spawn : name -> PID -> ( appModel, SysMsg name appMsg )
    , apply : appModel -> AppliedActor appModel (SysMsg name appMsg)
    }


{-| The implementation of a Browser.application program

-}
type alias ApplicationImpl flags name appModel appMsg =
    { init : flags -> Url -> Nav.Key -> SysMsg name appMsg
    , spawn : name -> PID -> ( appModel, SysMsg name appMsg )
    , apply : appModel -> AppliedActor appModel (SysMsg name appMsg)
    , onUrlRequest : Browser.UrlRequest -> SysMsg name appMsg
    , onUrlChange : Url -> SysMsg name appMsg
    }


{-| Apply the compModel to an actor.

-}
applyModel : Actor compModel appModel msg -> compModel -> AppliedActor appModel msg
applyModel actor model =
    AppliedActor
        { init = actor.init
        , update = actor.update model
        , view = actor.view model
        , kill = actor.kill model
        , subs = actor.subs model
        }

{-| Create a [Browser.element] from your Actor System

[Browser.element]: https://package.elm-lang.org/packages/elm/browser/latest/Browser#element

-}
element :
    { init : flags -> SysMsg name appMsg
    , spawn : name -> PID -> ( appModel, SysMsg name appMsg )
    , apply : appModel -> AppliedActor appModel (SysMsg name appMsg)
    }
    -> Program flags (Model name appModel) (SysMsg name appMsg)
element impl =
    Browser.element
        { init = initElement impl
        , update = update impl
        , subscriptions = subscriptions impl
        , view = view impl
        }


{-| Create a [Browser.application] from your Actor System

[Browser.application]: https://package.elm-lang.org/packages/elm/browser/latest/Browser#application

-}
application :
    { init : flags -> Url -> Nav.Key -> SysMsg name appMsg
    , spawn : name -> PID -> ( appModel, SysMsg name appMsg )
    , apply : appModel -> AppliedActor appModel (SysMsg name appMsg)
    , onUrlRequest : Browser.UrlRequest -> SysMsg name appMsg
    , onUrlChange : Url -> SysMsg name appMsg
    }
    -> Program flags (Model name appModel) (SysMsg name appMsg)
application impl =
    Browser.application
        { init = initApplication impl
        , update = update impl
        , subscriptions = subscriptions impl
        , view = view impl >> (\html -> { title = "", body = [ html ] })
        , onUrlRequest = impl.onUrlRequest
        , onUrlChange = impl.onUrlChange
        }


initElement : ElementImpl flags name appModel appMsg -> flags -> ( Model name appModel, Cmd (SysMsg name appMsg) )
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
    ApplicationImpl flags name appModel appMsg
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


update : Impl name appModel appMsg a -> SysMsg name appMsg -> Model name appModel -> ( Model name appModel, Cmd (SysMsg name appMsg) )
update impl msg ((Model model) as m) =
    case msg of
        None ->
            ( m, Cmd.none )

        AppMsg _ ->
            ( m, Cmd.none )

        Init initMsg prefix ->
            { model | prefix = prefix }
                |> Model
                |> update impl initMsg

        UnmappedMsg appMsg ->
            ( m, Cmd.none )

        Ctrl ctrlMsg ->
            case ctrlMsg of
                Batch listOfMsgs ->
                    listOfMsgs
                        |> List.foldl
                            (\batchMsg previous ->
                                cmdAndThen (update impl batchMsg) previous
                            )
                            ( m, Cmd.none )

                Cmd cmd ->
                    ( m, cmd )

                SendToPID pid message ->
                    case getProcess pid model of
                        Just appModel ->
                            let
                                (AppliedActor applied) =
                                    impl.apply appModel

                                ( m2, newMsg ) =
                                    applied.update message pid
                                        |> Tuple.mapFirst (updateInstanceIn model pid >> Model)
                            in
                            update impl newMsg m2

                        Nothing ->
                            ( m, Cmd.none )

                SendToSingleton name message ->
                    case findSingletonPID name model of
                        Just pid ->
                            update impl (sendToPID pid message) m

                        Nothing ->
                            update impl (spawnSingleton name) m
                                |> cmdAndThen (update impl msg)

                Spawn name replyMsg ->
                    let
                        ( m2, pid ) =
                            newPID model

                        ( m3, newMsg ) =
                            spawn_ impl name pid m2
                    in
                    update impl newMsg (Model m3)
                        |> cmdAndThen (update impl (replyMsg pid))

                Kill ((PID _ key) as pid) ->
                    case Dict.get key model.instances of
                        Just appModel ->
                            let
                                (AppliedActor applied) =
                                    impl.apply appModel

                                componentLastWords =
                                    applied.kill pid
                            in
                            { model | instances = Dict.remove key model.instances }
                                |> Model
                                |> update impl componentLastWords

                        Nothing ->
                            ( m, Cmd.none )

                SpawnSingleton name ->
                    let
                        ( m2, pid ) =
                            newPID model

                        ( m3, newMsg ) =
                            appendSingleton name pid m2
                                |> spawn_ impl name pid
                    in
                    update impl newMsg (Model m3)

                AddView pid ->
                    ( { model | views = pid :: model.views }
                        |> Model
                    , Cmd.none
                    )


spawn_ : Impl name appModel appMsg a -> name -> PID -> ModelRecord name appModel -> ( ModelRecord name appModel, SysMsg name appMsg )
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


subscriptions : Impl name appModel appMsg a -> Model name appModel -> Sub (SysMsg name appMsg)
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


view : Impl name appModel appMsg a -> Model name appModel -> Html (SysMsg name appMsg)
view impl (Model model) =
    model.views
        ++ List.map Tuple.second model.singleton
        |> List.map
            (renderPID
                (\p ->
                    let
                        (AppliedActor applied) =
                            impl.apply p
                    in
                    applied.view
                )
                model.instances
            )
        |> Html.div []


renderPID : (appModel -> PID -> (PID -> Html msg) -> Html msg) -> Dict Int appModel -> PID -> Html msg
renderPID viewFn dict ((PID prefix pid) as p) =
    Dict.get pid dict
        |> Maybe.map
            (\appModel ->
                viewFn appModel p (renderPID viewFn dict)
            )
        |> Maybe.withDefault (Html.text "")


cmdAndThen : (m -> ( m, Cmd msg )) -> ( m, Cmd msg ) -> ( m, Cmd msg )
cmdAndThen fn ( m0, cmd0 ) =
    let
        ( m1, cmd1 ) =
            fn m0
    in
    ( m1, Cmd.batch [ cmd0, cmd1 ] )
