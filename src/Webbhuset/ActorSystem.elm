module Webbhuset.ActorSystem
    exposing
        ( PID
        , Model
        , Msg
        , Impl
        , AppliedContainer
        , element
        , application
        , pidToID
        , noOne
        )

import Webbhuset.Ctrl as Ctrl exposing (Msg)
import Random
import List.Extra as List
import Dict exposing (Dict)
import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Url exposing (Url)


type PID =
    PID String Int


noOne : String -> PID
noOne name =
    PID name 0

pidToID : PID -> String
pidToID (PID prefix id) =
    prefix ++ (String.fromInt id)


type alias Model actor process =
    { instances : Dict Int process
    , lastPID : Int
    , prefix : String
    , singleton : List ( actor, PID )
    , views : List PID
    }


type alias AppliedContainer process msg =
    { init : PID -> (process, msg)
    , recv : msg -> PID -> (process, msg)
    , view : PID -> (PID -> Html msg) -> Html msg
    , kill : PID -> msg
    , subs : PID -> Sub msg
    }

type alias Msg actor data
    = Ctrl.Msg PID actor data


type alias Impl actor process data a =
    { a
        | spawn : actor -> PID -> (process, Msg actor data)
        , apply : process -> AppliedContainer process (Msg actor data)
    }

type alias ElementImpl actor process data =
    Impl actor process data
        { init : Msg actor data
        }

type alias ApplicationImpl actor process data =
    Impl actor process data
        { init : Url -> Nav.Key -> Msg actor data
        , onUrlRequest : Browser.UrlRequest -> (Msg actor data)
        , onUrlChange : Url -> (Msg actor data)
        }


element : ElementImpl actor process data -> Program () (Model actor process) (Msg actor data)
element impl =
    Browser.element
        { init = initElement impl
        , update = update impl
        , subscriptions = subscriptions impl
        , view = view impl
        }


application : ApplicationImpl actor process data -> Program () (Model actor process) (Msg actor data)
application impl =
    Browser.application
        { init = initApplication impl
        , update = update impl
        , subscriptions = subscriptions impl
        , view = view impl >> (\html -> { title = "", body = [ html ] } )
        , onUrlRequest = impl.onUrlRequest
        , onUrlChange = impl.onUrlChange
        }


initElement : ElementImpl actor process data -> flags -> ( Model actor process, Cmd (Msg actor data) )
initElement impl flags =
    Tuple.pair
        { instances = Dict.empty
        , lastPID = 100
        , prefix = ""
        , singleton = []
        , views = []
        }
        ( Random.generate
            (Ctrl.Init impl.init)
            prefixGenerator
        )

initApplication :
    ApplicationImpl actor process data
    -> flags
    -> Url
    -> Nav.Key
    -> ( Model actor process, Cmd (Msg actor data) )
initApplication impl flags url key =
    Tuple.pair
        { instances = Dict.empty
        , lastPID = 100
        , prefix = ""
        , singleton = []
        , views = []
        }
        ( Random.generate
            (Ctrl.Init (impl.init url key))
            prefixGenerator
        )


prefixGenerator : Random.Generator String
prefixGenerator =
    Random.int 0 60
        |> Random.list 16
        |> Random.map
            ( List.map
                (\n ->
                    if n < 10 then
                        Char.fromCode ( n + 48 )
                    else if n < 35 then
                        Char.fromCode ( n + 55 )
                    else
                        Char.fromCode ( n + 62 )
                )
                >> String.fromList
            )


update : Impl actor process data a -> (Msg actor data) -> Model actor process -> ( Model actor process, Cmd (Msg actor data) )
update impl msg model =
    case msg of
        Ctrl.None ->
            (model, Cmd.none)

        Ctrl.MsgTo _ ->
            (model, Cmd.none)

        Ctrl.Init initMsg prefix ->
            { model | prefix = prefix }
                |> update impl initMsg

        Ctrl.Ctrl ctrlMsg ->
            case ctrlMsg of
                Ctrl.Batch listOfMsgs ->
                    listOfMsgs
                        |> List.foldl
                            (\batchMsg previous ->
                                andThen (update impl batchMsg) previous
                            )
                            ( model, Cmd.none )

                Ctrl.Cmd cmd ->
                    (model, cmd)

                Ctrl.SendToPID pid message ->
                    case getProcess pid model of
                        Just process ->
                            let
                                (m2, newMsg) =
                                    .recv (impl.apply process) message pid
                                        |> Ctrl.map (updateInstanceIn model pid)
                            in
                            update impl newMsg m2

                        Nothing ->
                            ( model, Cmd.none )

                Ctrl.SendToSingleton actor message ->
                    case findSingletonPID actor model of
                        Just pid ->
                            update impl (Ctrl.sendToPID pid message) model

                        Nothing ->
                            update impl (Ctrl.spawnSingleton actor) model
                               |> andThen (update impl msg)

                Ctrl.Spawn actor replyMsg ->
                    let
                        ( m2, pid ) =
                            newPID model

                        ( m3, newMsg ) =
                            spawn impl actor pid m2
                    in
                    update impl newMsg m3
                        |> andThen (update impl (replyMsg pid))

                Ctrl.Kill ((PID _ pid) as p) ->
                    case Dict.get pid model.instances of
                        Just process ->
                            let
                                componentLastWords =
                                    .kill (impl.apply process) p
                            in
                            { model | instances = Dict.remove pid model.instances }
                                |> update impl componentLastWords

                        Nothing ->
                            ( model, Cmd.none )

                Ctrl.SpawnSingleton actor ->
                    let
                        ( m2, pid ) =
                            newPID model

                        ( m3, newMsg ) =
                            appendSingleton actor pid m2
                                |> spawn impl actor pid
                    in
                    update impl newMsg m3

                Ctrl.AddView pid ->
                    ( { model | views = pid :: model.views }
                    , Cmd.none
                    )


spawn : Impl actor process data a -> actor -> PID -> Model actor process -> ( Model actor process, Msg actor data )
spawn impl actor pid model =
    impl.spawn actor pid
        |> Ctrl.map (updateInstanceIn model pid)


newPID : Model actor process -> ( Model actor process, PID )
newPID model =
    model.lastPID
        |> PID model.prefix
        |> Tuple.pair ( { model | lastPID = 1 + model.lastPID } )


getProcess : PID -> Model actor process -> Maybe process
getProcess (PID _ pid) model =
    Dict.get pid model.instances

getInstanceFrom : Model actor process -> PID -> Maybe process
getInstanceFrom model (PID _ pid) =
    Dict.get pid model.instances


updateInstanceIn : Model actor process -> PID -> process -> Model actor process
updateInstanceIn model (PID _ pid) process =
    { model | instances = Dict.insert pid process model.instances }


appendSingleton : actor -> PID -> Model actor process -> Model actor process
appendSingleton actor pid model =
    { model
        | singleton = (actor, pid) :: model.singleton
    }


findSingletonPID : actor -> Model actor process -> Maybe PID
findSingletonPID actor model =
    model.singleton
        |> List.find ( \(a, _) -> a == actor )
        |> Maybe.map Tuple.second


subscriptions : Impl actor process data a -> Model actor process -> Sub (Msg actor data)
subscriptions impl model =
    model.instances
        |> Dict.foldl
            (\pid process subs ->
                let
                    sub =
                        .subs (impl.apply process) (PID model.prefix pid)
                in
                if sub == Sub.none then
                    subs
                else
                    sub::subs
            )
            []
        |> Sub.batch


view : Impl actor process data a -> Model actor process -> Html (Msg actor data)
view impl model =
    model.views ++ (List.map Tuple.second model.singleton)
        |> List.map (renderPID (\p -> .view (impl.apply p)) model.instances)
        |> Html.div []


renderPID : (process -> PID -> (PID -> Html msg) -> Html msg) -> Dict Int process -> PID -> Html msg
renderPID viewFn dict (PID prefix pid) =
    Dict.get pid dict
        |> Maybe.map
            ( \process ->
                viewFn process (PID prefix pid) (renderPID viewFn dict)
            )
        |> Maybe.withDefault (Html.text "")


andThen : (m -> ( m, Cmd msg )) -> ( m, Cmd msg ) -> ( m , Cmd msg )
andThen fn ( m0, cmd0 ) =
    let
        (m1, cmd1) =
            fn m0
    in
    ( m1, Cmd.batch [cmd0, cmd1] )
