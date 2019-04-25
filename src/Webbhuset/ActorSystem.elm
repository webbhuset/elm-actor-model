module Webbhuset.ActorSystem
    exposing
        ( PID --Internal
        , Model
        , Msg(..)
        , Control(..)
        , Impl
        , AppliedActor
        -- CTRL
        , none
        , msgTo
        , batch
        , sendToPID
        , sendToSingleton
        , spawn
        , spawnSingleton
        , kill
        , addView
        -- System
        , element
        , application
        )

import Random
import List.Extra as List
import Dict exposing (Dict)
import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Url exposing (Url)
import Webbhuset.Internal.PID as PID exposing (PID(..))
import Task


type alias PID = PID.PID

-- Ctrl


type Msg actor msgTo
    = None
    | MsgTo msgTo
    | Ctrl (Control actor msgTo)
    | Init (Msg actor msgTo) String


type Control actor msgTo
    = Batch ( List (Msg actor msgTo) )
    | Cmd (Cmd (Msg actor msgTo))
    | Kill PID
    | SendToPID PID ( Msg actor msgTo )
    | SendToSingleton actor ( Msg actor msgTo )
    | Spawn actor ( PID -> Msg actor msgTo )
    | SpawnSingleton actor
    | AddView PID


--
-- Control
--

none : Msg actor msgTo
none =
    None


msgTo : msgTo -> Msg actor msgTo
msgTo d =
    MsgTo d


batch : List (Msg actor msgTo) -> Msg actor msgTo
batch list =
    Ctrl (Batch list)


sendToPID : PID -> Msg actor msgTo -> Msg actor msgTo
sendToPID pid msg =
    if msg == None then
        msg
    else
        Ctrl (SendToPID pid msg)


sendToSingleton : actor -> Msg actor msgTo -> Msg actor msgTo
sendToSingleton actor msg =
    if msg == None then
        msg
    else
        Ctrl (SendToSingleton actor msg)


spawn : actor -> ( PID -> Msg actor msgTo ) -> Msg actor msgTo
spawn actor replyMsg =
    Ctrl (Spawn actor replyMsg)


spawnSingleton : actor -> Msg actor msgTo
spawnSingleton actor =
    Ctrl (SpawnSingleton actor)


kill : PID -> Msg actor msgTo
kill pid =
    Ctrl (Kill pid)


addView : PID -> Msg actor msgTo
addView pid =
    Ctrl (AddView pid)


-- Sys


type alias Model actor process =
    { instances : Dict Int process
    , lastPID : Int
    , prefix : String
    , singleton : List ( actor, PID )
    , views : List PID
    }


type alias AppliedActor process msg =
    { init : PID -> (process, msg)
    , update : msg -> PID -> (process, msg)
    , view : PID -> (PID -> Html msg) -> Html msg
    , kill : PID -> msg
    , subs : PID -> Sub msg
    }

type alias Impl actor process msgTo a =
    { a
        | spawn : actor -> PID -> (process, Msg actor msgTo)
        , apply : process -> AppliedActor process (Msg actor msgTo)
    }

type alias ElementImpl flags actor process msgTo =
    Impl actor process msgTo
        { init : flags -> Msg actor msgTo
        }

type alias ApplicationImpl flags actor process msgTo =
    Impl actor process msgTo
        { init : flags -> Url -> Nav.Key -> Msg actor msgTo
        , onUrlRequest : Browser.UrlRequest -> (Msg actor msgTo)
        , onUrlChange : Url -> (Msg actor msgTo)
        }


element : ElementImpl flags actor process msgTo -> Program flags (Model actor process) (Msg actor msgTo)
element impl =
    Browser.element
        { init = initElement impl
        , update = update impl
        , subscriptions = subscriptions impl
        , view = view impl
        }


application : ApplicationImpl flags actor process msgTo -> Program flags (Model actor process) (Msg actor msgTo)
application impl =
    Browser.application
        { init = initApplication impl
        , update = update impl
        , subscriptions = subscriptions impl
        , view = view impl >> (\html -> { title = "", body = [ html ] } )
        , onUrlRequest = impl.onUrlRequest
        , onUrlChange = impl.onUrlChange
        }


initElement : ElementImpl flags actor process msgTo -> flags -> ( Model actor process, Cmd (Msg actor msgTo) )
initElement impl flags =
    Tuple.pair
        { instances = Dict.empty
        , lastPID = 100
        , prefix = ""
        , singleton = []
        , views = []
        }
        ( Random.generate
            (Init (impl.init flags))
            prefixGenerator
        )

initApplication :
    ApplicationImpl flags actor process msgTo
    -> flags
    -> Url
    -> Nav.Key
    -> ( Model actor process, Cmd (Msg actor msgTo) )
initApplication impl flags url key =
    Tuple.pair
        { instances = Dict.empty
        , lastPID = 100
        , prefix = ""
        , singleton = []
        , views = []
        }
        ( Random.generate
            (Init (impl.init flags url key))
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


update : Impl actor process msgTo a -> (Msg actor msgTo) -> Model actor process -> ( Model actor process, Cmd (Msg actor msgTo) )
update impl msg model =
    case msg of
        None ->
            (model, Cmd.none)

        MsgTo _ ->
            (model, Cmd.none)

        Init initMsg prefix ->
            { model | prefix = prefix }
                |> update impl initMsg

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
                    (model, cmd)

                SendToPID pid message ->
                    case getProcess pid model of
                        Just process ->
                            let
                                (m2, newMsg) =
                                    .update (impl.apply process) message pid
                                        |> Tuple.mapFirst (updateInstanceIn model pid)
                            in
                            update impl newMsg m2

                        Nothing ->
                            ( model, Cmd.none )

                SendToSingleton actor message ->
                    case findSingletonPID actor model of
                        Just pid ->
                            update impl (sendToPID pid message) model

                        Nothing ->
                            update impl (spawnSingleton actor) model
                               |> cmdAndThen (update impl msg)

                Spawn actor replyMsg ->
                    let
                        ( m2, pid ) =
                            newPID model

                        ( m3, newMsg ) =
                            spawn_ impl actor pid m2
                    in
                    update impl newMsg m3
                        |> cmdAndThen (update impl (replyMsg pid))

                Kill ((PID _ pid) as p) ->
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

                SpawnSingleton actor ->
                    let
                        ( m2, pid ) =
                            newPID model

                        ( m3, newMsg ) =
                            appendSingleton actor pid m2
                                |> spawn_ impl actor pid
                    in
                    update impl newMsg m3

                AddView pid ->
                    ( { model | views = pid :: model.views }
                    , Cmd.none
                    )


spawn_ : Impl actor process msgTo a -> actor -> PID -> Model actor process -> ( Model actor process, Msg actor msgTo )
spawn_ impl actor pid model =
    impl.spawn actor pid
        |> Tuple.mapFirst (updateInstanceIn model pid)


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


subscriptions : Impl actor process msgTo a -> Model actor process -> Sub (Msg actor msgTo)
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


view : Impl actor process msgTo a -> Model actor process -> Html (Msg actor msgTo)
view impl model =
    model.views ++ (List.map Tuple.second model.singleton)
        |> List.map (renderPID (\p -> .view (impl.apply p)) model.instances)
        |> Html.div []


renderPID : (process -> PID -> (PID -> Html msg) -> Html msg) -> Dict Int process -> PID -> Html msg
renderPID viewFn dict ((PID prefix pid) as p) =
    Dict.get pid dict
        |> Maybe.map
            ( \process ->
                viewFn process p (renderPID viewFn dict)
            )
        |> Maybe.withDefault (Html.text "")


cmdAndThen : (m -> ( m, Cmd msg )) -> ( m, Cmd msg ) -> ( m , Cmd msg )
cmdAndThen fn ( m0, cmd0 ) =
    let
        (m1, cmd1) =
            fn m0
    in
    ( m1, Cmd.batch [cmd0, cmd1] )
