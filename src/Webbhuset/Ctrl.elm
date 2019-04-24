module Webbhuset.Ctrl exposing (..)

import Task

type Msg pid actor msgTo
    = None
    | MsgTo msgTo
    | Ctrl (Control pid actor msgTo)
    | Init (Msg pid actor msgTo) String


type Control pid actor msgTo
    = Batch ( List (Msg pid actor msgTo) )
    | Cmd (Cmd (Msg pid actor msgTo))
    | Kill pid
    | SendToPID pid ( Msg pid actor msgTo )
    | SendToSingleton actor ( Msg pid actor msgTo )
    | Spawn actor ( pid -> Msg pid actor msgTo )
    | SpawnSingleton actor
    | AddView pid


--
-- Control
--

none : Msg pid actor msgTo
none =
    None


msgTo : msgTo -> Msg pid actor msgTo
msgTo d =
    MsgTo d


batch : List (Msg pid actor msgTo) -> Msg pid actor msgTo
batch list =
    Ctrl (Batch list)


cmd : Cmd (Msg pid actor msgTo) -> Msg pid actor msgTo
cmd c =
    Ctrl (Cmd c)


sendToPID : pid -> Msg pid actor msgTo -> Msg pid actor msgTo
sendToPID pid msg =
    if msg == None then
        msg
    else
        Ctrl (SendToPID pid msg)


sendToSingleton : actor -> Msg pid actor msgTo -> Msg pid actor msgTo
sendToSingleton actor msg =
    if msg == None then
        msg
    else
        Ctrl (SendToSingleton actor msg)


spawn : actor -> ( pid -> Msg pid actor msgTo ) -> Msg pid actor msgTo
spawn actor replyMsg =
    Ctrl (Spawn actor replyMsg)


spawnSingleton : actor -> Msg pid actor msgTo
spawnSingleton actor =
    Ctrl (SpawnSingleton actor)


kill : pid -> Msg pid actor msgTo
kill pid =
    Ctrl (Kill pid)


addView : pid -> Msg pid actor msgTo
addView pid =
    Ctrl (AddView pid)

--
-- Composing Messages
--

noMsg : a -> ( a, Msg pid actor msgTo)
noMsg a =
    ( a, none )


map : (a -> b) -> ( a, Msg pid actor msgTo ) -> ( b, Msg pid actor msgTo )
map fn ( a, msg ) =
    ( fn a, msg )


andThen : (a -> ( b, Msg pid actor msgTo )) -> ( a, Msg pid actor msgTo ) -> ( b, Msg pid actor msgTo )
andThen fn ( a, msg0 ) =
    let
        ( b, msg1 ) =
            fn a
    in
        ( b, append msg0 msg1 )


append : Msg pid actor msgTo -> Msg pid actor msgTo -> Msg pid actor msgTo
append msg1 msg2 =
    if msg1 == None then
        msg2
    else if msg2 == None then
        msg1
    else
        Ctrl (Batch [ msg1, msg2 ])


appendMsg : Msg pid actor msgTo -> (a, Msg pid actor msgTo) -> (a, Msg pid actor msgTo)
appendMsg msg1 ( a, msg0 ) =
    ( a, append msg0 msg1 )


toCmd : Msg pid actor msgTo -> Cmd (Msg pid actor msgTo)
toCmd msg =
    if msg == None then
        Cmd.none
    else
        Task.perform
            identity
            (Task.succeed msg)
