module Webbhuset.PID
    exposing
        ( PID
        , Set
        , toString
        , null
        , emptySet
        , insert
        , remove
        , toList
        )

import Webbhuset.Internal.PID as PID exposing (PID(..))
import Set


type alias PID = PID.PID


type Set =
    PIDSet (Set.Set ( String, Int ))


toString : PID -> String
toString (PID prefix pid) =
    prefix ++ (String.fromInt pid)


null : PID
null =
    PID "null" 0


-- PID Set

emptySet : Set
emptySet =
    PIDSet Set.empty


insert : PID -> Set -> Set
insert (PID prefix pid) (PIDSet set) =
    Set.insert ( prefix, pid ) set
        |> PIDSet


remove : PID -> Set -> Set
remove (PID prefix pid) (PIDSet set) =
    Set.remove ( prefix, pid ) set
        |> PIDSet


toList : Set -> List PID
toList (PIDSet set) =
    Set.toList set
        |> List.map (\(prefix, id) -> PID prefix id)
