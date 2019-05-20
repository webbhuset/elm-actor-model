module Webbhuset.PID.Set exposing
    ( PID
    , Set
    , empty
    , insert
    , isEmpty
    , remove
    , toList
    )

{-|

## Set of PID

Since PIDs are not comparable they can not be used with the Set in elm/core.

When using an event/observer pattern in the actor model it is useful
to gather PIDs that are observing an event in a Set.

@docs Set
    , PID
    , empty
    , insert
    , isEmpty
    , remove
    , toList
-}

import Dict exposing (Dict)
import Webbhuset.Internal.PID as PID exposing (PID(..))


{-| A PID is an identifier for a Process.

A process is an instance of an Actor / Component
-}
type alias PID =
    PID.PID


{-| Store PIDs in a Set

-}
type Set
    = PIDSet (Dict Int PID)


{-| Create an empty set

-}
empty : Set
empty =
    PIDSet Dict.empty


{-| Insert PID in a Set

-}
insert : PID -> Set -> Set
insert ((PID { key } as pid)) (PIDSet dict) =
    Dict.insert key pid dict
        |> PIDSet


{-| Remove a PID from a Set

-}
remove : PID -> Set -> Set
remove (PID { key }) (PIDSet dict) =
    Dict.remove key dict
        |> PIDSet


{-| Get all pids as a List

-}
toList : Set -> List PID
toList (PIDSet dict) =
    Dict.values dict


{-| Check if a Set is empty.

-}
isEmpty : Set -> Bool
isEmpty (PIDSet dict) =
    Dict.isEmpty dict
