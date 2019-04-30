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

@docs Set
    , PID
    , empty
    , insert
    , isEmpty
    , remove
    , toList
-}

import Set
import Webbhuset.Internal.PID as PID exposing (PID(..))


{-| A PID is an identifier for a Process.

A process is an instance of an Actor / Component
-}
type alias PID =
    PID.PID


{-| Store PIDs in a Set

-}
type Set
    = PIDSet (Set.Set ( String, Int ))


{-| Create an empty set

-}
empty : Set
empty =
    PIDSet Set.empty


{-| Insert PID in a Set

-}
insert : PID -> Set -> Set
insert (PID prefix pid) (PIDSet set) =
    Set.insert ( prefix, pid ) set
        |> PIDSet


{-| Remove a PID from a Set

-}
remove : PID -> Set -> Set
remove (PID prefix pid) (PIDSet set) =
    Set.remove ( prefix, pid ) set
        |> PIDSet


{-| Get all pids as a List

-}
toList : Set -> List PID
toList (PIDSet set) =
    Set.toList set
        |> List.map (\( prefix, id ) -> PID prefix id)


{-| Check if a Set is empty.

-}
isEmpty : Set -> Bool
isEmpty (PIDSet set) =
    Set.isEmpty set
