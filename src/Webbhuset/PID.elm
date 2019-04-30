module Webbhuset.PID exposing
    ( PID
    , Set
    , emptySet
    , insert
    , isEmpty
    , null
    , remove
    , toList
    , toString
    )

{-| PIDs

## PID

@docs PID, toString, null

## Set of PID

@docs Set
    , emptySet
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


{-| Stringify PID

This is useful when you need to make a unique string id, for example
to use as html ids.

The string is unique for the component instance.

-}
toString : PID -> String
toString (PID prefix pid) =
    prefix ++ String.fromInt pid


{-| PID for testing purposes.

-}
null : PID
null =
    PID "null" 0



-- PID Set


{-| Create an empty set

-}
emptySet : Set
emptySet =
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
