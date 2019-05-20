module Webbhuset.PID exposing
    ( PID
    , null
    , toString
    )

{-|

## PID

@docs PID, toString, null

-}

import Webbhuset.Internal.PID as PID exposing (PID(..))


{-| A PID is an identifier for a Process.

A process is an instance of an Actor / Component
-}
type alias PID =
    PID.PID


{-| Stringify PID

This is useful when you need to make a unique string id, for example
to use as html ids.

The string is unique for the component instance.

-}
toString : PID -> String
toString (PID { prefix, key }) =
    prefix ++ String.fromInt key


{-| PID for testing purposes.

-}
null : PID
null =
    PID
        { isSingleton = False
        , prefix = "null"
        , key = 0
        }

