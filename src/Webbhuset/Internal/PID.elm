module Webbhuset.Internal.PID exposing (PID(..), Meta)


type PID
    = PID Meta


type alias Meta =
    { isSingleton : Bool
    , prefix : String
    , spawnedBy : Int
    , key : Int
    }
