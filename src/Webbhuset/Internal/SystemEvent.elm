module Webbhuset.Internal.SystemEvent exposing (Handling(..))


type Handling msgIn
    = Default
    | DoNothing
    | HandleWith msgIn
