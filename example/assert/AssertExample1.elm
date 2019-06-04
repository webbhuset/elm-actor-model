module AssertExample1 exposing
    ( MsgIn(..)
    , MsgOut(..)
    , Model
    , component
    )

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Webbhuset.Component as Component exposing (PID)
import Webbhuset.Component.SystemEvent as SystemEvent exposing (SystemEvent)
import Webbhuset.PID.Set as PIDSet




type MsgIn
    = GoodButtonClicked
    | BadButtonClicked
    | ObserveSomething PID
    | ChangeSomething String


type MsgOut
    = SomethingGood Int
    | SomethingBad
    | SomethingWasChanged (List PID) String


type alias Model =
    { pid : PID
    , howGood : Int
    , something : String
    , observers : PIDSet.Set
    }


--
-- Component
--

component : Component.UI Model MsgIn MsgOut
component =
    { init = init
    , update = update
    , view = view
    , onSystem = onSystem
    , subs = subs
    }


init : PID -> ( Model , List MsgOut, Cmd MsgIn )
init pid =
    ( { pid = pid
      , howGood = 0
      , something = ""
      , observers = PIDSet.empty
      }
    , []
    , Cmd.none
    )


onSystem : SystemEvent -> SystemEvent.Handling MsgIn
onSystem event =
    SystemEvent.default


subs : Model -> Sub MsgIn
subs model =
    Sub.none


update : MsgIn -> Model -> ( Model, List MsgOut, Cmd MsgIn )
update msgIn model =
    case msgIn of
        GoodButtonClicked ->
            ( { model | howGood = model.howGood + 1 }
            , [ SomethingGood (model.howGood + 1)
              ]
            , Cmd.none
            )

        BadButtonClicked ->
            ( { model | howGood = model.howGood - 1 }
            , [ SomethingBad
              ]
            , Cmd.none
            )

        ObserveSomething pid ->
            ( { model | observers = PIDSet.insert pid model.observers }
            , [ SomethingWasChanged [ pid ] model.something
              ]
            , Cmd.none
            )

        ChangeSomething newThing ->
            ( { model | something = newThing }
            , [ SomethingWasChanged (PIDSet.toList model.observers) newThing
              ]
            , Cmd.none
            )

view : Model -> Html MsgIn
view model =
    Html.div
        [ HA.class "example"
        ]
        [ Html.node "style" [] [ Html.text css ]
        , Html.button
            [ Events.onClick GoodButtonClicked
            ]
            [ Html.text "Good button"
            ]
        , Html.button
            [ Events.onClick BadButtonClicked
            ]
            [ Html.text "Bad button"
            ]
        , Html.span
            []
            [ Html.text "My Status: "
            , Html.text
                (if model.howGood < 0 then
                    "This sucks!!!"
                else if model.howGood == 0 then
                    "Bad"
                else if model.howGood == 1 then
                    "Not that bad"
                else if model.howGood == 2 then
                    "Acceptable (lagom)"
                else if model.howGood == 3 then
                    "Good"
                else if model.howGood == 4 then
                    "Really Good"
                else
                    "Can't get better than this"
                )
            ]
        ]


css =
    """
.example > * {
    margin: 0.5rem;
}
"""
