module DrunkLabel exposing (Msg(SetValue), update, view)

import Html exposing (..)
import Html.App as App
import Time exposing (Time, millisecond)
import Char
import String
import Random
import List exposing (..)
import List.Extra exposing (..)
import DrunkTyper exposing (..)


main =
  App.program
    { init = init (Random.initialSeed 0)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { value : String
  , inProcess : String
  , sobriety : Float
  , brashness : Float
  , nextSeed : Random.Seed
  , nextWait : Time
  , dir : Direction
  }


init : Random.Seed -> (Model, Cmd Msg)
init seed =
  { value = "A long, long, time ago, I can still remember how that music used to make me smile"
  , inProcess = ""
  , sobriety = 0.95
  , brashness = 0.8
  , nextSeed = seed
  , nextWait = 50 * millisecond
  , dir = Forward
  } ! []


-- UPDATE

type Msg
  = SetValue String
  | NextKey

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetValue val ->
      { model | value = val, inProcess = "" } ! []
    NextKey ->
      let
        (nextText, dir, nextSeed) = drunkTyper model
        (nextWait, nextSeed') = Random.step (Random.float 30 200) nextSeed
      in
        { model
          | inProcess = nextText
          , nextSeed = nextSeed'
          , nextWait = nextWait
          , dir = dir
        } ! []


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.value == model.inProcess
    then Sub.none
    else Time.every model.nextWait (always NextKey)


-- VIEW

view : Model -> Html Msg
view model =
  h1 [] [ text model.inProcess ]


