module DrunkLabel exposing (
  Model,
  defaultModel,
  Msg
    ( SetValue
    , SetSobriety
    , SetBrashness
    , SetSpeed
    , ShowCursor
    , SetCursorBlinkInterval
    ),
  update,
  view,
  subscriptions)

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

type alias Model = DrunkTyper.Model

defaultModel =
  { value = ""
  , inProcess = ""
  , sobriety = 1
  , brashness = 0
  , nextSeed = Random.initialSeed 0
  , nextWait = 50 * millisecond
  , minWait = 30 * millisecond
  , maxWait = 200 * millisecond
  , dir = Forward
  , showCursor = True
  , cursorOn = False
  , cursorBlinkInterval = 500 * millisecond
  }

init : Random.Seed -> (Model, Cmd Msg)
init seed =
  { defaultModel | nextSeed = seed } ! []


-- UPDATE

type Msg
  = SetValue String
  | SetSobriety Float
  | SetBrashness Float
  | SetSpeed Time Time
  | ShowCursor Bool
  | SetCursorBlinkInterval Time
  | ToggleCursor
  | NextKey

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetValue val ->
      { model | value = val, dir = Backward True } ! []
    SetSobriety val ->
      { model | sobriety = val, dir = Backward True } ! []
    SetBrashness val ->
      { model | brashness = val, dir = Backward True } ! []
    SetSpeed min max ->
      { model | minWait = min, maxWait = max, dir = Backward True } ! []
    ToggleCursor ->
      { model | cursorOn = model.showCursor && not model.cursorOn } ! []
    ShowCursor show ->
      { model | showCursor = show } ! []
    SetCursorBlinkInterval val ->
      { model | cursorBlinkInterval = val } ! []
    NextKey ->
      let
        (nextText, dir, nextSeed) = drunkTyper model
        (nextWait, nextSeed') = Random.step (Random.float model.minWait model.maxWait) nextSeed
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
  let
    typing =
      case model.dir of
        Backward True -> Time.every (min model.maxWait <| 50 * millisecond) (always NextKey)
        _ ->
          if model.value == model.inProcess
            then Sub.none
            else Time.every model.nextWait (always NextKey)
    cursorBlinking =
      if model.showCursor
        then Time.every model.cursorBlinkInterval (always ToggleCursor)
        else Sub.none
  in
    Sub.batch [ typing, cursorBlinking ]


-- VIEW

view : Model -> Html Msg
view model =
  let
    cursor =
      if model.showCursor && model.cursorOn
        then cursorCharacter
        else ""
  in
    text <| model.inProcess ++ cursor

cursorCharacter : String
cursorCharacter =
  String.fromList [ Char.fromCode 9608 ]

