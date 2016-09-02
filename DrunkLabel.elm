module DrunkLabel exposing (Msg(SetValue), update, view)

import Html exposing (..)
import Html.App as App
import Time exposing (Time, millisecond)
import String


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { value : String
  , inProcess : String
  }


init : (Model, Cmd Msg)
init =
  { value = "Hello World!", inProcess = "" } ! []


-- UPDATE

type Msg
  = SetValue String
  | NextLetter

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetValue val ->
      { model | value = val, inProcess = "" } ! []
    NextLetter ->
      let
        cur = String.toList model.inProcess
        exp = String.toList model.value
        next = List.head <| List.drop (List.length cur) exp
        nextModel =
          case next of
            Nothing -> model
            Just c -> { model | inProcess = String.fromList (cur ++ [c]) }
      in
        nextModel ! []


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.value == model.inProcess
    then Sub.none
    else Time.every (50 * millisecond) (always NextLetter)


-- VIEW

view : Model -> Html Msg
view model =
  text model.inProcess

