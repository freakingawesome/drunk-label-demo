module DrunkLabel exposing (Msg(SetValue), update, view)

import Html exposing (..)
import Html.App as App
import Time exposing (Time, millisecond)



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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetValue val ->
      { model | value = val, inProcess = "" } ! []


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  text model.value
