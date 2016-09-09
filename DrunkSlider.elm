module DrunkSlider exposing (..) -- exposing (Msg(SetValue), update, view)

import Html exposing (..)
import Html.Attributes as Att exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Time exposing (Time, millisecond)
import Char
import String
import Random
import List exposing (..)
import List.Extra exposing (..)
import DrunkLabel
import Json.Decode as Json
import Update.Extra.Infix exposing (..)


main =
  App.program
    { init = init (Random.initialSeed 0)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { preview : DrunkLabel.Model
  }

previewText = "The quick brown fox jumps over the lazy dog" 

init : Random.Seed -> (Model, Cmd Msg)
init seed =
  let preview = DrunkLabel.defaultModel
  in
    { preview = { preview | value = previewText}
    } ! []


-- UPDATE

type Msg
  = PreviewMsg DrunkLabel.Msg
  | SetSobriety String -- parsed to Float
  | SetBrashness String -- parsed to Float

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    setPreviewFloat str default submsg =
      let
        fval =
          case String.toFloat str of
            Ok f -> f
            Err _ -> default
      in
        update (PreviewMsg (submsg fval)) model
  in
    case msg of
      PreviewMsg submsg ->
        let (preview, cmd') = DrunkLabel.update submsg model.preview
        in { model | preview = preview } ! [ Cmd.map PreviewMsg cmd' ]
      SetSobriety str ->
        setPreviewFloat str model.preview.sobriety DrunkLabel.SetSobriety
      SetBrashness str ->
        setPreviewFloat str model.preview.brashness DrunkLabel.SetBrashness


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map PreviewMsg <| DrunkLabel.subscriptions model.preview


-- VIEW

view : Model -> Html Msg
view model =
  div [ style [("font-family", "sans-serif")]]
    [ h3 [] [ text "Sobriety" ]
    , sliderView model.preview.sobriety SetSobriety 0.5 1
    , h3 [] [ text "Brashness" ]
    , sliderView model.preview.brashness SetBrashness 0 1
    , pre [ style [("font-size", "24px")] ] [ App.map PreviewMsg <| DrunkLabel.view model.preview ]
    ]


sliderView val msg min max =
  div []
    [ input
      [ on "input" (Json.map msg targetValue)
      , type' "range"
      , Att.min <| toString min
      , Att.max <| toString max
      , Att.step "0.05"
      , value (toString val)
      ] []
    ]

