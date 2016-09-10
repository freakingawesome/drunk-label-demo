module Demo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Time exposing (Time, millisecond, second)
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


init : Random.Seed -> (Model, Cmd Msg)
init seed =
  let
    preview = DrunkLabel.defaultModel
    defaultText = "A quick brown fox jumps over the lazy dog"
  in
    { preview = { preview | value = defaultText }
    } ! []


-- UPDATE

type Msg
  = PreviewMsg DrunkLabel.Msg
  | SetSobriety String -- parsed to Float
  | SetBrashness String -- parsed to Float
  | SetMinWait String -- parsed to Float
  | SetMaxWait String -- parsed to Float
  | ToggleShowCursor
  | SetCursorBlinkInterval String -- parsed to Float

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
      SetMinWait str ->
        setPreviewFloat str model.preview.brashness (flip DrunkLabel.SetSpeed model.preview.maxWait)
      SetMaxWait str ->
        setPreviewFloat str model.preview.brashness (DrunkLabel.SetSpeed model.preview.minWait)
      ToggleShowCursor ->
        update (PreviewMsg <| DrunkLabel.ShowCursor <| not model.preview.showCursor) model
      SetCursorBlinkInterval str ->
        setPreviewFloat str model.preview.cursorBlinkInterval DrunkLabel.SetCursorBlinkInterval


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map PreviewMsg <| DrunkLabel.subscriptions model.preview


-- VIEW
view : Model -> Html Msg
view model =
  Html.div [] [
    Html.div [Html.Attributes.attribute "class" "row"] [
      Html.div [Html.Attributes.attribute "class" "col-sm-4"] [
        Html.h3 [] [Html.text "Sobriety"]
        , sliderView model.preview.sobriety SetSobriety 0.5 1

        , Html.h3 [] [Html.text "Brashness"]
        , sliderView model.preview.brashness SetBrashness 0 1

        , Html.h3 [] [Html.text "Min Wait Time"]
        , sliderView model.preview.minWait SetMinWait 0 model.preview.maxWait

        , Html.h3 [] [Html.text "Max Wait Time"]
        , sliderView model.preview.maxWait SetMaxWait model.preview.minWait (1 * second)

        , Html.h3 [] [
          Html.label [] [
            Html.input [Html.Attributes.attribute "type" "checkbox", (onClick ToggleShowCursor), (checked model.preview.showCursor)] [], Html.text "
            Show Cursor?
          "]
        ]

        , viewCursorBlinkInterval model

        , Html.textarea [Html.Attributes.attribute "rows" "3", Html.Attributes.attribute "cols" "80", (onInput (PreviewMsg << DrunkLabel.SetValue))] [Html.text model.preview.value]

        , Html.div [] [Html.a [(onClick (PreviewMsg <| DrunkLabel.SetValue kanye))] [Html.text "kanye"]]
      ]

      , Html.div [Html.Attributes.attribute "class" "col-sm-8"] [
        Html.pre [Html.Attributes.attribute "style" "font-size: 24px"] [App.map PreviewMsg <| DrunkLabel.view model.preview]
      ]
    ]
  ]


sliderView val msg min max =
  Html.div [] [
    Html.input [Html.Attributes.attribute "type" "range", Html.Attributes.attribute "min" (toString min), Html.Attributes.attribute "max" (toString max), Html.Attributes.attribute "step" "0.01", Html.Attributes.attribute "value" (toString val), (on "input" (Json.map msg targetValue))] []
  ]


viewCursorBlinkInterval model =
  if model.preview.showCursor
    then
      Html.div [] [
        Html.h3 [] [Html.text "Cursor Blink Interval"]
        , sliderView model.preview.cursorBlinkInterval SetCursorBlinkInterval 10 (1 * second)
      ]
    else
      Html.div [] []













-- Kanye's diggin' it
kanye =
  """McDonalds Man by Kanye West

McDonalds Man
The french fries had a plan
The french fries had a plan
The salad bar and the ketchup made a band
Cus the french fries had a plan
The french fries had a plan

McDonalds Man
McDonalds
I know them french fries have a plan
I know them french fries have a plan
The cheeseburger and the shakes formed a band
To overthrow the french fries plan
I always knew them french fries was evil man
Smelling all good and shit
I don't trust no food that smells that good man
I don't trust it
I just can't

McDonalds Man
McDonalds Man
McDonalds, damn
Them french fries look good tho
I knew the Diet Coke was jealous of the fries
I knew the McNuggets was jealous of the fries
Even the McRib was jealous of the fries
I could see it through his artificial meat eyes
And he only be there some of the time

Everybody was jealous of them french fries
Except for that one special guy
That smooth apple pie"""
