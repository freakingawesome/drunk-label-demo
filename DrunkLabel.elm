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

kanye =
  """McDonalds Man 
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
I don’t trust no food that smells that good man 
I don’t trust it 
I just can’t 
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
That smooth apple pie
  """

init : Random.Seed -> (Model, Cmd Msg)
init seed =
  { value = kanye
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
  pre [] [ text model.inProcess ]


