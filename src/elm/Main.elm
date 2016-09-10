module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Att exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Random
import Demo

main =
  App.program
    { init = Demo.init (Random.initialSeed 0)
    , view = Demo.view
    , update = Demo.update
    , subscriptions = Demo.subscriptions
    }
