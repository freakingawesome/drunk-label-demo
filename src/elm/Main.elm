module Main exposing (..)

import Html.App as App
import Random
import Demo

main : Program Never
main =
  App.program
    { init = Demo.init (Random.initialSeed 0)
    , view = Demo.view
    , update = Demo.update
    , subscriptions = Demo.subscriptions
    }
