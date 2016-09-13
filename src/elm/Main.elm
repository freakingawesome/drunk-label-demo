module Main exposing (..)

import Html.App as App
import Demo

main : Program Flags
main =
  App.programWithFlags
    { init = \flags -> Demo.init flags.initialSeed
    , view = Demo.view
    , update = Demo.update
    , subscriptions = Demo.subscriptions
    }

type alias Flags =
  { initialSeed: Int }
