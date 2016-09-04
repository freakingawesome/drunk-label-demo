module DrunkTyper exposing (Direction(..), drunkTyper)

import Time exposing (Time, millisecond)
import Char
import String
import Random
import List exposing (..)
import List.Extra exposing (..)

type TypedKey
  = Untyped Char
  | Matched Char
  | Wrong Char Char -- 1: Expected, 2: Typed
  | Excess Char

type Direction
  = Forward
  | Backward

-- This doesn't belong here... refactor!
type alias Model =
  { value : String
  , inProcess : String
  , sobriety : Float
  , brashness : Float
  , nextSeed : Random.Seed
  , nextWait : Time
  , dir : Direction
  }


drunkTyper : Model -> (String, Direction, Random.Seed)
drunkTyper model =
  let
    typedKeys = toTypedKeys model.value model.inProcess
    numWrong =
      length
        <| filter (\x ->
          case x of
            Wrong _ _ -> True
            _ -> False
          ) typedKeys
    (drunked, nextSeed') =
      case model.dir of
        Forward ->
          appendNextLetter typedKeys model
        Backward ->
          Maybe.withDefault [] (List.Extra.init (String.toList model.inProcess))
            |> String.fromList
            |> flip (,) model.nextSeed
    (dir, nextSeed'') =
        case model.dir of
          Forward ->
            if numWrong == 0
              then (Forward, nextSeed')
              else Random.step (Random.map (\f -> if f > model.brashness then Backward else Forward) (Random.float 0 1)) nextSeed'
          Backward ->
            if numWrong == 0
              then (Forward, nextSeed')
              else (Backward, nextSeed')
  in
    (drunked, dir, nextSeed'')

appendNextLetter : List TypedKey -> Model -> (String, Random.Seed)
appendNextLetter typedKeys model =
  let
    (accuracy, nextSeed) =
      Random.step (Random.float 0 1) model.nextSeed
    (randChar, nextSeed') =
      Random.step (Random.map (Char.fromCode) (Random.int 48 122)) nextSeed
    filterTyped x =
      case x of
        Matched c -> Just c
        Wrong _ c -> Just c
        _ -> Nothing
    skipTyped =
      filter (\x ->
        case x of
          Untyped c -> True
          _ -> False)
    nextLetter =
      case head <| skipTyped typedKeys of
        Just (Untyped c) ->
          if accuracy > model.sobriety
            then [randChar]
            else [c]
        _ -> []
  in
    filterMap filterTyped typedKeys ++ nextLetter
      |> String.fromList
      |> flip (,) nextSeed

toTypedKeys : String -> String -> List TypedKey
toTypedKeys expected current =
  let
    typedKey entry =
      case entry of
        First c -> Untyped c
        Second c -> Excess c
        Both exp cur -> if exp == cur then Matched exp else Wrong exp cur
  in
    zipAll (String.toList expected) (String.toList current)
      |> List.map typedKey

type FullZipItem a b
  = First a
  | Second b
  | Both a b

zipAll : List a -> List b -> List (FullZipItem a b)
zipAll a b =
  case (a, b) of
    ([], []) -> []
    (x::xs, []) -> First x :: zipAll xs []
    ([], y::ys) -> Second y :: zipAll [] ys
    (x::xs, y::ys) -> Both x y :: zipAll xs ys


