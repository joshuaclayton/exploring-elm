module Seed (initialModel, signal, update, Model, Action) where

import Time exposing (..)

type alias Model = { seed: Float }
type Action = Update Float

seedFps = 30
initialModel : Model
initialModel = { seed = 50000 }

update : Action -> Model -> Model
update action model =
  case action of
    Update newSeed -> { model | seed = newSeed }

signal : (Action -> a) -> Signal a
signal action =
  Signal.sampleOn (fps seedFps) ((Signal.map <| action) updateSeed)

updateSeed : Signal Action
updateSeed = (Signal.map <| Update) (every (millisecond*50))
