import StartApp
import Effects exposing (Never, Effects)
import Html exposing (..)
import Seed exposing (..)

type Action =
  NoOp |
  HandleSeed Seed.Action

type alias Model = { seed: Seed.Model }

noEffects : a -> (a, Effects b)
noEffects thing = (thing, Effects.none)

init = noEffects { seed = Seed.initialModel }

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> noEffects model
    HandleSeed act -> noEffects { model | seed = Seed.update act model.seed }

inputs : List (Signal Action)
inputs =
  [Seed.signal HandleSeed]

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ text ("Hello world: " ++ (model.seed.seed |> toString)) ]

app = StartApp.start { init = init, view = view, update = update, inputs = inputs }

main = app.html
