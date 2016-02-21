import StartApp
import Effects exposing (Never, Effects)
import Html exposing (..)
import Seed exposing (..)
import Stick exposing (..)

type Action = NoOp
  | HandleSeed Seed.Action
  | HandleStick Stick.Action

type alias Model = { seed: Seed.Model, stick: Stick.Model }

noEffects : a -> (a, Effects b)
noEffects thing = (thing, Effects.none)

init = noEffects { seed = Seed.initialModel, stick = Stick.initialModel }

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> noEffects model
    HandleSeed act ->
      let newSeed = Seed.update act model.seed
      in noEffects { model | seed = newSeed, stick = (Stick.reseed model.stick (round newSeed.seed)) }
    HandleStick act -> noEffects { model | stick = Stick.update act model.stick }

inputs : List (Signal Action)
inputs = [Seed.signal HandleSeed]

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ text ("Hello world: " ++ (model.seed.seed |> toString))
    , Stick.view (Signal.forwardTo address HandleStick) model.stick
    ]

app = StartApp.start { init = init, view = view, update = update, inputs = inputs }

main = app.html
