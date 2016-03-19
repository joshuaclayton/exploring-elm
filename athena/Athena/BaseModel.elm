module Athena.BaseModel (Model, Action(..), initialModel, update) where

import Effects exposing (Effects)
import Athena.Decode exposing (..)
import JsonApi exposing (JsonApiBody)
import Athena.Model exposing (Topic)
import Athena.Filter.Model
import Athena.Filter.Update


noEffects : a -> ( a, Effects b )
noEffects thing =
  ( thing, Effects.none )


type Action
  = NoOp
  | NewResponse (Maybe JsonApiBody)
  | HandleFilter Athena.Filter.Model.Action


type alias Model =
  { topics : List Topic, filters : Athena.Filter.Model.Model }


initialModel : Model
initialModel =
  { topics = [], filters = Athena.Filter.Model.initialModel }


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      noEffects model

    NewResponse data ->
      noEffects { model | topics = processTopics data }

    HandleFilter act ->
      let
        ( newFilter, effects ) =
          Athena.Filter.Update.update act model.filters
      in
        noEffects { model | filters = newFilter }
