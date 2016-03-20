module Athena.Filter.Update (..) where

import Effects exposing (Effects)
import String exposing (trim)
import Athena.Filter.Model exposing (..)


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model, Effects.none )

    FilterTeacher user ->
      let
        isSet =
          model.filterByTeacher == Teacher user

        resultFilter =
          case isSet of
            True ->
              TeacherNoOp

            False ->
              Teacher user
      in
        ( { model | filterByTeacher = resultFilter }, Effects.none )

    FilterLocation location ->
      let
        isLocationCurrentlySet =
          model.filterByLocation == Location location

        resultLocationFilter =
          case isLocationCurrentlySet of
            True ->
              LocationNoOp

            False ->
              Location location
      in
        ( { model | filterByLocation = resultLocationFilter }, Effects.none )

    FilterCategory category ->
      let
        isFilterCurrentlySet =
          model.filterByCategory == Category category

        resultFilter =
          case isFilterCurrentlySet of
            True ->
              CategoryNoOp

            False ->
              Category category
      in
        ( { model | filterByCategory = resultFilter }, Effects.none )

    FilterTextSearch search ->
      let
        resultSearchFilter =
          case trim (search) == "" of
            True ->
              SearchNoOp

            False ->
              Search search
      in
        ( { model | filterBySearch = resultSearchFilter }, Effects.none )
