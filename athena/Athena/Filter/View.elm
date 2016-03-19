module Athena.Filter.View (locationsFilter, searchFilter, teachersFilter) where

import Html exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (type', classList)
import Athena.Filter.Model exposing (..)
import Athena.Model exposing (User, fullName)


locationsFilter : Signal.Address Action -> List String -> Html
locationsFilter address locations =
  ul [] (List.map (\location -> li [ onClick address <| FilterLocation location ] [ text location ]) locations)


teachersFilter : Signal.Address Action -> List User -> Model -> Html
teachersFilter address teachers model =
  ul
    []
    (List.map
      (\teacher ->
        li
          [ classList [ ( "active", model.filterByTeacher == Teacher teacher ) ], onClick address <| FilterTeacher teacher ]
          [ text <| fullName teacher ]
      )
      teachers
    )


searchFilter : Signal.Address Action -> Html
searchFilter address =
  input [ type' "text", on "input" targetValue (Signal.message address << FilterTextSearch) ] []
