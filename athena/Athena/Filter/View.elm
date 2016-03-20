module Athena.Filter.View (locationsFilter, searchFilter, teachersFilter, categoriesFilter) where

import Html exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (type', classList)
import Athena.Filter.Model exposing (..)
import Athena.Model exposing (User, fullName)


locationsFilter : Signal.Address Action -> List String -> Html
locationsFilter address locations =
  div
    []
    [ h2 [] [ text "Filter by Location" ]
    , ul [] (List.map (locationFilter address) locations)
    ]


locationFilter : Signal.Address Action -> String -> Html
locationFilter address location =
  li [ onClick address <| FilterLocation location ] [ text location ]


categoriesFilter : Signal.Address Action -> List String -> Html
categoriesFilter address categories =
  div
    []
    [ h2 [] [ text "Filter by Category" ]
    , ul [] (List.map (categoryFilter address) categories)
    ]


categoryFilter : Signal.Address Action -> String -> Html
categoryFilter address category =
  li [ onClick address <| FilterCategory category ] [ text category ]


teachersFilter : Signal.Address Action -> List User -> Model -> Html
teachersFilter address teachers model =
  div
    []
    [ h2 [] [ text "Filter by Teacher" ]
    , ul [] (List.map (teacherFilter address model) teachers)
    ]


teacherFilter : Signal.Address Action -> Model -> User -> Html
teacherFilter address model teacher =
  li
    [ classList [ ( "active", model.filterByTeacher == Teacher teacher ) ]
    , onClick address <| FilterTeacher teacher
    ]
    [ text <| fullName teacher ]


searchFilter : Signal.Address Action -> Html
searchFilter address =
  input [ type' "text", on "input" targetValue (Signal.message address << FilterTextSearch) ] []
