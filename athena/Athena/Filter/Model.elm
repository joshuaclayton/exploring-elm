module Athena.Filter.Model (..) where

import String exposing (toLower, contains)
import Athena.Model exposing (Topic, User)
import Debug


type Action
  = NoOp
  | FilterLocation String
  | FilterTextSearch String
  | FilterTeacher User
  | FilterCategory String


type LocationFilter
  = LocationNoOp
  | Location String


type SearchFilter
  = SearchNoOp
  | Search String


type TeacherFilter
  = TeacherNoOp
  | Teacher User


type CategoryFilter
  = CategoryNoOp
  | Category String


type alias Model =
  { filterByLocation : LocationFilter
  , filterBySearch : SearchFilter
  , filterByTeacher : TeacherFilter
  , filterByCategory : CategoryFilter
  }


initialModel : Model
initialModel =
  { filterByLocation = LocationNoOp
  , filterBySearch = SearchNoOp
  , filterByTeacher = TeacherNoOp
  , filterByCategory = CategoryNoOp
  }


applyFilters : Topic -> Model -> Topic
applyFilters topic filter =
  topic
    |> filterByLocation filter.filterByLocation
    |> filterBySearch filter.filterBySearch
    |> filterByTeacher filter.filterByTeacher
    |> filterByCategory filter.filterByCategory


filterByLocation : LocationFilter -> Topic -> Topic
filterByLocation filter topic =
  let
    filterTeachingMove teachingMove =
      case filter of
        Location locationName ->
          teachingMove.location_in_topic == locationName

        LocationNoOp ->
          True

    teachingMoves =
      List.filter filterTeachingMove topic.teaching_moves
  in
    { topic | teaching_moves = teachingMoves }


containsValue : String -> String -> Bool
containsValue searchTerm container =
  let
    lowerContainer =
      container |> toLower

    lowerTerm =
      searchTerm |> toLower
  in
    lowerTerm `contains` lowerContainer


filterBySearch : SearchFilter -> Topic -> Topic
filterBySearch filter topic =
  let
    filterTeachingMove teachingMove =
      case filter of
        Search search ->
          List.any (containsValue search) [ teachingMove.item.name, teachingMove.item.description ]

        SearchNoOp ->
          True

    teachingMoves =
      List.filter filterTeachingMove topic.teaching_moves
  in
    { topic | teaching_moves = teachingMoves }


filterByTeacher : TeacherFilter -> Topic -> Topic
filterByTeacher filter topic =
  let
    filterTeachingMove teachingMove =
      case filter of
        Teacher user ->
          teachingMove.item.user == user

        TeacherNoOp ->
          True

    teachingMoves =
      List.filter filterTeachingMove topic.teaching_moves
  in
    { topic | teaching_moves = teachingMoves }


filterByCategory : CategoryFilter -> Topic -> Topic
filterByCategory filter topic =
  let
    filterTeachingMove teachingMove =
      case filter of
        Category categoryName ->
          teachingMove.item.category == categoryName

        CategoryNoOp ->
          True

    teachingMoves =
      List.filter filterTeachingMove topic.teaching_moves
  in
    { topic | teaching_moves = teachingMoves }
