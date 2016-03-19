module Athena.Filter.Model (..) where

import String exposing (toLower, contains)
import Athena.Model exposing (Topic, User)
import Debug


type Action
  = NoOp
  | FilterLocation String
  | FilterTextSearch String
  | FilterTeacher User


type LocationFilter
  = LocationNoOp
  | Location String


type SearchFilter
  = SearchNoOp
  | Search String


type TeacherFilter
  = TeacherNoOp
  | Teacher User


type alias Model =
  { filterByLocation : LocationFilter
  , filterBySearch : SearchFilter
  , filterByTeacher : TeacherFilter
  }


initialModel : Model
initialModel =
  { filterByLocation = LocationNoOp, filterBySearch = SearchNoOp, filterByTeacher = TeacherNoOp }


applyFilters : Topic -> Model -> Topic
applyFilters topic filter =
  topic
    |> filterByLocation filter.filterByLocation
    |> filterBySearch filter.filterBySearch
    |> filterByTeacher filter.filterByTeacher


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
