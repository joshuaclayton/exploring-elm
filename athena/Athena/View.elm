module Athena.View (view) where

import Html exposing (..)
import Athena.BaseModel exposing (..)
import Athena.Model exposing (..)
import Athena.Filter.View
import Athena.Filter.Model
import Athena.ListExtras exposing (..)


view : Signal.Address Action -> Model -> Html
view address model =
  div [] [ Athena.Filter.View.searchFilter (Signal.forwardTo address HandleFilter), renderTopics address model ]


renderTopics : Signal.Address Action -> Model -> Html
renderTopics address model =
  case model.topics of
    [] ->
      h2 [] [ text "Loading topics..." ]

    _ ->
      ul [] (List.map (renderTopic address model) model.topics)


teachersFor : Topic -> List User
teachersFor topic =
  uniqBy .id (List.map (.item >> .user) topic.teaching_moves)


renderTopic : Signal.Address Action -> Model -> Topic -> Html
renderTopic address model topic =
  let
    topic' =
      Athena.Filter.Model.applyFilters topic model.filters
  in
    div
      []
      [ h2 [] [ text (topic.name ++ " (id: " ++ topic.id ++ ")") ]
      , Athena.Filter.View.locationsFilter (Signal.forwardTo address HandleFilter) topic.locations
      , Athena.Filter.View.teachersFilter (Signal.forwardTo address HandleFilter) (teachersFor topic) model.filters
      , ul [] (List.map renderTeachingMove topic'.teaching_moves)
      , hr [] []
      ]


renderTeachingMove : TeachingMove -> Html
renderTeachingMove teachingMove =
  li
    []
    [ text (teachingMove.line_number ++ " - " ++ teachingMove.location_in_topic)
    , renderItem teachingMove teachingMove.item
    ]


renderItem : TeachingMove -> Item -> Html
renderItem teachingMove item =
  let
    fullName user =
      user.first_name ++ " " ++ user.last_name
  in
    div
      []
      [ h2 [] [ text teachingMove.item.name ]
      , p [] [ text ("by " ++ (fullName item.user)) ]
      , p [] [ text item.description ]
      , (renderComments item.comments)
      ]


renderComments : List Comment -> Html
renderComments comments =
  case comments of
    [] ->
      div [] []

    _ ->
      div
        []
        [ h3 [] [ text ("Comments (" ++ (comments |> List.length |> toString) ++ ")") ]
        , ul [] (List.map renderComment comments)
        ]


renderComment : Comment -> Html
renderComment comment =
  li
    []
    [ text comment.body
    , i [] [ text (comment.user |> fullName) ]
    , renderReplies comment.comments
    ]


renderReplies : Responses -> Html
renderReplies comments =
  case comments of
    Responses [] ->
      span [] []

    Responses comments' ->
      ul
        []
        (List.map renderComment comments')
