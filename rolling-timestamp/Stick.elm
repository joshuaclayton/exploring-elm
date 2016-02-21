module Stick (Model, Action, initialModel, reseed, view, update) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (..)

type Action = NoOp | Next
type Rotation = SlantRight | SlantLeft | NoSlant
type Color = Red | Orange | Yellow | Green | Blue | Violet

type alias Model = { seed: Seed, list: List (Color, Rotation) }

initialModel : Model
initialModel =
  let initialSeed' = initialSeed 50000
      (list, seed') = buildList initialSeed'
  in { seed = initialSeed', list = list }

update action model =
  let (list, seed') = buildList model.seed
  in case action of
    NoOp -> model
    Next -> { model | seed = seed', list = list }

reseed : Model -> Int -> Model
reseed model float =
  { model | seed = initialSeed float }

view address model =
  div []
    [ sticks model.list
    , button [ onClick address Next ] [ text "Next" ]
    ]

stick : (Color, Rotation) -> Html
stick (color, rotation) =
  let color' = colorToString color
      rotation' = rotationToString rotation
  in div [class "stick"] [text (color' ++ "-" ++ rotation')]

sticks : List (Color, Rotation) -> Html
sticks list =
  div [] (List.map stick list)

colorToString color =
  case color of
    Red -> "red"
    Orange -> "orange"
    Yellow -> "yellow"
    Green -> "green"
    Blue -> "blue"
    Violet -> "violet"

rotationToString rotation =
  case rotation of
    SlantRight -> "slant-right"
    SlantLeft -> "slant-left"
    NoSlant -> "no-slant"

buildListOfRandomValues : Seed -> Int -> Int -> List Int
buildListOfRandomValues seed count upperLimit =
  let (list', seed') = generate (Random.list count (int 0 upperLimit)) seed
  in list'

randomColor : Seed -> Int -> List Color
randomColor seed count =
  let result = buildListOfRandomValues seed count 5
      calculate result' = case result' of
        0 -> Red
        1 -> Orange
        2 -> Yellow
        3 -> Green
        4 -> Blue
        5 -> Violet
        _ -> Red
  in List.map calculate result

randomRotation : Seed -> Int -> List Rotation
randomRotation seed count =
  let result = buildListOfRandomValues seed count 2
      calculate result' = case result' of
        0 -> SlantRight
        1 -> SlantLeft
        2 -> NoSlant
        _ -> NoSlant
  in List.map calculate result

randomBoardSize : Seed -> (Int, Seed)
randomBoardSize seed =
  let (result, seed') = generate (int 0 3) seed
  in (4 + result, seed')

buildList : Seed -> (List (Color, Rotation), Seed)
buildList seed =
  let (listSize, seed') = randomBoardSize seed
      list = zip (randomColor seed listSize) (randomRotation seed listSize)
  in (list, seed')

zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) -> (x,y) :: zip xs' ys'
    (_, _) -> []
