module Athena.ListExtras (groupBy, uniqBy) where

import Set exposing (fromList, toList)


groupBy : (a -> comparable) -> List a -> List ( comparable, List a )
groupBy f list =
  let
    uniqueValues =
      (fromList >> toList) <| List.map f list
  in
    List.map
      (\value ->
        ( value, List.filter (\i -> f i == value) list )
      )
      uniqueValues


uniqBy : (a -> comparable) -> List a -> List a
uniqBy f list =
  groupBy f list
    |> List.map (\( k, v ) -> v |> List.head)
    |> List.filterMap identity
