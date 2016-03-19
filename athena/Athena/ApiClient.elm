module Athena.ApiClient (getTopics) where

import Effects exposing (Effects)
import Http
import Task
import JsonApi exposing (jsonApiBody)
import Athena.BaseModel exposing (..)


getTopics : Effects Action
getTopics =
  Http.get jsonApiBody "https://api.teachathena.org/api/topics?filter[search]=Be&include=teaching-moves.item.user,teaching-moves.item.comments.user,teaching-moves.item.comments.comments.user"
    |> Task.toMaybe
    |> Task.map NewResponse
    |> Effects.task
