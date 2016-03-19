module Athena.Decode (processTopics) where

import Json.Decode as Json exposing (..)
import Json.Decode.Extra exposing ((|:))
import Athena.Model exposing (..)
import JsonApi exposing (..)


processTopics : Maybe JsonApiBody -> List Topic
processTopics data =
  let
    payload =
      Maybe.withDefault nullJsonApiBody data

    processor =
      processDomainEntity topicRelationshipProcessor.decoder topicRelationshipProcessor.relationships
  in
    filterPayloadByType payload.data topicRelationshipProcessor.typeName <| processor payload.included


user : String -> Decoder User
user id =
  succeed (User id)
    |: ("first-name" := string)
    |: ("last-name" := string)


comment : String -> Decoder Comment
comment id =
  succeed (Comment id)
    |: ("body" := string)
    |: (succeed nullUser)
    |: (succeed (Responses []))


item : String -> Decoder Item
item id =
  succeed (Item id)
    |: ("name" := string)
    |: ("description" := oneOf [ string, null "" ])
    |: ("requesting-feedback" := bool)
    |: (succeed nullUser)
    |: (succeed [])


topic : String -> Decoder Topic
topic id =
  succeed (Topic id)
    |: ("locations" := list string)
    |: ("name" := string)
    |: (succeed [])


teachingMove : String -> Decoder TeachingMove
teachingMove id =
  succeed (TeachingMove id)
    |: ("location-in-topic" := oneOf [ string, null "" ])
    |: ("line-number" := oneOf [ string, null "" ])
    |: (succeed nullItem)


setUser : b -> { a | user : b } -> { a | user : b }
setUser user record =
  { record | user = user }


setComments : b -> { a | comments : b } -> { a | comments : b }
setComments comments record =
  { record | comments = comments }


setTeachingMoves : b -> { a | teaching_moves : b } -> { a | teaching_moves : b }
setTeachingMoves teachingMoves record =
  { record | teaching_moves = teachingMoves }


setItem : b -> { a | item : b } -> { a | item : b }
setItem item record =
  { record | item = item }


itemRelationshipProcessor : RelationshipProcessor Item
itemRelationshipProcessor =
  { decoder = item
  , relationships =
      (\record included payload ->
        record
          |> (setUser (hasOne userRelationshipProcessor included payload))
          |> (setComments (hasMany commentRelationshipProcessor included payload))
      )
  , relationshipName = "item"
  , typeName = "items"
  , default = nullItem
  }


userRelationshipProcessor : RelationshipProcessor User
userRelationshipProcessor =
  { decoder = user
  , relationships = (\record included payload -> record)
  , relationshipName = "user"
  , typeName = "users"
  , default = nullUser
  }


commentRelationshipProcessor : RelationshipProcessor Comment
commentRelationshipProcessor =
  { decoder = comment
  , relationships =
      (\record included payload ->
        record
          |> (setComments (Responses (hasMany commentRelationshipProcessor included payload)))
          |> (setUser (hasOne userRelationshipProcessor included payload))
      )
  , relationshipName = "comments"
  , typeName = "comments"
  , default = nullComment
  }


teachingMoveRelationshipProcessor : RelationshipProcessor TeachingMove
teachingMoveRelationshipProcessor =
  { decoder = teachingMove
  , relationships =
      (\record included payload ->
        record
          |> (setItem (hasOne itemRelationshipProcessor included payload))
      )
  , relationshipName = "teaching-moves"
  , typeName = "teaching-moves"
  , default = nullTeachingMove
  }


topicRelationshipProcessor : RelationshipProcessor Topic
topicRelationshipProcessor =
  { decoder = topic
  , relationships =
      (\record included payload ->
        setTeachingMoves (hasMany teachingMoveRelationshipProcessor included payload) record
      )
  , relationshipName = "topics"
  , typeName = "topics"
  , default = nullTopic
  }
