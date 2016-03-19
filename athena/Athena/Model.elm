module Athena.Model where

type alias Item =
  { id: String
  , name: String
  , description: String
  , requesting_feedback: Bool
  , user: User
  , comments: List Comment
  }

type alias TeachingMove =
  { id: String
  , location_in_topic: String
  , line_number: String
  , item: Item
  }

type alias Topic =
  { id: String
  , locations: List String
  , name: String
  , teaching_moves: List TeachingMove
  }

type alias User =
  { id: String
  , first_name: String
  , last_name: String
  }

type alias Comment =
  { id: String
  , body: String
  , user: User
  , comments: Responses
  }

type Responses = Responses (List Comment)

nullTopic : Topic
nullTopic = { id = "", locations = [], name = "", teaching_moves = [] }

nullTeachingMove : TeachingMove
nullTeachingMove = { id = "", location_in_topic = "", line_number = "", item = nullItem }

nullItem : Item
nullItem = { id = "", name = "", description = "", requesting_feedback = False, user = nullUser, comments = [] }

nullUser : User
nullUser = { id = "", first_name = "", last_name = "" }

nullComment : Comment
nullComment = { id = "", body = "", user = nullUser, comments = Responses [] }

fullName : User -> String
fullName user = user.first_name ++ " " ++ user.last_name
