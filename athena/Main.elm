import StartApp
import Effects exposing (Never, Effects)
import Html exposing (..)
import Http
import Json.Decode as Json exposing (..)
import Json.Decode.Extra exposing ((|:))
import Task
import Debug exposing (..)
import JsonApi exposing (..)

type Action
  = NoOp
  | NewResponse (Maybe JsonApiBody)

type alias Model = { topics: List Topic  }

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

initialModel : Model
initialModel = { topics = [] }

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
    |: ("location-in-topic" := oneOf [ string, null ""])
    |: ("line-number" := oneOf [ string, null ""])
    |: (succeed nullItem)

noEffects : a -> (a, Effects b)
noEffects thing = (thing, Effects.none)

init = (initialModel, getTopics)

setUser : b -> { a | user: b } -> { a | user: b }
setUser user record = { record | user = user }

setComments : b -> { a | comments: b } -> { a | comments: b }
setComments comments record = { record | comments = comments }

setTeachingMoves : b -> { a | teaching_moves: b } -> { a | teaching_moves : b }
setTeachingMoves teachingMoves record = { record | teaching_moves = teachingMoves }

setItem : b -> { a | item: b } -> { a | item: b }
setItem item record = { record | item = item }

processTopicRelationships : Topic -> List JsonApiPayload -> JsonApiPayload -> Topic
processTopicRelationships topic included payload =
  let hasManyTeachingMoves = hasMany teachingMoveRelationshipProcessor included payload
  in
     topic
     |> (setTeachingMoves hasManyTeachingMoves)

processCommentRelationships : Comment -> List JsonApiPayload -> JsonApiPayload -> Comment
processCommentRelationships comment included payload =
  let hasManyComments = hasMany commentRelationshipProcessor included payload
      hasOneUser = hasOne userRelationshipProcessor included payload
  in
     comment
     |> (setComments (Responses hasManyComments))
     |> (setUser hasOneUser)

processItemRelationships : Item -> List JsonApiPayload -> JsonApiPayload -> Item
processItemRelationships item included payload =
  let hasOneUser = hasOne userRelationshipProcessor included payload
      hasManyComments = hasMany commentRelationshipProcessor included payload
  in
     item
     |> (setUser hasOneUser)
     |> (setComments hasManyComments)

processTeachingMoveRelationships : TeachingMove -> List JsonApiPayload -> JsonApiPayload -> TeachingMove
processTeachingMoveRelationships teachingMove included payload =
  let hasOneItem = hasOne itemRelationshipProcessor included payload
  in
     teachingMove
     |> (setItem hasOneItem)


















itemRelationshipProcessor : RelationshipProcessor Item
itemRelationshipProcessor =
  { decoder = item
  , relationships = processItemRelationships
  , relationshipName = "item"
  , typeName = "items"
  , default = nullItem
  }

userRelationshipProcessor : RelationshipProcessor User
userRelationshipProcessor =
  { decoder = user
  , relationships = nullRelationshipProcessor
  , relationshipName = "user"
  , typeName = "users"
  , default = nullUser
  }

commentRelationshipProcessor : RelationshipProcessor Comment
commentRelationshipProcessor =
  { decoder = comment
  , relationships = processCommentRelationships
  , relationshipName = "comments"
  , typeName = "comments"
  , default = nullComment
  }

teachingMoveRelationshipProcessor : RelationshipProcessor TeachingMove
teachingMoveRelationshipProcessor =
  { decoder = teachingMove
  , relationships = processTeachingMoveRelationships
  , relationshipName = "teaching-moves"
  , typeName = "teaching-moves"
  , default = nullTeachingMove
  }

topicRelationshipProcessor : RelationshipProcessor Topic
topicRelationshipProcessor =
  { decoder = topic
  , relationships = processTopicRelationships
  , relationshipName = "topics"
  , typeName = "topics"
  , default = nullTopic
  }












update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> noEffects model
    NewResponse data ->
      let payload = Maybe.withDefault nullJsonApiBody data
          processor' = (processDomainEntity topicRelationshipProcessor.decoder topicRelationshipProcessor.relationships)
      in noEffects { model | topics = (filterPayloadByType payload.data topicRelationshipProcessor.typeName (processor' payload.included)) }

inputs : List (Signal Action)
inputs = []

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ text "Hello world"
    , renderTopics model.topics
    ]

renderTopics : List Topic -> Html
renderTopics topics =
  case topics of
    [] -> h2 [] [text "Loading topics..."]
    _ -> ul [] (List.map renderTopic topics)

renderTopic : Topic -> Html
renderTopic topic =
  div
    []
    [ h2 [] [text (topic.name ++ " (id: " ++ topic.id ++ ")")]
    , ul [] (List.map (\location -> li [] [text location]) topic.locations)
    , ul [] (List.map renderTeachingMove topic.teaching_moves)
    ]

renderTeachingMove : TeachingMove -> Html
renderTeachingMove teachingMove =
  li [] [
    text (teachingMove.line_number ++ " - " ++ teachingMove.location_in_topic)
  , renderItem teachingMove teachingMove.item
  ]

fullName : User -> String
fullName user = user.first_name ++ " " ++ user.last_name

renderItem : TeachingMove -> Item -> Html
renderItem teachingMove item =
  let fullName user = user.first_name ++ " " ++ user.last_name
  in
    div [] [
      h2 [] [text teachingMove.item.name]
    , p [] [text ("by " ++ (fullName item.user))]
    , p [] [text item.description]
    , (renderComments item.comments)
    ]

renderComments : List Comment -> Html
renderComments comments =
  case comments of
    [] -> div [] []
    _ ->
      div
        []
        [ h3 [] [text ("Comments (" ++ (comments |> List.length |> toString) ++ ")")]
        , ul [] (List.map renderComment comments)
        ]

renderComment : Comment -> Html
renderComment comment =
  li
    []
    [ text comment.body
    , i [] [text (comment.user |> fullName)]
    , renderReplies comment.comments
    ]

renderReplies : Responses -> Html
renderReplies comments =
  case comments of
    Responses [] -> span [] []
    Responses comments' ->
      ul
        []
        (List.map renderComment comments')

app = StartApp.start { init = init, view = view, update = update, inputs = inputs }

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

main = app.html

getTopics : Effects Action
getTopics =
  Http.get jsonApiBody "https://api.teachathena.org/api/topics?filter[search]=Be&include=teaching-moves.item.user,teaching-moves.item.comments.user,teaching-moves.item.comments.comments.user"
  |> Task.toMaybe
  |> Task.map NewResponse
  |> Effects.task
