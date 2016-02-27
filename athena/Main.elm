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

initialModel : Model
initialModel = { topics = [] }

nullTopic : Topic
nullTopic = { id = "", locations = [], name = "", teaching_moves = [] }

nullTeachingMove : TeachingMove
nullTeachingMove = { id = "", location_in_topic = "", line_number = "", item = nullItem }

nullItem : Item
nullItem = { id = "", name = "", description = "", requesting_feedback = False }

item : String -> Decoder Item
item id =
  succeed (Item id)
    |: ("name" := string)
    |: ("description" := oneOf [ string, null "" ])
    |: ("requesting-feedback" := bool)

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











processTopicRelationships : Topic -> List JsonApiPayload -> JsonApiPayload -> Topic
processTopicRelationships topic included payload =
  let teachingMoves = handleGeneric teachingMoveThing included payload
  in { topic | teaching_moves = teachingMoves }

processTeachingMoveRelationships : TeachingMove -> List JsonApiPayload -> JsonApiPayload -> TeachingMove
processTeachingMoveRelationships teachingMove included payload =
  let firstRecord default things = Maybe.withDefault default (things |> List.head)
      items = handleGeneric itemThing included payload
      item = firstRecord itemThing.default items
  in { teachingMove | item = item }

nullRelationshipProcessor : a -> List b -> b -> a
nullRelationshipProcessor generic included genericPayload =
  generic

filterRecordsForRelationship : String -> String -> List JsonApiRelationship -> List JsonApiPayload -> List JsonApiPayload
filterRecordsForRelationship relationshipName filter allRelationships includedRecords =
  let relationshipIds = relationshipIdsByType relationshipName allRelationships
      filterRecordsToRelationship = (\record -> List.member record.id relationshipIds)
  in
      List.filter filterRecordsToRelationship (filterListByType filter includedRecords)

handleGeneric : Thing a -> List JsonApiPayload -> JsonApiPayload -> List a
handleGeneric thing includedRecords primaryRecord =
  let filteredRecords = filterRecordsForRelationship thing.relationshipName thing.typeName primaryRecord.relationships includedRecords
  in List.map (thing.processor includedRecords) filteredRecords

type alias Thing a =
  { processor: (List JsonApiPayload -> JsonApiPayload -> a)
  , relationshipName: String
  , typeName: String
  , default: a
  }

itemThing : Thing Item
itemThing =
  { processor = (processDomainEntity item nullRelationshipProcessor)
  , relationshipName = "item"
  , typeName = "items"
  , default = nullItem
  }

teachingMoveThing : Thing TeachingMove
teachingMoveThing =
  { processor = (processDomainEntity teachingMove processTeachingMoveRelationships)
  , relationshipName = "teaching-moves"
  , typeName = "teaching-moves"
  , default = nullTeachingMove
  }

topicThing : Thing Topic
topicThing =
  { processor = (processDomainEntity topic processTopicRelationships)
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
      in noEffects { model | topics = (filterPayloadByType payload.data topicThing.typeName (topicThing.processor payload.included)) }

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
  , text teachingMove.item.name
  ]

app = StartApp.start { init = init, view = view, update = update, inputs = inputs }

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

main = app.html

getTopics : Effects Action
getTopics =
  Http.get jsonApiBody "https://api.teachathena.org/api/topics?filter[search]=Be&include=teaching-moves.item"
  |> Task.toMaybe
  |> Task.map NewResponse
  |> Effects.task
