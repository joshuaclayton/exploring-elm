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
  , items: List Item
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

nullTopic = { id = "", locations = [], name = "", teaching_moves = [] }

nullTeachingMove : TeachingMove
nullTeachingMove = { id = "", location_in_topic = "", line_number = "", items = [] }

item : String -> Decoder Item
item id =
  succeed (Item id)
    |: ("name" := oneOf [ string, null "" ])
    |: ("description" := oneOf [ string, null "" ])
    |: ("requesting-feedback" := oneOf [ bool, null False ])

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
    |: (succeed [])

noEffects : a -> (a, Effects b)
noEffects thing = (thing, Effects.none)

init = (initialModel, getTopics)











processDomainEntity : (String -> Decoder a) -> (a -> List JsonApiPayload -> JsonApiPayload -> a) -> List JsonApiPayload -> JsonApiPayload -> a
processDomainEntity genericDecoder relationshipProcessor includedRecords payload =
  case decodeValue (genericDecoder payload.id) payload.attributes of
    Ok decodedValue' -> relationshipProcessor decodedValue' includedRecords payload
    Err message -> crash message



processTeachingMoveRelationships : TeachingMove -> List JsonApiPayload -> JsonApiPayload -> TeachingMove
processTeachingMoveRelationships teachingMove included payload =
  let items = log "items" (handleItems "items" included payload)
  in { teachingMove | items = items }

processTopicRelationships : Topic -> List JsonApiPayload -> JsonApiPayload -> Topic
processTopicRelationships topic included payload =
  let teachingMoves = log "moves" (handle "teaching-moves" included payload)
      includedLength = log "included length" (included |> List.length)
  in { topic | teaching_moves = teachingMoves }

nullRelationshipProcessor : a -> List JsonApiPayload -> JsonApiPayload -> a
nullRelationshipProcessor generic included genericPayload =
  generic

handle : String -> List JsonApiPayload -> JsonApiPayload -> List TeachingMove
handle filter includedRecords primaryRecord =
  let relationshipIds = relationshipIdsByType filter primaryRecord.relationships
      includedLength = log "included length in 'handle'" (includedRecords |> List.length)
      filterRecordsToRelationship = (\record -> List.member record.id relationshipIds)
      filteredRecords = List.filter filterRecordsToRelationship (filterListByType filter includedRecords)
  in List.map (processDomainEntity teachingMove processTeachingMoveRelationships includedRecords) filteredRecords


handleItems : String -> List JsonApiPayload -> JsonApiPayload -> List Item
handleItems filter includedRecords primaryRecord =
  let relationshipIds = log ("relationshipIds for " ++ filter) (relationshipIdsByType filter primaryRecord.relationships)
      filterRecordsToRelationship = (\record -> List.member record.id relationshipIds)
      filteredRecords = List.filter filterRecordsToRelationship (filterListByType filter includedRecords)
      recordsLength = log "records length" (includedRecords |> List.head)
  in List.map (processDomainEntity item nullRelationshipProcessor includedRecords) filteredRecords















update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> noEffects model
    NewResponse data ->
      let payload = Maybe.withDefault nullJsonApiBody data
      in noEffects { model | topics = (filterPayloadByType payload.data "topics" (processDomainEntity topic processTopicRelationships payload.included)) }

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
  , div [] (List.map (\item -> text item.name) teachingMove.items)
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
