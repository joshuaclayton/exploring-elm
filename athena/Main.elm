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

type alias TeachingMove =
  { id: String
  , location_in_topic: String
  , line_number: String
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
nullTeachingMove = { id = "", location_in_topic = "", line_number = "" }

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

noEffects : a -> (a, Effects b)
noEffects thing = (thing, Effects.none)

init = (initialModel, getTopics)

processTeachingMove : JsonApiPayload -> TeachingMove
processTeachingMove teachingMovePayload =
  case decodeValue (teachingMove teachingMovePayload.id) teachingMovePayload.attributes of
    Ok val -> val
    Err message -> crash message

processTopic : JsonApiBody -> JsonApiPayload -> Topic
processTopic body topicPayload =
  case decodeValue (topic topicPayload.id) topicPayload.attributes of
    Ok topic' -> processTopicRelationships topic' body.included topicPayload
    Err message -> crash message

handle : String -> List JsonApiPayload -> JsonApiPayload -> List TeachingMove
handle filter includedRecords primaryRecord =
  let relationshipIds = relationshipIdsByType filter primaryRecord.relationships
      filterRecordsToRelationship = (\record -> List.member record.id relationshipIds)
      filteredRecords = List.filter filterRecordsToRelationship (filterListByType filter includedRecords)
  in List.map processTeachingMove filteredRecords

processTopicRelationships : Topic -> List JsonApiPayload -> JsonApiPayload -> Topic
processTopicRelationships topic included payload =
  let teachingMoves = log "moves" (handle "teaching-moves" included payload)
  in { topic | teaching_moves = teachingMoves }

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> noEffects model
    NewResponse data ->
      let payload = Maybe.withDefault nullJsonApiBody data
      in noEffects { model | topics = (filterPayloadByType payload.data "topics" (processTopic payload)) }

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
    , ul [] (List.map (\teaching_move -> li [] [text (teaching_move.line_number ++ " - " ++ teaching_move.location_in_topic)]) topic.teaching_moves)
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
