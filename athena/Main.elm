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

topic : String -> Decoder Topic
topic id =
  succeed (Topic id)
    |: ("locations" := list string)
    |: ("name" := string)
    |: (succeed [])

teachingMove : String -> Decoder TeachingMove
teachingMove id =
  succeed (TeachingMove id)
    |: ("location-in-topic" := string)
    |: ("line-number" := string)

noEffects : a -> (a, Effects b)
noEffects thing = (thing, Effects.none)

init = (initialModel, getTopics)

processTopic : JsonApiBody -> JsonApiPayload -> Topic
processTopic body payload =
  case decodeValue (topic payload.id) payload.attributes of
    Ok val -> processTopicRelationships val body payload
    Err message -> crash message

relationshipByType : String -> List JsonApiRelationship -> Maybe JsonApiRelationship
relationshipByType filter relationships =
  (List.filter (\relationship -> relationship.name == filter) relationships)
  |> List.head

relationshipIdsByType : String -> List JsonApiRelationship -> List String
relationshipIdsByType filter relationships =
  case relationshipByType filter relationships of
    Just relationship -> List.map .id relationship.data
    Nothing -> []

filterPayloadsByType : String -> List JsonApiPayload -> List JsonApiPayload
filterPayloadsByType filter payloads =
  List.filter (\payload -> payload.type' == filter) payloads

handle : String -> List JsonApiIdentity -> JsonApiPayload -> List TeachingMove
handle filter includedRecords primaryRecord =
  let relationshipIds = relationshipIdsByType filter primaryRecord.relationships
      -- filteredRecords = List.filter (\record -> List.member record.id relationshipIds) (filterPayloadsByType filter includedRecords)
      -- mm = log "relationshipIds" relationshipIds
      -- mm1 = log "filter" filter
      -- mm2 = log "includedRecords" includedRecords
  in []

processTopicRelationships : Topic -> JsonApiBody -> JsonApiPayload -> Topic
processTopicRelationships topic body payload =
  let teachingMoves = log "moves" (handle "teaching-moves" body.included payload)
  in topic

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> noEffects model
    NewResponse data ->
      let payload = Maybe.withDefault nullJsonApiBody data
          foo' = log "payload data" payload.data
          foo = log "payload included" (payload.included |> List.length)
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
