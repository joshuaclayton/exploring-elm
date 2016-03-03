module JsonApi (JsonApiBody, JsonApiPayload, JsonApiIdentity, JsonApiRelationship, RelationshipProcessor, hasOne, hasMany, jsonApiBody, filterPayloadByType, relationshipIdsByType, filterListByType, processDomainEntity, nullJsonApiBody) where

import Json.Decode as Json exposing (..)
import Json.Decode.Extra exposing ((|:))
import Debug exposing (..)

type alias JsonApiBody =
  { data: List JsonApiPayload
  , included: List JsonApiPayload
  }

type alias JsonApiPayload =
  { id: String
  , type': String
  , attributes: Value
  , relationships: List JsonApiRelationship
  }

type alias JsonApiRelationship =
  { name: String
  , data: List JsonApiIdentity
  }

type alias JsonApiIdentity =
  { id: String
  , type': String
  }

type alias RelationshipProcessor a =
  { decoder: (String -> Decoder a)
  , relationships: (a -> List JsonApiPayload -> JsonApiPayload -> a)
  , relationshipName: String
  , typeName: String
  , default: a
  }

nullJsonApiBody : JsonApiBody
nullJsonApiBody = { data = [], included = [] }

nullJsonApiRelationship : JsonApiRelationship
nullJsonApiRelationship = { name = "", data = [] }

jsonApiBody : Decoder JsonApiBody
jsonApiBody =
  succeed JsonApiBody
    |: ("data" := list jsonApiPayload)
    |: ((maybe ("included" := list jsonApiPayload)) `andThen` maybeListToDecoderList)

jsonApiPayload : Decoder JsonApiPayload
jsonApiPayload =
  succeed JsonApiPayload
    |: ("id" := string)
    |: ("type" := string)
    |: ("attributes" := value)
    |: ("relationships" := ((keyValuePairs value) `andThen` jsonApiRelationshipTuples))

jsonApiIdentity : Decoder JsonApiIdentity
jsonApiIdentity =
  succeed JsonApiIdentity
    |: ("id" := string)
    |: ("type" := string)

decodedValue : (String -> Decoder a) -> JsonApiPayload -> Result String a
decodedValue decoder payload =
  decodeValue (decoder payload.id) payload.attributes

processDomainEntity : (String -> Decoder a) -> (a -> List JsonApiPayload -> JsonApiPayload -> a) -> List JsonApiPayload -> JsonApiPayload -> a
processDomainEntity genericDecoder relationshipProcessor includedRecords payload =
  case decodedValue genericDecoder payload of
    Ok decodedValue' -> processDomainRelationships decodedValue' relationshipProcessor includedRecords payload
    Err message -> crash message

processDomainRelationships : a -> (a -> List JsonApiPayload -> JsonApiPayload -> a) -> List JsonApiPayload -> JsonApiPayload -> a
processDomainRelationships record relationshipProcessor includedRecords payload =
  relationshipProcessor record includedRecords payload


filterRecordsForRelationship : String -> String -> List JsonApiRelationship -> List JsonApiPayload -> List JsonApiPayload
filterRecordsForRelationship relationshipName filter allRelationships includedRecords =
  let relationshipIds = relationshipIdsByType relationshipName allRelationships
      filterRecordsToRelationship = (\record -> List.member record.id relationshipIds)
  in
      List.filter filterRecordsToRelationship (filterListByType filter includedRecords)

hasMany : RelationshipProcessor a -> List JsonApiPayload -> JsonApiPayload -> List a
hasMany processor includedRecords primaryRecord =
  let filteredRecords = filterRecordsForRelationship processor.relationshipName processor.typeName primaryRecord.relationships includedRecords
      processor' = (processDomainEntity processor.decoder processor.relationships)
  in List.map (processor' includedRecords) filteredRecords

hasOne : RelationshipProcessor a -> List JsonApiPayload -> JsonApiPayload -> a
hasOne processor included payload =
  let firstRecord default things = Maybe.withDefault default (things |> List.head)
      allRecords = hasMany processor included payload
  in firstRecord processor.default allRecords





jsonApiRelationshipTuples : List (String, Value) -> Decoder (List JsonApiRelationship)
jsonApiRelationshipTuples listOfTuples =
  let relationshipList (name, payload) =
    case decodeValue (jsonApiRelationship name) payload of
      Ok val -> val
      Err msg -> log msg nullJsonApiRelationship
  in succeed (List.map relationshipList listOfTuples)

maybeListToDecoderList : Maybe (List a) -> Decoder (List a)
maybeListToDecoderList data =
  succeed (Maybe.withDefault [] data)

jsonApiRelationship : String -> Decoder JsonApiRelationship
jsonApiRelationship name =
  succeed JsonApiRelationship
    |: (succeed name)
    |: ((maybe ("data" := oneOf [list jsonApiIdentity, map (\a -> [a]) jsonApiIdentity])) `andThen` maybeListToDecoderList)

filterPayloadByType : List JsonApiPayload -> String -> (JsonApiPayload -> a) -> List a
filterPayloadByType payloads filter fn =
  let filteredPayloads = List.filter (\payload -> payload.type' == filter) payloads
  in List.map fn filteredPayloads

relationshipByType : String -> List { a | name: String } -> Maybe { a | name: String }
relationshipByType filter relationships =
  (List.filter (\relationship -> relationship.name == filter) relationships)
  |> List.head

relationshipIdsByType : String -> List JsonApiRelationship -> List String
relationshipIdsByType filter relationships =
  case relationshipByType filter relationships of
    Just relationship -> List.map .id relationship.data
    Nothing -> []

filterListByType : String -> List { a | type': String } -> List { a | type': String }
filterListByType filter payloads =
  List.filter (\payload -> payload.type' == filter) payloads
