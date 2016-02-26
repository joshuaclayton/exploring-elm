module JsonApi (JsonApiBody, JsonApiPayload, JsonApiIdentity, JsonApiRelationship, jsonApiBody, filterPayloadByType, nullJsonApiBody) where

import Json.Decode as Json exposing (..)
import Json.Decode.Extra exposing ((|:))
import Debug exposing (..)

type alias JsonApiBody =
  { data: List JsonApiPayload
  , included: List JsonApiIdentity
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

nullJsonApiBody : JsonApiBody
nullJsonApiBody = { data = [], included = [] }

nullJsonApiRelationship : JsonApiRelationship
nullJsonApiRelationship = { name = "", data = [] }

jsonApiBody : Decoder JsonApiBody
jsonApiBody =
  succeed JsonApiBody
    |: ("data" := list jsonApiPayload)
    -- |: ((maybe ("included" := list jsonApiPayload)) `andThen` maybeListToDecoderList)
    |: ("included" := list jsonApiIdentity)

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
    |: ((maybe ("data" := list jsonApiIdentity)) `andThen` maybeListToDecoderList)

filterPayloadByType : List JsonApiPayload -> String -> (JsonApiPayload -> a) -> List a
filterPayloadByType payloads filter fn =
  let filteredPayloads = List.filter (\payload -> payload.type' == filter) payloads
  in List.map fn filteredPayloads
