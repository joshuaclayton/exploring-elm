import Html exposing (Html, text)
import Date exposing (Date)
import Time exposing (Time)
import Signal
import Debug

timeNow : Signal Time
timeNow = Time.every <| Time.millisecond*500

main : Signal Html
main =
  let ago = Date.fromTime 1457649523000
  in
    Signal.map (\now ->
      text <| distanceOfTimeInWords ago (Date.fromTime now)
    ) timeNow

type DateDiff = PastDate Int | FutureDate Int

type alias TimeM =
  { maximumValue: Int
  , string: String
  , amount: Int
  }

baseTime = { maximumValue = 1, amount = 0, string = "nothing" }

buildTime : Int -> TimeM -> String -> TimeM
buildTime multiplier unit descriptor =
  TimeM (multiplier*unit.maximumValue) descriptor 0

milliseconds = buildTime 1    baseTime     "millisecond"
seconds      = buildTime 1000 milliseconds "second"
minutes      = buildTime 60   seconds      "minute"
hours        = buildTime 60   minutes      "hour"
days         = buildTime 24   hours        "day"
months       = buildTime 30   days         "month"
years        = buildTime 12   months       "year"

dateDiff : Date -> Date -> DateDiff
dateDiff fromTime toTime =
  let delta = Date.toTime toTime - Date.toTime fromTime
      delta' = (round << abs) delta
  in
    if delta <= 0 then
      FutureDate delta'
    else
      PastDate delta'

measurement : Int -> TimeM
measurement delta =
  let
    timeValues = [milliseconds, seconds, minutes, hours, days, months, years]
    remainder = List.filter (\f -> f.maximumValue < delta) timeValues
    lowest = Maybe.withDefault years (remainder |> List.reverse |> List.head)
    convertDeltaToCorrectUnits d v = round <| (d |> toFloat)/(v |> toFloat)
  in { lowest | amount = convertDeltaToCorrectUnits delta lowest.maximumValue }

pluralize : Int -> String
pluralize val = if val == 1 then "" else "s"

pastToString : Bool -> String
pastToString past = if past then "ago" else "from now"

pluralizeDuration : Int -> String -> Bool -> String
pluralizeDuration val unit past =
  (val |> toString) ++ " " ++ unit ++ pluralize val ++ " " ++ pastToString past

distanceOfTimeInWords : Date -> Date -> String
distanceOfTimeInWords fromTime toTime =
  let (past, delta) = case dateDiff fromTime toTime of
        PastDate val -> (True, val)
        FutureDate val -> (False, val)
      timeM = measurement delta
  in pluralizeDuration timeM.amount timeM.string past
