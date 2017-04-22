package com.github.fedragon.tcxparser

import org.joda.time.DateTime
import scala.xml._

case class Position(
  latitudeDegrees: Double,
  longitudeDegrees: Double)

case class HeartRateBpm(value: Int)

case class Trackpoint(
  time: DateTime,
  position: Option[Position],
  altitudeMeters: Option[Double],
  distanceMeters: Option[Double],
  heartRateBpm: Option[HeartRateBpm],
  sensorState: Option[String])

case class Lap(
  startTime: DateTime,
  totalTimeSeconds: Double,
  distanceMeters: Double,
  maximumSpeed: Double,
  calories: Long,
  averageHeartRateBpm: Option[HeartRateBpm],
  maximumHeartRateBpm: Option[HeartRateBpm],
  intensity: Option[String],
  notes: Option[String],
  triggerMethod: String,
  track: Seq[Trackpoint])

case class Activity(sport: String, id: String, laps: Seq[Lap])

case class TrainingCenterDatabase(activities: Seq[Activity])

object Parser {

  def parse(fileName: String): TrainingCenterDatabase =
    parse(XML.loadFile(fileName))

  def parse(root: NodeSeq): TrainingCenterDatabase =
    TrainingCenterDatabase(
      (root \\ "Activity").map { activity =>
        Activity(
          (activity \ "@Sport").text,
          (activity \ "Id").text,
          parseLaps(activity \ "Lap"))
      }
    )

  private[tcxparser] def parseLaps(laps: NodeSeq) =
    laps.map { lap =>
      Lap(
        DateTime.parse((lap \ "@StartTime").text),
        (lap \ "TotalTimeSeconds").text.toDouble,
        (lap \ "DistanceMeters").text.toDouble,
        (lap \ "MaximumSpeed").text.toDouble,
        (lap \ "Calories").text.toLong,
        optionally(lap \ "AverageHeartRateBpm")(parseHeartRateBpm),
        optionally(lap \ "MaximumHeartRateBpm")(parseHeartRateBpm),
        optionally(lap \ "Intensity")(_.text),
        optionally(lap \ "Notes")(_.text),
        (lap \ "TriggerMethod").text,
        parseTrack(lap \ "Track")
      )
    }

  private[tcxparser] def parseHeartRateBpm(heartRateBpm: NodeSeq) =
    HeartRateBpm((heartRateBpm \ "Value").text.toInt)

  private[tcxparser] def parseTrack(track: NodeSeq) =
    (track \ "Trackpoint").map { point =>
      Trackpoint(
        DateTime.parse((point \ "Time").text),
        optionally(point \ "Position")(parsePosition),
        optionally(point \ "AltitudeMeters")(_.text.toDouble),
        optionally(point \ "DistanceMeters")(_.text.toDouble),
        optionally(point \ "HeartRateBpm")(parseHeartRateBpm),
        optionally(point \ "SensorState")(_.text)
      )
    }

  private[tcxparser] def parsePosition(position: NodeSeq) =
    Position(
      (position \ "LatitudeDegrees").text.toDouble,
      (position \ "LongitudeDegrees").text.toDouble
    )

  private[tcxparser] def optionally[T](node: NodeSeq)(f: NodeSeq => T) =
    Option(node).filter(_.nonEmpty).map(f)
}
