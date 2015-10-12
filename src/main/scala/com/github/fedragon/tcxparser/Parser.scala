package com.github.fedragon.tcxparser

import java.io.File
import org.joda.time.DateTime
import scala.xml._

case class Position(latitudeDegrees: Double, longitudeDegrees: Double)

case class Trackpoint(
  time: DateTime,
  position: Option[Position],
  altitudeMeters: Option[Double],
  distanceMeters: Option[Double],
  heartRateBpm: Option[HeartRateBpm],
  sensorState: String)
case class HeartRateBpm(value: Int)

case class Lap(
  startTime: DateTime,
  totalTimeSeconds: Double,
  distanceMeters: Double,
  maximumSpeed: Double,
  calories: Long,
  averageHeartRateBpm: Option[HeartRateBpm],
  maximumHeartRateBpm: Option[HeartRateBpm],
  intensity: String,
  triggerMethod: String,
  track: Seq[Trackpoint])

case class Activity(sport: String, id: String, laps: Seq[Lap])

case class TrainingCenterDatabase(activities: Seq[Activity])

object Parser {

  def parse(root: NodeSeq) =
    TrainingCenterDatabase(
      (root \\ "Activity").map { activity =>
        Activity(
          (activity \ "@Sport").text,
          (activity \ "Id").text,
          parseLaps(activity \ "Lap"))
      })

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
        (lap \ "Intensity").text,
        (lap \ "TriggerMethod").text,
        parseTrack(lap \ "Track"))
    }

  private[tcxparser] val parseHeartRateBpm = (heartRateBpm: NodeSeq) =>
    HeartRateBpm((heartRateBpm \ "Value").text.toInt)

  private[tcxparser] def parseTrack(track: NodeSeq) =
    (track \ "Trackpoint").map { point =>
      Trackpoint(
        DateTime.parse((point \ "Time").text),
        optionally(point \ "Position")(parsePosition),
        optionally(point \ "AltitudeMeters")(_.text.toDouble),
        optionally(point \ "DistanceMeters")(_.text.toDouble),
        optionally(point \ "HeartRateBpm")(parseHeartRateBpm),
        (point \ "SensorState").text)
    }

  private[tcxparser] val parsePosition = (position: NodeSeq) =>
    Position(
      (position \ "LatitudeDegrees").text.toDouble,
      (position \ "LongitudeDegrees").text.toDouble)

  private def optionally[T](node: NodeSeq)(f: NodeSeq => T) =
    if(node.isEmpty) None
    else Some(f(node))
}
