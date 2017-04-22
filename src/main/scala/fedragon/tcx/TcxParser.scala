package fedragon.tcx

import org.joda.time.DateTime
import scala.util.{Failure, Success, Try}
import scala.xml._

case class Position(
  latitudeDegrees: Double,
  longitudeDegrees: Double)

case class HeartRateBpm(value: Int)

case class TrackPoint(
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
  track: Seq[TrackPoint])

case class Activity(sport: String, id: String, laps: Seq[Lap])

case class TrainingCenterDatabase(activities: Seq[Activity])

object TcxParser {

  def parse(path: String): Try[TrainingCenterDatabase] =
    parse(XML.loadFile(path))

  def parse(root: NodeSeq): Try[TrainingCenterDatabase] =
    sequence((root \\ "Activity").map(parseActivity))
      .map(TrainingCenterDatabase(_))

  private[tcx] def parseActivity(activity: NodeSeq) =
    for {
      laps <- sequence((activity \ "Lap").map(parseLap))
    } yield Activity(
      (activity \ "@Sport").text,
      (activity \ "Id").text,
      laps)

  private[tcx] def parseLap(lap: NodeSeq) =
    for {
      track <- parseTrack(lap \ "Track")
      avg <- optionally(lap \ "AverageHeartRateBpm")(parseHeartRateBpm)
      max <- optionally(lap \ "MaximumHeartRateBpm")(parseHeartRateBpm)
      intensity <- optionally(lap \ "Intensity")(_.text)
      notes <- optionally(lap \ "Notes")(_.text)
      startTime <- Try(DateTime.parse((lap \ "@StartTime").text))
      totalTime <- Try((lap \ "TotalTimeSeconds").text.toDouble)
      distance <- Try((lap \ "DistanceMeters").text.toDouble)
      maxSpeed <- Try((lap \ "MaximumSpeed").text.toDouble)
      calories <- Try((lap \ "Calories").text.toLong)
    } yield Lap(
      startTime,
      totalTime,
      distance,
      maxSpeed,
      calories,
      avg,
      max,
      intensity,
      notes,
      (lap \ "TriggerMethod").text,
      track)

  private[tcx] def parseHeartRateBpm(heartRateBpm: NodeSeq) =
    HeartRateBpm((heartRateBpm \ "Value").text.toInt)

  private[tcx] def parseTrackPoint(point: NodeSeq) =
    for {
      datetime <- Try(DateTime.parse((point \ "Time").text))
      position <- optionally(point \ "Position")(parsePosition)
      altitude <- optionally(point \ "AltitudeMeters")(_.text.toDouble)
      distance <- optionally(point \ "DistanceMeters")(_.text.toDouble)
      heartRate <- optionally(point \ "HeartRateBpm")(parseHeartRateBpm)
      sensorState <- optionally(point \ "SensorState")(_.text)
    } yield TrackPoint(
      datetime,
      position,
      altitude,
      distance,
      heartRate,
      sensorState)

  private[tcx] def parseTrack(track: NodeSeq) =
    sequence((track \ "Trackpoint").map(parseTrackPoint))

  private[tcx] def parsePosition(position: NodeSeq) =
    Position(
      (position \ "LatitudeDegrees").text.toDouble,
      (position \ "LongitudeDegrees").text.toDouble)

  private[tcx] def optionally[T](node: NodeSeq)(f: NodeSeq => T) =
    Try(Option(node).filter(_.nonEmpty).map(f))

  private[tcx] def sequence[T](xs: Seq[Try[T]]): Try[Seq[T]] =
    xs
      .find(_.isFailure)
      .map(t => Failure(t.failed.get))
      .getOrElse(Success(xs.map(_.get)))
}