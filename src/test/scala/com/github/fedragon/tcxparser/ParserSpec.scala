package com.github.fedragon.tcxparser

import org.joda.time.DateTime
import org.scalatest._
import scala.xml.NodeSeq

class ParserSpec extends FreeSpec with Matchers {
  import Parser._

  "TCX" - {
    "Position is parsed successfully" in {
      val root =
        <Position>
          <LatitudeDegrees>52.4</LatitudeDegrees>
          <LongitudeDegrees>4.5</LongitudeDegrees>
        </Position>

      parsePosition(root) shouldEqual Position(52.4, 4.5)
    }

    "HeartRateBpm is parsed successfully" in {
      val root =
        <HeartRateBpm>
          <Value>154</Value>
        </HeartRateBpm>

      parseHeartRateBpm(root) shouldEqual HeartRateBpm(154)
    }

    "Track is parsed successfully" in {
      val track = parseTrack(
        <Track>
          <Trackpoint>
            <Time>2016-04-30T09:38:58.000Z</Time>
            <AltitudeMeters>3.0</AltitudeMeters>
            <DistanceMeters>100.0</DistanceMeters>
            <HeartRateBpm>
              <Value>87</Value>
            </HeartRateBpm>
            <SensorState>Present</SensorState>
          </Trackpoint>
        </Track>).head
      val expected = Trackpoint(
        DateTime.parse("2016-04-30T09:38:58.000Z"),
        None,
        Some(3.0),
        Some(100.0),
        Some(HeartRateBpm(87)),
        Some("Present"))

      track shouldEqual expected
    }

    "Lap is parsed successfully" in {
      val laps = parseLaps(
        <Lap StartTime="2016-04-30T09:48:06.375Z">
          <TotalTimeSeconds>6.0</TotalTimeSeconds>
          <DistanceMeters>15.5</DistanceMeters>
          <MaximumSpeed>9.69</MaximumSpeed>
          <Calories>50</Calories>
          <AverageHeartRateBpm>
            <Value>145</Value>
          </AverageHeartRateBpm>
          <MaximumHeartRateBpm>
            <Value>160</Value>
          </MaximumHeartRateBpm>
          <Intensity>Active</Intensity>
          <TriggerMethod>Manual</TriggerMethod>
          <Track></Track>
        </Lap>)
      val expected = Lap(
        DateTime.parse("2016-04-30T09:48:06.375Z"),
        6.0,
        15.5,
        9.69,
        50,
        Some(HeartRateBpm(145)),
        Some(HeartRateBpm(160)),
        Some("Active"),
        None,
        "Manual",
        Seq.empty)

      laps.size shouldBe 1
      laps.head shouldEqual expected
    }

    "Activity is parsed successfully" in {
      val tcd = parse(
        <TrainingCenterDatabase>
          <Activities>
            <Activity Sport="Running">
              <Id>2016-04-30T09:38:57.000Z</Id>
              <Lap StartTime="2016-04-30T09:38:57.000Z">
                <TotalTimeSeconds>549.0</TotalTimeSeconds>
                <DistanceMeters>1277.699951171875</DistanceMeters>
                <MaximumSpeed>9.79999828338623</MaximumSpeed>
                <Calories>1467</Calories>
                <AverageHeartRateBpm>
                  <Value>140</Value>
                </AverageHeartRateBpm>
                <MaximumHeartRateBpm>
                  <Value>151</Value>
                </MaximumHeartRateBpm>
                <Intensity>Active</Intensity>
                <TriggerMethod>Manual</TriggerMethod>
                <Track>
                  <Trackpoint>
                    <Time>2016-04-30T09:38:58.000Z</Time>
                    <AltitudeMeters>3.0</AltitudeMeters>
                    <DistanceMeters>100.0</DistanceMeters>
                    <HeartRateBpm>
                      <Value>87</Value>
                    </HeartRateBpm>
                    <SensorState>Present</SensorState>
                  </Trackpoint>
                </Track>
              </Lap>
            </Activity>
          </Activities>
        </TrainingCenterDatabase>)

      tcd.activities.size shouldBe 1
      tcd.activities.head.id.toString shouldBe "2016-04-30T09:38:57.000Z"
    }

    "file is parsed successfully" in {
      val tcd = parse(getClass.getResource("/test.xml").getPath)

      tcd.activities.size shouldBe 1
      tcd.activities.head.id.toString shouldBe "2016-04-30T09:38:57.000Z"
    }
  }

  "optionally" - {
    "applies function to node, if it exists" in {
      optionally(<A>1</A>)(_.text.toInt + 1) shouldBe Some(2)
    }

    "returns None when it doesn't exist" in {
      optionally(NodeSeq.Empty)(_ => ()) shouldBe None
    }
  }
}