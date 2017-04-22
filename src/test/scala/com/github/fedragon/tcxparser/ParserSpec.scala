package com.github.fedragon.tcxparser

import org.scalatest._
import scala.xml.NodeSeq

class ParserSpec extends FlatSpec with Matchers {

  "optionally" should "apply function to node, if it exists" in {
    Parser.optionally(<A>1</A>)(_.text.toInt + 1) shouldBe Some(2)
  }

  it should "return None when node doesn't exist" in {
    Parser.optionally(NodeSeq.Empty)(_ => ()) shouldBe None
  }

  "Parser" should "parse a Position" in {
    val root =
      <Position>
        <LatitudeDegrees>52.4</LatitudeDegrees>
        <LongitudeDegrees>4.5</LongitudeDegrees>
      </Position>

    val result = Parser.parsePosition(root)

    result shouldBe Position(52.4, 4.5)
  }

  it should "parse a HeartRateBpm" in {
    val root =
      <HeartRateBpm>
        <Value>154</Value>
      </HeartRateBpm>

    val result = Parser.parseHeartRateBpm(root)

    result.value shouldBe 154
  }

  it should "parse a Track" in {
    val root =
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

    val result = Parser.parseTrack(root).head

    result.time.toString shouldBe "2016-04-30T09:38:58.000Z"
    result.altitudeMeters shouldBe Some(3.0)
    result.distanceMeters shouldBe Some(100.0)
    result.heartRateBpm shouldBe Some(HeartRateBpm(87))
    result.sensorState shouldBe Some("Present")
  }

  it should "parse a Lap" in {
    val root =
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
        <Track>
        </Track>
      </Lap>

    val result = Parser.parseLaps(root).head

    result.startTime.toString shouldBe "2016-04-30T09:48:06.375Z"
    result.totalTimeSeconds shouldBe 6.0
    result.distanceMeters shouldBe 15.5
    result.maximumSpeed shouldBe 9.69
    result.calories shouldBe 50
    result.averageHeartRateBpm shouldBe Some(HeartRateBpm(145))
    result.maximumHeartRateBpm shouldBe Some(HeartRateBpm(160))
    result.intensity shouldBe Some("Active")
    result.notes shouldBe None
    result.triggerMethod shouldBe "Manual"
  }

  it should "parse an Activity" in {
    val root =
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
    </TrainingCenterDatabase>

    val result = Parser.parse(root)

    result.activities.head.id.toString shouldBe "2016-04-30T09:38:57.000Z"
  }

  it should "parse a whole file" in {
    val result = Parser.parse(getClass.getResource("/test.xml").getPath)

    result.activities.head.id.toString shouldBe "2016-04-30T09:38:57.000Z"
  }
}