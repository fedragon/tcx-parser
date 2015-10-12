name := """tcx-parser"""

organization := "com.github.fedragon"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.8.2",
  "org.joda" % "joda-convert" % "1.8",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.3",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)
