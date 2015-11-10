organization := "edu.umass.cs.iesl"

name := "xml_annotator"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-target:jvm-1.7",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-language:postfixOps",
  "-Yno-adapted-args",
  "-Ywarn-dead-code"
)

libraryDependencies ++= Seq(
  "org.jdom" % "jdom2" % "2.0.6",
  "org.scalatest" % "scalatest_2.11" % "2.2.5" % "test"
)
