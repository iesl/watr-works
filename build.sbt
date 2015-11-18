organization := "edu.umass.cs.iesl"

name := "watr-marks"

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
  "org.scalaz" %% "scalaz-core" % "7.1.5",
  "com.lihaoyi" % "ammonite-repl" % "0.4.8" cross CrossVersion.full,
  "com.lihaoyi" %% "ammonite-ops" % "0.4.8",
  "com.lihaoyi" %% "scalatags" % "0.5.3",
  "org.scalatest" % "scalatest_2.11" % "2.2.5" % "test"
)

val imports = """import ammonite.ops._
ammonite.ops.ImplicitWd._
edu.umass.cs.iesl.watr
edu.umass.cs.iesl.watr._
edu.umass.cs.iesl.watr.shell._
edu.umass.cs.iesl.watr.shell.ops._""".split("\n").mkString(",")

initialCommands := s""" ammonite.repl.Repl.run("${imports}") """
