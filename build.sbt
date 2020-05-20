import sbt.Keys._
import ReleaseTransformations._

// Release.settings
Global / onChangedBuildSource := ReloadOnSourceChanges // | IgnoreSourceChanges

val Lib = CommonLibs

lazy val root = (project in file("."))
  .settings(SensibleProject.settings: _*)
  .aggregate(
    prelude, watrmarks, textworks
  )


lazy val prelude = (project in file("watr-prelude"))
  .settings(SensibleProject.settings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value
  ))

lazy val watrmarks = project
  .in(file("watr-marks"))
  .settings(SensibleProject.settings: _*)
  .settings(libraryDependencies ++=
    Lib.circeJson ++
    Seq(
      "org.scalatest"              %% "scalatest"              % Lib.scalatestVersion % "test",
      "com.lihaoyi"                %% "fansi"                  % Lib.fansiV,
      "com.lihaoyi"                %% "sourcecode"             % Lib.sourcecodeV,
      "com.lihaoyi"                %% "pprint"                 % "0.5.9",
      "org.scalaz"                 %% "scalaz-core"            % Lib.scalazVersion,
      Lib.ammoniteOps,
      Lib.guava % Optional,
      "com.github.davidmoten"       % "rtree"                  % "0.8.7",
      "com.github.davidmoten"       % "flatbuffers-java"       % "1.10.0.2"
    ) ++ LogLibs.logback ++ TestLibs.testAndCheck
  )

lazy val textworks = (project in file("text-works"))
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(BuildInfoPlugin)
  .settings(SensibleProject.settings: _*)
  .settings(SensibleProject.runForked: _*)
  .settings(SensibleProject.buildInfoSettings:_*)
  .settings(Release.settings:_*)
  .settings(libraryDependencies ++=
    LogLibs.logback ++ TestLibs.testAndCheck ++
    Lib.circeJson ++ Seq(
      Lib.fs2Core,
      Lib.fs2IO,
      Lib.pdfbox,
      Lib.lucene4s,
      Lib.guava,
      Lib.scopt,
      Lib.ammoniteOps,
      Lib.shapeless
    ))
  .dependsOn(prelude, watrmarks)

