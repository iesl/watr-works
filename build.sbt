import sbt.Keys._
import ReleaseTransformations._

Release.settings
Global / onChangedBuildSource := ReloadOnSourceChanges // | IgnoreSourceChanges

val Lib = CommonLibs

fork := true

lazy val root = (project in file("."))
  .aggregate(
    prelude,
    watrmarks,
    textworks,
  )

lazy val prelude = (project in file("modules/watr-prelude"))
  .settings(SensibleProject.settings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

lazy val watrmarks = (project in file("modules/watr-marks"))
  .settings(SensibleProject.settings: _*)
  .settings(
    libraryDependencies ++=
      LogLibs.logback
      ++ TestLibs.testAndCheck
      ++ Lib.featran
      ++ Lib.smile
      ++ Lib.cats
      ++ Lib.circeJson
      ++ Lib.zio
      ++ Lib.consoleUtils
      ++ Lib.rtrees
      ++ Lib.scalaz
      ++ Lib.doodle
      ++ Seq(
        Lib.ammoniteOps,
        Lib.guava
      )
  )

lazy val textworks = (project in file("modules/text-works"))
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(BuildInfoPlugin)
  .settings(SensibleProject.settings: _*)
  .settings(SensibleProject.runForked: _*)
  .settings(SensibleProject.buildInfoSettings: _*)
  .settings(
    libraryDependencies ++=
      LogLibs.logback
      ++ TestLibs.testAndCheck
      ++ Lib.cats
      ++ Lib.zio
      ++ Lib.featran
      ++ Lib.smile
      ++ Lib.circeJson
      ++ Lib.doodle
      ++ Lib.javacvLibs
      ++ Seq(
        Lib.pdfbox,
        Lib.guava,
        Lib.scopt,
        Lib.decline,
        Lib.ammoniteOps,
        Lib.scalaGraph,
        Lib.shapeless
      )
  )
  .dependsOn(prelude, watrmarks)
