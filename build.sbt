import sbt.Keys._
import ReleaseTransformations._

Release.settings
Global / onChangedBuildSource := ReloadOnSourceChanges // | IgnoreSourceChanges

val Lib = CommonLibs

lazy val root = (project in file("."))
  .settings(SensibleProject.settings: _*)
  .aggregate(
    prelude,
    watrmarks,
    textworks
  )

lazy val prelude = (project in file("watr-prelude"))
  .settings(SensibleProject.settings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

lazy val watrmarks = project
  .in(file("watr-marks"))
  .settings(SensibleProject.settings: _*)
  .settings(
    libraryDependencies ++=
      LogLibs.logback
        ++ TestLibs.testAndCheck
        ++ Lib.featran
        ++ Lib.smile
        ++ Lib.circeJson
        ++ Lib.zio
        ++ Lib.consoleUtils
        ++ Lib.rtrees
        ++ Lib.scalaz
        ++ Seq(
          Lib.ammoniteOps,
          Lib.guava
        )
  )

lazy val textworks = (project in file("text-works"))
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(BuildInfoPlugin)
  .settings(SensibleProject.settings: _*)
  .settings(SensibleProject.runForked: _*)
  .settings(SensibleProject.buildInfoSettings: _*)
  .settings(
    libraryDependencies ++=
      LogLibs.logback
        ++ TestLibs.testAndCheck
        ++ Lib.zio
        ++ Lib.featran
        ++ Lib.smile
        ++ Lib.circeJson
        ++ Seq(
          Lib.pdfbox,
          Lib.guava,
          Lib.scopt,
          Lib.ammoniteOps,
          Lib.shapeless
        )
  )
  .dependsOn(prelude, watrmarks)
