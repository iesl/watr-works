import sbt.Keys._
import ReleaseTransformations._

Release.settings
Global / onChangedBuildSource := ReloadOnSourceChanges // | IgnoreSourceChanges

val Lib = CommonLibs

  //.settings(SensibleProject.settings: _*)
lazy val root = (project in file("."))
  .aggregate(
    prelude,
    watrmarks,
    textworks,
    watrtable
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

lazy val watrtable = (project in file("modules/watr-table"))
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
        ++ Lib.circeJson
        ++ Lib.http4s
        ++ Seq(
          Lib.scopt,
          Lib.ammoniteOps,
          Lib.ammonite,
          "com.lihaoyi" %% "scalatags"   % "0.9.4"
        )
  )
  .dependsOn(prelude, watrmarks, textworks)
