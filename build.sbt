import sbt.Keys._
import ReleaseTransformations._

Release.settings
Global / onChangedBuildSource := ReloadOnSourceChanges // | IgnoreSourceChanges

val Lib = CommonLibs

fork := true

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

// Platform classifier for native library dependencies
val platform = org.bytedeco.javacpp.Loader.Detector.getPlatform


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
        ++ Seq(
          Lib.pdfbox,
          Lib.guava,
          // Lib.javacv,
    "org.bytedeco"   % "javacpp"    % "1.5.5"        withSources() withJavadoc(),
    "org.bytedeco"   % "javacpp"    % "1.5.5"        classifier platform,
    "org.bytedeco"   % "javacv"     % "1.5.5"        withSources() withJavadoc(),
    "org.bytedeco"   % "opencv"     % "4.5.1-1.5.5"  withSources() withJavadoc(),
    "org.bytedeco"   % "opencv"     % "4.5.1-1.5.5"  classifier platform,
    "org.bytedeco"   % "openblas"     % "0.3.13-1.5.5"  withSources() withJavadoc(),
    "org.bytedeco"   % "openblas"     % "0.3.13-1.5.5"  classifier platform,
          Lib.scopt,
          Lib.ammoniteOps,
          Lib.scalaGraph,
          Lib.scalaGraphConstrained,
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
