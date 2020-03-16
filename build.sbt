import sbt.Keys._
import ReleaseTransformations._

Release.settings
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
    Seq(
      "io.circe"                   %% "circe-generic"          % Lib.circeJsonVersion,
      "io.circe"                   %% "circe-parser"           % Lib.circeJsonVersion,
      "io.circe"                   %% "circe-literal"          % Lib.circeJsonVersion,
      "com.chuusai"                %% "shapeless"              % Lib.shapelessV,
      "org.scalatest"              %% "scalatest"              % Lib.scalatestVersion % "test",
      "com.lihaoyi"                %% "fansi"                  % Lib.fansiV,
      "com.lihaoyi"                %% "sourcecode"             % Lib.sourcecodeV,
      "org.scalaz"                 %% "scalaz-core"            % Lib.scalazVersion,
      Lib.ammoniteOps,
      Lib.guava % Optional,
      "com.lihaoyi"                %% "scalatags"              % Lib.scalaTagsVersion,
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
  .settings(libraryDependencies ++=
    LogLibs.logback ++ TestLibs.testAndCheck ++
    Lib.fs2 ++
    Lib.circeJson ++ Seq(
      "co.fs2" %% "fs2-io" % Lib.fs2Version,
      "org.apache.pdfbox" % "pdfbox" % "2.0.19",
      "com.outr" %% "lucene4s" %  Lib.luceneV,
      Lib.lucene4s,
      Lib.guava,
      Lib.scopt,
      Lib.ammoniteOps,
      Lib.shapeless
    ))
  .dependsOn(prelude, watrmarks)

// lazy val watrshed = (project in file("watr-shed"))
//   .enablePlugins(JavaAppPackaging)
//   .settings(mappings in Universal in (Compile, packageDoc) := Seq())
//   .settings(SensibleProject.settings: _*)
//   .settings(SensibleProject.runForked: _*)
//   .settings(libraryDependencies ++=
//     LogLibs.logback ++ TestLibs.testAndCheck ++
//     DatabaseLibs.doobieDb ++
//     Lib.fs2 ++
//     Lib.circeJson ++
//     Seq(
//       Lib.scopt,
//       Lib.ammonite,
//       Lib.shapeless,
//       Lib.lucene4s,
//       "com.github.tototoshi" %% "scala-csv" % "1.3.6"
//     ))
//   .dependsOn(prelude, watrmarks, textworks)

