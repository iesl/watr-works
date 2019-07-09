import sbtcrossproject.{crossProject, CrossType}
import sbt.Keys._
import ReleaseTransformations._

SensibleProject.settings
Release.settings

val Lib = CommonLibs

lazy val root = (project in file("."))
  .settings(SensibleProject.settings: _*)
  .aggregate(
    prelude, watrmarksJVM, watrmarksJS, textworks, watrshed, watrcolorServer
  )


lazy val prelude = (project in file("watr-prelude"))
  .settings(SensibleProject.settings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value
  ))

lazy val watrmarks = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .withoutSuffixFor(JVMPlatform)
  .in(file("watr-marks"))
  .settings(SensibleProject.settings: _*)
  // .settings(Release.settings :_*)
  .settings(libraryDependencies ++=
    Seq(
      "io.circe"                   %%% "circe-generic"          % Lib.circeJsonVersion,
      "io.circe"                   %%% "circe-parser"           % Lib.circeJsonVersion,
      "io.circe"                   %%% "circe-literal"          % Lib.circeJsonVersion,
      "com.chuusai"                %%% "shapeless"              % Lib.shapelessV,
      "org.scalatest"              %%% "scalatest"              % Lib.scalatestVersion % "test",
      "com.lihaoyi"                %%% "fansi"                  % Lib.fansiV,
      "com.lihaoyi"                %%% "sourcecode"             % Lib.sourcecodeV,
      "org.scalaz"                 %%% "scalaz-core"            % Lib.scalazVersion
    )
  )
  .jsSettings(scalacOptions += "-P:scalajs:sjsDefinedByDefault")
  .jsSettings(scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) })
  .jvmSettings(libraryDependencies ++=
    LogLibs.logback ++ TestLibs.testAndCheck ++ Seq(
        Lib.ammoniteOps,
        Lib.guava % Optional,
        "org.scala-js"               %% "scalajs-stubs"          % "1.0.0" % "provided",
        "com.lihaoyi"                %% "scalatags"              % Lib.scalaTagsVersion,
        "com.github.davidmoten"       % "rtree"                  % "0.8.7",
        "com.github.davidmoten"       % "flatbuffers-java"       % "1.10.0.2"
      ))

lazy val watrmarksJS = watrmarks.js
lazy val watrmarksJVM = watrmarks.jvm


lazy val textworks = (project in file("text-works"))
  .enablePlugins(JavaAppPackaging)
  .settings(mappings in Universal in (Compile, packageDoc) := Seq())
  .enablePlugins(BuildInfoPlugin)
  .settings(SensibleProject.settings: _*)
  .settings(SensibleProject.runForked: _*)
  .settings(SensibleProject.buildInfoSettings:_*)
  .settings(libraryDependencies ++=
    LogLibs.logback ++ TestLibs.testAndCheck ++
    Lib.fs2 ++
    Lib.circeJson ++ Seq(
      "co.fs2" %% "fs2-io" % Lib.fs2Version,
      "org.apache.pdfbox" % "pdfbox" % "2.0.16",
      "com.outr" %% "lucene4s" %  Lib.luceneV,
      Lib.lucene4s,
      Lib.guava,
      Lib.scopt,
      Lib.ammoniteOps,
      Lib.shapeless
    ))
  .dependsOn(prelude, watrmarksJVM)

lazy val watrshed = (project in file("watr-shed"))
  .enablePlugins(JavaAppPackaging)
  .settings(mappings in Universal in (Compile, packageDoc) := Seq())
  .settings(SensibleProject.settings: _*)
  .settings(SensibleProject.runForked: _*)
  .settings(libraryDependencies ++=
    LogLibs.logback ++ TestLibs.testAndCheck ++
    DatabaseLibs.doobieDb ++
    Lib.fs2 ++
    Lib.circeJson ++
    Seq(
      Lib.scopt,
      Lib.ammonite,
      Lib.shapeless,
      Lib.lucene4s,
      "com.github.tototoshi" %% "scala-csv" % "1.3.6"
    ))
  .dependsOn(prelude, watrmarksJVM, textworks)

lazy val watrcolorServer = (project in file("watr-color-server"))
  .enablePlugins(JavaAppPackaging)
  .settings(mappings in Universal in (Compile, packageDoc) := Seq())
  .settings(SensibleProject.settings: _*)
  .settings(
    fork in run := true,
    connectInput := true,
    outputStrategy := Some(StdoutOutput)
  )
  .settings(libraryDependencies ++=
    Lib.fs2 ++
    LogLibs.logback ++
    TestLibs.testAndCheck ++
    DatabaseLibs.doobieDb ++
    Lib.http4s ++
    Lib.circeJson ++
    Lib.tsec
  )
  .settings((resources in Compile) ++= Seq(
    (fastOptJS in (watrmarksJS, Compile)).value.data,
    ((artifactPath in (watrmarksJS, Compile, fastOptJS)) map { (fastop) =>
      val path = fastop.getPath
      val map = path + ".map"
      file(map)
    }).value
  ))
  .dependsOn(watrmarksJVM, watrshed)
