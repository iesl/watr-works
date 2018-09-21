// import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType, _}
import sbtcrossproject.{crossProject, CrossType}
import sbt.Keys._
import ReleaseTransformations._

SensibleProject.settings
// enablePlugins(ScalaJSPlugin)

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

// (crossProject in file("watr-marks"))
lazy val watrmarks = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .withoutSuffixFor(JVMPlatform)
  .in(file("watr-marks"))
  .settings(SensibleProject.settings: _*)
  .settings(Release.settings :_*)
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
  .jvmSettings(libraryDependencies ++=
    LogLibs.logback ++ TestLibs.testAndCheck ++ Seq(
        Lib.ammoniteOps,
        Lib.guava % Optional,
        "com.lodborg"                 % "interval-tree"          % "1.0.0",
        "org.scala-js"               %% "scalajs-stubs"          % "0.6.25" % "provided",
        "com.lihaoyi"                %% "scalatags"              % Lib.scalaTagsVersion,
        "com.github.davidmoten"       % "rtree"                  % "0.8.6",
        "com.github.davidmoten"       % "flatbuffers-java"       % "1.9.0.1",
        "ichi.bench" % "thyme"        % "0.1.1" from "http://plastic-idolatry.com/jars/thyme-0.1.1.jar"
      ))

lazy val watrmarksJS = watrmarks.js
lazy val watrmarksJVM = watrmarks.jvm

lazy val textworks = (project in file("text-works"))
  .enablePlugins(JavaAppPackaging)
  .settings(mappings in (Compile, packageDoc) := Seq())
  .settings(SensibleProject.settings: _*)
  .settings(SensibleProject.runForked: _*)
  .settings(Release.settings :_*)
  .settings(libraryDependencies ++=
    LogLibs.logback ++ TestLibs.testAndCheck ++
    Lib.fs2 ++
    Lib.circeJson ++ Seq(
      "co.fs2" %% "fs2-io" % Lib.fs2Version,
      "org.apache.pdfbox" % "pdfbox" % "2.0.11",
      "com.outr" %% "lucene4s" %  Lib.luceneV,
      Lib.guava,
      "com.vividsolutions" % "jts-core" % "1.14.0",
      Lib.scopt,
      Lib.scrimageCore,
      Lib.ammoniteOps,
      Lib.shapeless
    ))
  .dependsOn(prelude, watrmarksJVM)

lazy val watrshed = (project in file("watr-shed"))
  .enablePlugins(JavaAppPackaging)
  .settings(mappings in (Compile, packageDoc) := Seq())
  .settings(SensibleProject.settings: _*)
  .settings(SensibleProject.runForked: _*)
  .settings(libraryDependencies ++=
    LogLibs.logback ++ TestLibs.testAndCheck ++
    DatabaseLibs.doobieDb ++
    Lib.fs2 ++
    Lib.circeJson ++
    Seq(
      Lib.scopt,
      Lib.scrimageCore,
      Lib.ammonite,
      Lib.shapeless,
      "com.outr" %% "lucene4s" %  Lib.luceneV,
      "com.github.tototoshi" %% "scala-csv" % "1.3.5"
    ))
  .dependsOn(prelude, watrmarksJVM, textworks)

lazy val watrcolorServer = (project in file("watr-color-server"))
  .enablePlugins(JavaAppPackaging)
  .settings(mappings in (Compile, packageDoc) := Seq())
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


