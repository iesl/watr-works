import sbt.Keys._
import ReleaseTransformations._

SensibleThisBuild.settings
SensibleProject.settings
enablePlugins(ScalaJSPlugin)

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

lazy val watrmarks = (crossProject in file("watr-marks"))
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
  .jvmSettings(libraryDependencies ++=
    LogLibs.logback ++
    Seq(
      "org.scalaz"                 %% "scalaz-scalacheck-binding" % Lib.scalazVersion  % "test",
      "org.scalacheck"             %% "scalacheck"                % "1.13.5"       % "test", //  force()
      Lib.ammoniteOps,
      "org.scala-js"               %% "scalajs-stubs"          % "0.6.22" % "provided",
      "com.lihaoyi"                %% "scalatags"              % Lib.scalaTagsVersion,
      "com.github.davidmoten"       % "rtree"                  % "0.8.0.4",
      "com.github.davidmoten"       % "flatbuffers-java"       % "1.8.0.1",
      "ichi.bench" % "thyme"        % "0.1.1" from "http://plastic-idolatry.com/jars/thyme-0.1.1.jar"
    ))

lazy val watrmarksJS = watrmarks.js
  .settings(scalacOptions += "-P:scalajs:sjsDefinedByDefault")

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
      "org.apache.pdfbox" % "pdfbox" % "2.0.8",
      "com.google.guava" % "guava" % "23.0",
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
    // Freestyle.libs ++
    Lib.circeJson ++
    Seq(
      Lib.scopt,
      Lib.scrimageCore,
      Lib.ammonite,
      Lib.shapeless
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


// lazy val micrositeSettings = Seq(
//   micrositeName                 := "WatrWorks",
//   micrositeDescription          := "Text Extraction and Annotation Suite",
//   micrositeAuthor               := "IESL",
//   micrositeHomepage             := "https://iesl.github.io/watr-works/",
//   micrositeOrganizationHomepage := "http://www.iesl.cs.umass.edu/",
//   micrositeDocumentationUrl     := "/watr-works/docs/",
//   micrositeBaseUrl              := "/watr-works",
//   micrositeGithubRepo           := "watr-works",
//   micrositeGithubOwner          := "IESL",
//   micrositePushSiteWith         := GitHub4s,
//   micrositeGithubToken          := sys.env.get("GITHUB_MICROSITES_TOKEN"),
//   includeFilter in Jekyll       := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.md"
// )

// lazy val watrdocs = (project in file("watr-docs"))
//   .settings(SensibleProject.settings: _*)
//   .settings(moduleName := "watrworks-documentation")
//   .settings(name := "watrworks-documentation")
//   .settings(micrositeSettings: _*)
//   .enablePlugins(MicrositesPlugin)
//   .dependsOn(watrmarksJVM)
//   .dependsOn(textworks)
//   .dependsOn(watrshed)
