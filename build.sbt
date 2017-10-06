import sbt.Keys._
import ReleaseTransformations._

SensibleThisBuild.settings
SensibleProject.settings

val Lib = CommonLibs

lazy val jsProjects = Seq[ProjectReference](
  watrmarksJS // , watrcolorsJS
)

lazy val jvmProjects = Seq[ProjectReference](
  prelude, watrmarksJVM, textworks // , watrshed, watrcolorsJVM
)
lazy val root = (project in file("."))
  .enablePlugins(ScalaJSPlugin)
  .aggregate( (jsProjects ++ jvmProjects): _*)

lazy val prelude = (project in file("watr-prelude"))
  .settings(SensibleProject.settings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value
  ))

lazy val watrmarks = (crossProject in file("watr-marks"))
  .settings(SensibleProject.settings: _*)
  .settings(Release.settings :_*)
  .settings(libraryDependencies ++= Seq(
    "org.scalaz"                 %%% "scalaz-core"            % Lib.scalazVersion,
    "com.chuusai"                %%% "shapeless"              % "2.3.2",
    "com.lihaoyi"                %%% "scalatags"              % Lib.scalaTagsVersion,
    "com.lihaoyi"                %%% "fansi"                  % Lib.fansiV,
    "com.lihaoyi"                %%% "sourcecode"             % Lib.sourcecodeV,
    "com.slamdata"               %%% "matryoshka-core"        % Lib.matryoshkaCoreV,
    "com.slamdata"               %%% "matryoshka-scalacheck"  % Lib.matryoshkaCoreV % "compile, test"
  ))
  .jvmSettings(libraryDependencies ++=
    LogLibs.logback ++
    TestLibs.testAndCheck ++ Seq(
      Lib.ammoniteOps,
      Lib.playJson,
      "com.github.davidmoten" % "rtree" % "0.8.0.1",
      "com.github.davidmoten" % "flatbuffers-java" % "1.7.0.1",
      "ichi.bench" % "thyme" % "0.1.1" from "http://plastic-idolatry.com/jars/thyme-0.1.1.jar"
    ))

lazy val watrmarksJS = watrmarks.js

lazy val watrmarksJVM = watrmarks.jvm

lazy val textworks = (project in file("text-works"))
  .enablePlugins(JavaAppPackaging)
  .settings(mappings in (Compile, packageDoc) := Seq())
  .settings(SensibleProject.settings: _*)
  .settings(Release.settings :_*)
  .settings(libraryDependencies ++=
    LogLibs.logback ++
    TestLibs.testAndCheck ++
    Lib.fs2 ++ Seq(
      "com.google.guava" % "guava" % "23.0",
      "com.vividsolutions" % "jts-core" % "1.14.0",
      Lib.scopt,
      Lib.scrimageCore,
      Lib.ammoniteOps,
      Lib.playJson,
      Lib.shapeless
    ))
  .dependsOn(prelude, watrmarksJVM)

lazy val watrshed = (project in file("watr-shed"))
  .enablePlugins(JavaAppPackaging)
  .settings(mappings in (Compile, packageDoc) := Seq())
  .settings(SensibleProject.settings: _*)
  .settings(libraryDependencies ++=
    LogLibs.logback ++
    DatabaseLibs.doobieDb ++
    TestLibs.testAndCheck ++
    Lib.fs2 ++ Seq(
      Lib.scopt,
      Lib.scrimageCore,
      Lib.ammonite,
      Lib.playJson,
      Lib.shapeless
    ))
  .dependsOn(prelude, watrmarksJVM, textworks)


// lazy val watrcolors = (crossProject in file("watr-colors"))
//   .settings(SensibleProject.settings: _*)
//   .settings(skip in packageJSDependencies := false)
//   .settings(libraryDependencies ++= Seq(
//     Lib.scalaAsync,
//     "com.lihaoyi"       %%% "scalatags"       % LibVersions.scalaTagsVersion,
//     "com.lihaoyi"       %%% "scalarx"         % "0.3.2",
//     "com.lihaoyi"       %%% "upickle"         % "0.4.4",
//     "com.lihaoyi"       %%% "autowire"        % "0.2.6"
//   ))
//   .jsSettings(libraryDependencies ++= Seq(
//     "fr.iscpif"         %%% "scaladget"       % LibVersions.scaladgetV,
//     "org.singlespaced"  %%% "scalajs-d3"      % "0.3.4",
//     "org.querki"        %%% "jquery-facade"   % "1.0",
//     "org.scala-js"      %%% "scalajs-dom"     % "0.9.3"),
//     scalacOptions -= "-Ywarn-dead-code" // doesn't play well with ScalaJS native binding declarations
//   )
//   .jvmSettings(libraryDependencies ++= Lib.http4s ++ TestLibs.testAndCheck ++ Seq(
//     "com.typesafe.akka"  %% "akka-actor"                % "2.5.4",
//     "org.webjars"        %  "bootstrap"                 % "3.3.7", // only used for css (bootstrap native is used instead)
//     "org.webjars"        %  "jquery"                    % "2.2.4",
//     "org.webjars"        %  "d3js"                      % "3.5.17",
//     "org.webjars"        %  "mousetrap"                 % "1.6.0"
//   ))

// lazy val watrcolorsJS = watrcolors.js
//   .dependsOn(watrmarksJS)

// lazy val watrcolorsJVM = watrcolors.jvm
//   .enablePlugins(JavaAppPackaging)
//   .dependsOn(watrmarksJVM, watrshed)
//   .settings(mainClass := Some("edu.umass.cs.iesl.watr.watrcolors.server.WatrColorTable"))
//   .settings((resources in Compile) ++= Seq(
//     (fastOptJS in (watrcolorsJS, Compile)).value.data,
//     (artifactPath in (watrcolorsJS, Compile, fastOptJS)).value,
//     ((classDirectory in (watrcolorsJS, Compile)).value / ".." / "watrcolors-fastopt.js.map").get.head
//   ))



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
