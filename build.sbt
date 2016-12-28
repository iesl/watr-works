import sbt.Keys._

SensibleThisBuild.settings

autoCompilerPlugins := true

val Lib = CommonLibs
val commonSettings = (
    SensibleProject.settings ++   // SensibleProject.acyclicPlugin ++
    SensibleProject.testSettings ++ // scalatex.SbtPlugin.projectSettings ++
    Seq(
      libraryDependencies ++= LogLibs.logback,
      libraryDependencies ++= TestLibs.testAndCheck,
      libraryDependencies ++= Lib.matryoshkaLibs, // Includes scalaz-core
      libraryDependencies ++= Seq(
        Lib.scalatags,
        Lib.ammonite,
        Lib.playJson,
        Lib.shapeless
      )
    ))

import ReleaseTransformations._

lazy val jsProjects = Seq[ProjectReference](
  watrmarksJS, watrcolorsJS
)

lazy val jvmProjects = Seq[ProjectReference](
  watrprelude, watrmarksJVM, watrshed, watrcolorsJVM
)

lazy val root = (project in file("."))
  .settings(Release.settings :_*)
  .aggregate( (jsProjects++jvmProjects): _*)


lazy val watrprelude = (project in file("watr-prelude"))
  .settings(commonSettings: _*)

lazy val watrmarks = (crossProject in file("watr-marks"))
  .settings(SensibleProject.settings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.scalaz"                 %%% "scalaz-core"  % Lib.scalazVersion,
    "com.github.julien-truffaut" %%% "monocle-core" % Lib.monocleVersion % "compile, test",
    "com.github.mpilquist"       %%% "simulacrum"   % "0.10.0"           % "compile, test",
    "com.lihaoyi"                %%% "scalatags"    % LibVersions.scalaTagsVersion,
    "me.chrons"                  %%% "boopickle"    % "1.2.5"
  ))
  .jvmSettings(commonSettings: _*)

lazy val watrdocs = scalatex.ScalatexReadme(
  projectId = "watr-docs",
  wd = file("watr-docs"),
  url = "https://github.com/iesl/watr-works/tree/acs-dev",
  source = "Readme")
  .settings(commonSettings: _*)

lazy val watrmarksJS = watrmarks.js

lazy val watrmarksJVM = watrmarks.jvm
  .dependsOn(watrprelude)
  .settings((resources in Compile) += (
    (fastOptJS in (watrmarksJS, Compile)).value.data
  ))

lazy val watrshed = (project in file("watr-shed"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= DatabaseLibs.slickDb)
  .settings(libraryDependencies += "net.sf.jsi" % "jsi" % "1.1.0-SNAPSHOT")
  .dependsOn(watrprelude)
  .dependsOn(watrmarksJVM)


enablePlugins(ScalaJSPlugin)
enablePlugins(WorkbenchPlugin)

lazy val watrcolors = (crossProject in file("watr-colors"))
  .settings(SensibleProject.settings: _*)
  .settings(libraryDependencies ++= Seq(
    Lib.scalaAsync,
    "com.lihaoyi"  %%% "scalatags"   % LibVersions.scalaTagsVersion,
    "me.chrons" %%% "boopickle" % "1.2.5",
    "com.lihaoyi" %%% "autowire" % "0.2.6"
  ))
  .jsSettings(libraryDependencies ++= Seq(
    "org.querki" %%% "jquery-facade" % "1.0",
    "org.scala-js" %%% "scalajs-dom" % "0.9.1"),
    scalacOptions -= "-Ywarn-dead-code") // doesn't play well with ScalaJS native binding declarations
  .jvmSettings(libraryDependencies ++= Seq(
    "io.spray" %% "spray-can" % "1.3.4",
    "io.spray" %% "spray-routing-shapeless2" % "1.3.3",
    "com.typesafe.akka" %% "akka-actor" % "2.4.16",
    "org.webjars.bower" % "fabric" % "1.6.2",
    "org.webjars" % "bootstrap" % "3.3.7",
    "org.webjars" % "jquery" % "2.2.4",
    "org.webjars" % "mousetrap" % "1.6.0"))
  .dependsOn(watrmarks)

lazy val watrcolorsJS = watrcolors.js


lazy val watrcolorsJVM = watrcolors.jvm
  .dependsOn(watrshed)
  .settings((resources in Compile) ++= Seq(
    (fastOptJS in (watrcolorsJS, Compile)).value.data,
    (artifactPath in (watrcolorsJS, Compile, fastOptJS)).value,
    ((classDirectory in (watrcolorsJS, Compile)).value / ".." / "watrcolors-fastopt.js.map").get.head
  ))
