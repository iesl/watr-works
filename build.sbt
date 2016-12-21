import sbt.Keys._
import spray.revolver.AppProcess
import com.lihaoyi.workbench.Plugin._

SensibleThisBuild.settings

autoCompilerPlugins := true

enablePlugins(ScalaJSPlugin)

val Lib = CommonLibs

val commonSettings = (
    SensibleProject.settings ++   // SensibleProject.acyclicPlugin ++
    SensibleProject.testSettings ++
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

lazy val root = (project in file("."))
  .settings(Release.settings :_*)
  .aggregate(watrprelude, watrmarks, watrshed)


lazy val watrprelude = (project in file("watr-prelude"))
  .settings(commonSettings: _*)

lazy val watrmarks = (crossProject in file("watr-marks"))
    .dependsOn(watrprelude).aggregate(watrprelude)

// lazy val watrmarks = (project in file("watr-marks"))
//   .settings(commonSettings: _*)
//   .settings(libraryDependencies ++= Seq(
//     "net.sf.jsi" % "jsi" % "1.1.0-SNAPSHOT"
//   ))
//   .dependsOn(watrprelude)
//   .aggregate(watrprelude)
// .settings(libraryDependencies ++= Lib.scrimage)

lazy val watrshed = (project in file("watr-shed"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= DatabaseLibs.slickDb)
  .dependsOn(watrmarks)


lazy val watrcolors = (crossProject in file("watr-colors"))
  .settings(libraryDependencies ++= Seq(
    Lib.scalaAsync,
    "com.lihaoyi"            %%% "scalatags"        % LibVersions.scalaTagsVersion,
    "me.chrons" %%% "boopickle" % "1.2.5",
    "com.lihaoyi" %%% "autowire" % "0.2.6"
  ))
  .jsSettings(workbenchSettings:_*)
  .jsSettings(
  libraryDependencies ++= Seq(
    "org.querki" %%% "jquery-facade" % "1.0-RC6",
    "org.scala-js" %%% "scalajs-dom" % "0.9.1"
    ),
    // refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile),
    bootSnippet := "edu.umass.cs.iesl.watr.watrcolors.WatrColorClient().main();"
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "io.spray" %% "spray-can" % "1.3.4",
      "io.spray" %% "spray-routing-shapeless2" % "1.3.3",
      "com.typesafe.akka" %% "akka-actor" % "2.4.16",
      "org.webjars.bower" % "fabric" % "1.6.2",
      "org.webjars" % "bootstrap" % "3.3.7",
      "org.webjars" % "jquery" % "2.2.4",
      "org.webjars" % "mousetrap" % "1.6.0"
    )
  )


lazy val watrcolorsJS = watrcolors.js

lazy val watrcolorsJVM = watrcolors.jvm
  .settings((resources in Compile) += (
    (artifactPath in (watrcolorsJS, Compile, fastOptJS)).value
  ))
  .dependsOn(watrshed)
