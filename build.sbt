import sbt.Keys._
import spray.revolver.AppProcess
import com.lihaoyi.workbench.Plugin._

SensibleThisBuild.settings

autoCompilerPlugins := true

enablePlugins(ScalaJSPlugin)

val libV = LibVersions
val Lib = CommonLibs

val commonSettings = SensibleProject.settings ++
  SensibleProject.testSettings  ++
  Seq(
    libraryDependencies ++= LogLibs.logback,
    libraryDependencies ++= TestLibs.testAndCheck,
    libraryDependencies ++= Lib.matryoshkaLibs,
    libraryDependencies ++= Seq(
      Lib.scalazCore,
      Lib.scalatags,
      Lib.ammonite,
      Lib.playJson,
      Lib.shapeless,
      Lib.sourcecode,
      Lib.aspectJ

      // compilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.5"),
      // compilerPlugin("org.spire-math" %% "kind-projector"  % "0.9.2")
      // Not needed in watrmarks
      // Lib.scopt,
      // Lib.scalaAsync,
      // Lib.fastparse,

    )
  )


import ReleaseTransformations._

lazy val root = (project in file("."))
  .settings(Release.settings :_*)
  .aggregate(watrprelude, watrmarks)
// .aggregate(watrprelude, watrmarks, watrshed, watrcolorsJVM, watrcolorsJS)


lazy val watrprelude = (project in file("watr-prelude"))
  .settings(commonSettings: _*)


lazy val watrmarks = (project in file("watr-marks"))
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= Seq(
    "net.sf.jsi" % "jsi" % "1.1.0-SNAPSHOT"
  ))
  .dependsOn(watrprelude)
  .aggregate(watrprelude)


// lazy val watrshed = project
//   .in(file("watr-shed"))
//   .settings(libraryDependencies ++= (
//     commonDeps ++ DatabaseLibs.slickDb ++ Seq(
//       "com.sksamuel.scrimage" %% "scrimage-core"     % libV.scrimageVersion,
//       "com.sksamuel.scrimage" %% "scrimage-io-extra" % libV.scrimageVersion,
//       "com.sksamuel.scrimage" %% "scrimage-filters"  % libV.scrimageVersion
//     )
//   ))
//   .dependsOn(watrmarks)


// lazy val watrcolors = crossProject
//   .in(file("watr-colors"))
//   .settings(libraryDependencies ++= Seq(
//     "org.scala-lang.modules" %% "scala-async" % libV.scalaAsyncVersion,
//     "me.chrons" %%% "boopickle" % "1.2.4",
//     "com.lihaoyi" %%% "scalatags" % libV.scalaTagsVersion,
//     "com.lihaoyi" %%% "autowire" % "0.2.5"
//   ))
//   .jsSettings(workbenchSettings:_*)
//   .jsSettings(
//   libraryDependencies ++= Seq(
//     "org.querki" %%% "jquery-facade" % "1.0-RC6",
//       "org.scala-js" %%% "scalajs-dom" % "0.9.1"
//     ),
//     // refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile),
//     bootSnippet := "edu.umass.cs.iesl.watr.watrcolors.WatrColorClient().main();"
//   )
//   .jvmSettings(
//     libraryDependencies ++= Seq(
//       "io.spray" %% "spray-can" % "1.3.4",
//       "io.spray" %% "spray-routing-shapeless2" % "1.3.3",
//       "com.typesafe.akka" %% "akka-actor" % "2.4.11",
//       "org.webjars.bower" % "fabric" % "1.6.2",
//       "org.webjars" % "bootstrap" % "3.3.7",
//       "org.webjars" % "jquery" % "2.2.4",
//       "org.webjars" % "mousetrap" % "1.6.0"
//     )
//   )


// lazy val watrcolorsJS = watrcolors.js

// lazy val watrcolorsJVM = watrcolors.jvm
//   .settings((resources in Compile) += (
//     (artifactPath in (watrcolorsJS, Compile, fastOptJS)).value
//   ))
//   .dependsOn(watrshed)
