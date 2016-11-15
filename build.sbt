import sbt.Keys._
import spray.revolver.AppProcess
import com.lihaoyi.workbench.Plugin._

ThisBuildDefault.settings

// autoCompilerPlugins in ThisBuild := true
autoCompilerPlugins := true

organization in ThisBuild := "edu.umass.cs.iesl"

enablePlugins(ScalaJSPlugin)

scalaJSUseRhino in Global := false

val libV = LibVersions
val Lib = CommonLibs

val commonDeps = LogLibs.logback ++
  TestLibs.testAndCheck ++
  Lib.matryoshkaLibs ++ Seq(
    Lib.scalazCore,
    Lib.scalaAsync,
    Lib.scalatags,
    Lib.ammonite,
    Lib.fastparse,
    Lib.playJson,
    Lib.scopt,
    Lib.shapeless,
    compilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.5"),
    compilerPlugin("org.spire-math" %% "kind-projector"  % "0.9.3")
  )


import ReleaseTransformations._

lazy val root = (project in file("."))
  .settings(releaseProcess  := Seq[ReleaseStep](
    // checkSnapshotDependencies,            // : ReleaseStep
    inquireVersions,                        // : ReleaseStep
                                            // runTest,                             // : ReleaseStep
    setReleaseVersion,                      // : ReleaseStep
    commitReleaseVersion,                   // : ReleaseStep, performs the initial git checks
    tagRelease,                             // : ReleaseStep
    // publishArtifacts,                    // : ReleaseStep, checks whether `publishTo` is properly set up
    setNextVersion,                         // : ReleaseStep
    commitNextVersion,                      // : ReleaseStep
    pushChanges                             // : ReleaseStep, also checks that an upstream branch is properly configured
  ))
  .dependsOn(watrprelude, watrmarks, watrshed, watrcolorsJVM, watrcolorsJS)
  .aggregate(watrprelude, watrmarks, watrshed, watrcolorsJVM, watrcolorsJS)


lazy val watrprelude = (project in file("watr-prelude"))
  .settings(libraryDependencies ++= commonDeps)


lazy val watrmarks = (project in file("watr-marks"))
  .settings(libraryDependencies ++= commonDeps ++ Seq(
    "net.sf.jsi" % "jsi" % "1.1.0-SNAPSHOT"
  ))
  .dependsOn(watrprelude)
  .aggregate(watrprelude)


lazy val watrshed = project
  .in(file("watr-shed"))
  .settings(libraryDependencies ++= (
    commonDeps ++ DatabaseLibs.slickDb ++ Seq(
      "com.sksamuel.scrimage" %% "scrimage-core"     % libV.scrimageVersion,
      "com.sksamuel.scrimage" %% "scrimage-io-extra" % libV.scrimageVersion,
      "com.sksamuel.scrimage" %% "scrimage-filters"  % libV.scrimageVersion
    )
  ))
  .dependsOn(watrmarks)
  .aggregate(watrmarks)


lazy val watrcolors = crossProject
  .in(file("watr-colors"))
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-async" % libV.scalaAsyncVersion,
    "me.chrons" %%% "boopickle" % "1.2.4",
    "com.lihaoyi" %%% "scalatags" % libV.scalaTagsVersion,
    "com.lihaoyi" %%% "autowire" % "0.2.5"
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
      "com.typesafe.akka" %% "akka-actor" % "2.4.11",
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
