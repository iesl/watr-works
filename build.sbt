import sbt.Keys._
import spray.revolver.AppProcess
import com.lihaoyi.workbench.Plugin._

ThisBuildDefault.settings

organization in ThisBuild := "edu.umass.cs.iesl"

enablePlugins(ScalaJSPlugin)

scalaJSUseRhino in Global := false

val libV = LibVersions

val commonDeps =  LogLibs.logback ++ Seq(

   "org.scalaz"              %% "scalaz-core"      % libV.scalazVersion,
   "org.scala-lang.modules"  %% "scala-async"      % libV.scalaAsyncVersion,
   "com.lihaoyi"             %% "scalatags"        % libV.scalaTagsVersion,
   "com.lihaoyi"              % "ammonite"         % "0.7.8" cross CrossVersion.full,
   "com.lihaoyi"             %% "fastparse"        % "0.4.2",
   "com.typesafe.play"       %% "play-json"        % "2.5.9",
   "com.slamdata"            %% "matryoshka-core"  % "0.11.1",
   "com.github.scopt"        %% "scopt"            % "3.5.0",
   "org.typelevel"           %% "machinist"        % "0.6.0",

  // Test deps
  "org.scalaz"     %% "scalaz-scalacheck-binding" % libV.scalazVersion  % "test",
  "org.scalacheck" %% "scalacheck"                % "1.13.3"       % "test" force(),
  "org.scalatest" %% "scalatest" % libV.scalatestVersion % "test",
  "org.scalactic" %% "scalactic" % libV.scalatestVersion

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

val scrimageVersion = "2.1.7"

lazy val watrshed = project
  .in(file("watr-shed"))
  .settings(libraryDependencies ++= (commonDeps ++ Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % "1.55",
    "org.bouncycastle" % "bcpkix-jdk15on" % "1.55",
    "com.h2database" % "h2" % "1.4.192",
    "com.zaxxer" % "HikariCP" % "2.5.1",
    "com.typesafe.slick" %% "slick" % "3.1.1",
    "com.sksamuel.scrimage" %% "scrimage-core"     % scrimageVersion,
    "com.sksamuel.scrimage" %% "scrimage-io-extra" % scrimageVersion,
    "com.sksamuel.scrimage" %% "scrimage-filters"  % scrimageVersion
  )))
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
