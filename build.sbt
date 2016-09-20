import sbt.Keys._
import spray.revolver.AppProcess
import com.lihaoyi.workbench.Plugin._

enablePlugins(ScalaJSPlugin)

organization in ThisBuild := "edu.umass.cs.iesl"

scalaVersion in ThisBuild := "2.11.8"

shellPrompt in ThisBuild := Sensible.colorPrompt

val commonDeps =  Sensible.testLibs() ++ Sensible.logback ++ Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.6",
  "org.scala-lang.modules" %% "scala-async" % "0.9.6-RC5",
  "com.lihaoyi" %% "scalatags" % "0.6.0",
  "com.lihaoyi" %% "acyclic" % "0.1.4" % "provided",
  "com.lihaoyi" %% "ammonite-ops" % "0.7.7",
  "com.typesafe.play" %% "play-json" % "2.5.8",
  "com.github.scopt" %% "scopt" % "3.5.0"
)

resolvers in ThisBuild ++= List(
  "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  Resolver.jcenterRepo,
  Resolver.bintrayRepo("projectseptember", "maven") // FreeK
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
  .settings(Sensible.settings)
  .settings(libraryDependencies ++= commonDeps)


lazy val watrmarks = (project in file("watr-marks"))
  .settings(Sensible.settings)
  .settings(libraryDependencies ++= commonDeps ++ Seq(
    "net.sf.jsi" % "jsi" % "1.1.0-SNAPSHOT"
  ))
  .dependsOn(watrprelude)
  .aggregate(watrprelude)

val scrimageVersion = "2.1.7"
val fs2Version = "0.9.1"

lazy val watrshed = (project in file("watr-shed"))
  .settings(Sensible.settings)
  .settings(libraryDependencies ++= commonDeps)
  .settings(libraryDependencies ++= Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % "1.55",
    "org.bouncycastle" % "bcpkix-jdk15on" % "1.55",
    "com.lihaoyi" % "ammonite" % "0.7.7" cross CrossVersion.full,
    "com.h2database" % "h2" % "1.4.192",
    "com.zaxxer" % "HikariCP" % "2.5.1",
    "com.typesafe.slick" %% "slick" % "3.1.1",
    "com.sksamuel.scrimage" %% "scrimage-core"     % scrimageVersion,
    "com.sksamuel.scrimage" %% "scrimage-io-extra" % scrimageVersion,
    "com.sksamuel.scrimage" %% "scrimage-filters"  % scrimageVersion,
    "co.fs2" %% "fs2-core" % fs2Version,
    "co.fs2" %% "fs2-io" % fs2Version
  ))
  .dependsOn(watrmarks)
  .aggregate(watrmarks)


// Revolver.settings
lazy val watrcolors = (crossProject in file("watr-colors"))
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-async" % "0.9.6-RC5",
    "co.fs2" %%% "fs2-core" % fs2Version,
    "me.chrons" %%% "boopickle" % "1.2.4",
    "com.lihaoyi" %%% "autowire" % "0.2.5",
    "com.lihaoyi" %%% "scalarx" % "0.3.1",
    "com.lihaoyi" %%% "scalatags" % "0.6.0"
  )
).jsSettings(
  workbenchSettings:_*
).jsSettings(
  name := "watrcolors-client",
  libraryDependencies ++= Seq(
    "org.querki" %%% "jquery-facade" % "1.0-RC6",
    "org.scala-js" %%% "scalajs-dom" % "0.9.1"
  ),
  // refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile),
  bootSnippet := "edu.umass.cs.iesl.watr.watrcolors.WatrColorClient().main();"
).jvmSettings(
  Sensible.settings:_*
).jvmSettings(
  name := "watrcolors-server",
  libraryDependencies ++= Seq(
    "io.spray" %% "spray-can" % "1.3.3",
    "io.spray" %% "spray-routing-shapeless2" % "1.3.3",
    "com.typesafe.akka" %% "akka-actor" % "2.4.10",
    "org.webjars.bower" % "fabric" % "1.6.2",
    "org.webjars" % "bootstrap" % "3.3.7",
    "org.webjars" % "jquery" % "2.2.4",
    "org.webjars" % "mousetrap" % "1.6.0",
    "com.sksamuel.scrimage" %% "scrimage-core"     % scrimageVersion,
    "com.sksamuel.scrimage" %% "scrimage-io-extra" % scrimageVersion,
    "com.sksamuel.scrimage" %% "scrimage-filters"  % scrimageVersion
  )
)


lazy val watrcolorsJS = watrcolors.js

lazy val watrcolorsJVM = watrcolors.jvm.settings(
  (resources in Compile) += ({
    (fastOptJS in(watrcolorsJS, Compile)).value
    (artifactPath in (watrcolorsJS, Compile, fastOptJS)).value
  })
).dependsOn(watrshed)
