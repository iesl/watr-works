import sbt.Keys._
import spray.revolver.AppProcess
import com.lihaoyi.workbench.Plugin._

enablePlugins(ScalaJSPlugin)

organization in ThisBuild := "edu.umass.cs.iesl"

version in ThisBuild := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.8"

shellPrompt in ThisBuild := Sensible.colorPrompt

addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)

addCompilerPlugin("org.spire-math" %% "kind-projector"  % "0.8.0")

val freeKDeps = Seq(
  "com.projectseptember"            %% "freek"                        % "0.4.3",
  // "org.spire-math"                  %% "kind-projector"               % "0.7.1",
  "com.milessabin"                  % "si2712fix-library"             % "1.2.0"  cross CrossVersion.full,
  "org.typelevel" %% "cats" % "0.6.0"
)


val commonDeps =  Sensible.testLibs() ++ Sensible.logback ++ Seq(
  "net.sf.jsi" % "jsi" % "1.1.0-SNAPSHOT",
  "org.apache.commons" % "commons-lang3" % "3.4",
  "org.scalaz" %% "scalaz-core" % "7.2.5",
  "org.scala-lang.modules" %% "scala-async" % "0.9.6-RC2",
  "com.lihaoyi" %% "fastparse" % "0.3.7",
  "org.jdom" % "jdom2" % "2.0.6",
  "com.lihaoyi" %% "scalatags" % "0.6.0",
  "com.lihaoyi" %% "ammonite-ops" % "0.7.0",
  "com.typesafe.play" %% "play-json" % "2.5.4",
  "com.github.scopt" %% "scopt" % "3.5.0",
  "com.softwaremill.scalamacrodebug" %% "macros" % "0.4"
)


resolvers in ThisBuild ++= List(
  "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  Resolver.jcenterRepo,
  Resolver.bintrayRepo("projectseptember", "maven") // FreeK
)

lazy val root = (project in file("."))
  .dependsOn(watrmarks, watrshed, watrcolorsJVM, watrcolorsJS)
  .aggregate(watrmarks, watrshed, watrcolorsJVM, watrcolorsJS)


lazy val watrmarks = (project in file("watr-marks"))
  .settings(Sensible.settings)
  .settings(libraryDependencies ++= commonDeps)

lazy val watrshed = (project in file("watr-shed"))
  .settings(Sensible.settings)
  .settings(libraryDependencies ++= commonDeps)
  .settings(libraryDependencies ++= Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % "1.54",
    "org.bouncycastle" % "bcpkix-jdk15on" % "1.54",
    "com.h2database" % "h2" % "1.4.192",
    "com.zaxxer" % "HikariCP" % "2.4.7",
    "com.typesafe.slick" %% "slick" % "3.1.1"
  ))
  .dependsOn(watrmarks)
  .aggregate(watrmarks)

// Revolver.settings
lazy val watrcolors = (crossProject in file("watr-colors"))
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-async" % "0.9.6-RC2",
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
    "com.typesafe.akka" %% "akka-actor" % "2.4.8",
    "org.webjars.bower" % "fabric" % "1.6.2",
    "org.webjars" % "bootstrap" % "3.3.7",
    "org.webjars" % "jquery" % "2.2.4",
    // "org.webjars" % "jquery" % "3.0.0",
    "org.webjars" % "mousetrap" % "1.5.3"
  )
)


lazy val watrcolorsJS = watrcolors.js

lazy val watrcolorsJVM = watrcolors.jvm.settings(
  (resources in Compile) += ({
    (fastOptJS in(watrcolorsJS, Compile)).value
    (artifactPath in (watrcolorsJS, Compile, fastOptJS)).value
  })
).dependsOn(watrshed)
