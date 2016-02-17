import sbt.Keys._
import spray.revolver.AppProcess
import com.lihaoyi.workbench.Plugin._
import spray.revolver.RevolverPlugin.Revolver

enablePlugins(ScalaJSPlugin)

organization in ThisBuild := "edu.umass.cs.iesl"

version in ThisBuild := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.7"

shellPrompt in ThisBuild := { s: State =>
  val c = scala.Console
  val blue = c.RESET + c.BLUE + c.BOLD
  val white = c.RESET + c.BOLD
  val projectName = Project.extract(s).currentProject.id

  "[" + blue + projectName + white + "]>> " + c.RESET
}

scalacOptions in ThisBuild ++= Seq(
  "-target:jvm-1.7",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard"
)

lazy val doobieVersion = "0.2.3"
lazy val logbackVersion = "1.1.5"

libraryDependencies in ThisBuild ++= Seq(
  "org.apache.commons" % "commons-lang3" % "3.4",
  "com.github.pathikrit" %% "better-files" % "2.15.0",
  "org.scalaz" %% "scalaz-core" % "7.1.7",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.jdom" % "jdom2" % "2.0.6",
  "com.lihaoyi" %% "scalatags" % "0.5.4",
  "com.typesafe.play" %% "play-json" % "2.4.6",
  "com.lihaoyi" %% "sourcecode" % "0.1.0",
  "com.softwaremill.scalamacrodebug" %% "macros" % "0.4",
  "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test",
  "org.slf4j" % "slf4j-api" % "1.7.16",
  "ch.qos.logback" % "logback-core" % logbackVersion,
  "ch.qos.logback" % "logback-classic" % logbackVersion
)

resolvers in ThisBuild ++= List(
  // "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository",
  "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public",
  "ICM repository" at "http://maven.icm.edu.pl/artifactory/repo"
)

lazy val root = (project in file("."))
  .dependsOn(watrmarks, watrshed, watrcolorsJVM)
  .aggregate(watrmarks, watrshed, watrcolorsJVM)


lazy val watrmarks = (project in file("watr-marks"))

lazy val watrshed = (project in file("watr-shed"))
  .settings(libraryDependencies ++= Seq(
    "com.itextpdf" % "itextpdf" % "5.5.8",
    "edu.umass.cs.iesl" %% "spatialindexing" % "latest.release",
    "com.typesafe.slick" %% "slick" % "3.1.1",
    "org.postgresql"   % "postgresql"    % "9.4-1203-jdbc42",
    "pl.edu.icm.cermine" % "cermine-impl" % "1.8"
  ))
  .dependsOn(watrmarks)
  .aggregate(watrmarks)


lazy val watrcolors = (crossProject in file("watr-colors")).settings(
  libraryDependencies ++= Seq(
    "me.chrons" %%% "boopickle" % "1.1.2",
    // "com.lihaoyi" %%% "upickle" % "0.3.8",
    "com.lihaoyi" %%% "autowire" % "0.2.5",
    // "com.lihaoyi" %%% "scalarx" % "0.3.0",
    "com.lihaoyi" %%% "scalatags" % "0.5.4"
  )
).jsSettings(
  workbenchSettings:_*
).jsSettings(
  name := "watrcolors-client",
  libraryDependencies ++= Seq(
    "be.doeraene" %%% "scalajs-jquery" % "0.9.0",
    "org.scala-js" %%% "scalajs-dom" % "0.9.0"
  ),
  // refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile),
  bootSnippet := "edu.umass.cs.iesl.watr.watrcolors.WatrColorClient().main();"
).jvmSettings(
  Revolver.settings:_*
).jvmSettings(
  name := "watrcolors-server",
  libraryDependencies ++= Seq(
    "io.spray" %% "spray-can" % "1.3.3",
    "io.spray" %% "spray-routing" % "1.3.3",
    "com.typesafe.akka" %% "akka-actor" % "2.4.2",
    "org.webjars.bower" % "fabric" % "1.5.0",
    "org.webjars" % "bootstrap" % "3.3.6",
    "org.webjars" % "jquery" % "2.2.0",
    "org.webjars" % "mousetrap" % "1.5.3"
  )
)
lazy val watrcolorsJS = watrcolors.js

lazy val watrcolorsJVM = watrcolors.jvm.settings(
  // Revolver.reStart <<= Revolver.reStart dependsOn (fastOptJS in(watrcolorsJS, Compile)),
  (resources in Compile) += ({
    (fastOptJS in(watrcolorsJS, Compile)).value
    (artifactPath in (watrcolorsJS, Compile, fastOptJS)).value
  }))

  // .dependsOn(watrmarks, watrshed, watrcolorsJS)
  // .aggregate(watrmarks, watrshed, watrcolorsJS)
