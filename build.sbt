import sbt.Keys._
import spray.revolver.AppProcess
import com.lihaoyi.workbench.Plugin._
// import spray.revolver.RevolverPlugin.Revolver

enablePlugins(ScalaJSPlugin)

organization in ThisBuild := "edu.umass.cs.iesl"

version in ThisBuild := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.8"

// required for javacpp
classpathTypes in ThisBuild += "maven-plugin"

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
  "-Yno-adapted-args"
  // "-Ywarn-dead-code",
  // "-Ywarn-value-discard"
)

lazy val logbackVersion = "1.1.6"

libraryDependencies in ThisBuild ++= Seq(
  "net.sf.jsi" % "jsi" % "1.1.0-SNAPSHOT",
  "com.iheart" %% "ficus" % "1.2.3",
  "org.apache.commons" % "commons-lang3" % "3.4",
  "com.github.pathikrit" %% "better-files" % "2.15.0",
  "org.scalaz" %% "scalaz-core" % "7.1.7",
  "org.scala-lang.modules" %% "scala-async" % "latest.release",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.jdom" % "jdom2" % "2.0.6",
  "com.lihaoyi" %% "scalatags" % "0.5.4",
  "com.typesafe.play" %% "play-json" % "2.5.0",
  "com.github.scopt" %% "scopt" % "3.4.0",
  "com.itextpdf" % "itextpdf" % "5.5.9",
  "com.softwaremill.scalamacrodebug" %% "macros" % "0.4",
  "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test",
  "org.slf4j" % "slf4j-api" % "1.7.19",
  "ch.qos.logback" % "logback-core" % logbackVersion,
  "ch.qos.logback" % "logback-classic" % logbackVersion
)

resolvers in ThisBuild ++= List(
  "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

lazy val root = (project in file("."))
  .dependsOn(watrmarks, watrshed, watrcolorsJVM, watrcolorsJS)
  .aggregate(watrmarks, watrshed, watrcolorsJVM, watrcolorsJS)


lazy val watrmarks = (project in file("watr-marks"))

lazy val watrshed = (project in file("watr-shed"))
  .settings(libraryDependencies ++= Seq(
    "com.typesafe.slick" %% "slick" % "3.1.1",
    "org.postgresql"   % "postgresql"    % "9.4-1203-jdbc42",
    "pl.edu.icm.cermine" % "cermine-impl" % "1.8"
  ))
  .dependsOn(watrmarks)
  .aggregate(watrmarks)


Revolver.settings

lazy val watrcolors = (
  crossProject in file("watr-colors")
).settings(
  libraryDependencies ++= Seq(
    "me.chrons" %%% "boopickle" % "1.1.3",
    "com.lihaoyi" %%% "autowire" % "0.2.5",
    "com.lihaoyi" %%% "scalarx" % "0.3.1",
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
  // Revolver.settings:_*
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

lazy val watrcolorsJVM = watrcolors.jvm
.settings(
  (resources in Compile) += ({
    (fastOptJS in(watrcolorsJS, Compile)).value
    (artifactPath in (watrcolorsJS, Compile, fastOptJS)).value
  })
).dependsOn(watrshed)
