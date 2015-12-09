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

libraryDependencies in ThisBuild ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.5",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.jdom" % "jdom2" % "2.0.6",
  "com.lihaoyi" %% "scalatags" % "0.5.3",
  "org.scalatest" % "scalatest_2.11" % "2.2.5" % "test"
)

import com.lihaoyi.workbench.Plugin._

resolvers in ThisBuild += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

lazy val root = (project in file("."))
  .dependsOn(watrmarks, watrshed )
  .aggregate(watrmarks, watrshed )


lazy val watrmarks = (project in file("watr-marks"))

lazy val watrshed = (project in file("watr-shed"))
  .settings(libraryDependencies ++= Seq(
    "pl.edu.icm.cermine" % "cermine-impl" % "1.8-SNAPSHOT",
    "com.lihaoyi" %% "ammonite-ops" % "0.5.0",
    "com.lihaoyi" % "ammonite-repl" % "0.5.0" cross CrossVersion.full
  ))
  .settings(initialCommands := {
    val imports = """|import ammonite.ops._
                     |pl.edu.icm.cermine
                     |cermine.ExtractionUtils
                     |cermine.{ComponentConfiguration => Conf}
                     |ammonite.ops.ImplicitWd._
                     |edu.umass.cs.iesl.watr
                     |edu.umass.cs.iesl.watr._
                     |edu.umass.cs.iesl.watr.shell._
                     |edu.umass.cs.iesl.watr.shell.ops._""".stripMargin.split("\n").mkString(",")

    s""" ammonite.repl.Repl.run("${imports}") """
  })
  .dependsOn(watrmarks)
  .aggregate(watrmarks)


// lazy val watrcolors = (project in file("watr-colors"))
//   .enablePlugins(ScalaJSPlugin)
//   .settings(workbenchSettings:_*)
//   .settings(libraryDependencies ++= Seq(
//     "org.scala-js" %%% "scalajs-dom" % "0.8.0",
//     "com.lihaoyi" %%% "upickle" % "0.3.6",
//     "com.lihaoyi" %%% "autowire" % "0.2.4"
//   ))
//   .settings(bootSnippet := "edu.umass.cs.iesl.watr.example.ScalaJSExample().main(document.getElementById('canvas'));")
//   .settings(updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile))
//   .dependsOn(watrmarks, watrshed)

import sbt.Keys._
import com.lihaoyi.workbench.Plugin._
import spray.revolver.AppProcess
import spray.revolver.RevolverPlugin.Revolver

val watrcolors = (crossProject in file("watr-colors")).settings(
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "upickle" % "0.3.6",
    "com.lihaoyi" %%% "autowire" % "0.2.4",
    "com.lihaoyi" %%% "scalatags" % "0.5.3"
  )
).jsSettings(
  workbenchSettings:_*
).jsSettings(
  name := "watrcolors-client",
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.8.1"
  ),
  // bootSnippet := "edu.umass.cs.iesl.watr.example.ScalaJSExample().main(document.getElementById('canvas'));"
  bootSnippet := "edu.umass.cs.iesl.watr.watrcolors.ScalaJSExample().main();"
).jvmSettings(
  Revolver.settings:_*
).jvmSettings(
  name := "watrcolor-server",
  libraryDependencies ++= Seq(
    "io.spray" %% "spray-can" % "1.3.1",
    "io.spray" %% "spray-routing" % "1.3.1",
    "com.typesafe.akka" %% "akka-actor" % "2.3.2",
    "org.webjars" % "bootstrap" % "3.2.0"
  )
)

val watrcolorsJS = watrcolors.js

val watrcolorsJVM = watrcolors.jvm.settings(
  (resources in Compile) += {
    (fastOptJS in (watrcolorsJS, Compile)).value
    (artifactPath in (watrcolorsJS, Compile, fastOptJS)).value
  }
)
