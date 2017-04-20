import scala.util.{ Properties, Try }
import sbt._
import Keys._

object SensibleProject extends CommonLibs {

  lazy val acyclicPlugin =  Seq(
    addCompilerPlugin("com.lihaoyi" %% "acyclic" % acyclicVersion),
    scalacOptions += "-P:acyclic:force",
    libraryDependencies ++= Seq(acyclic)
  )


  val scalaOptionList = Seq(
    "-deprecation"
      , "-encoding", "UTF-8"
      , "-feature"
      , "-unchecked"
      , "-language:existentials"
      , "-language:higherKinds"
      , "-language:implicitConversions"
      , "-Xlint"
      , "-Ywarn-adapted-args"
      , "-Ywarn-inaccessible"
      , "-Ywarn-unused-import"
      , "-Ywarn-dead-code"
      , "-Ypartial-unification"
      , "-Xfuture"
  )



  lazy val settings =  Seq(
    scalaVersion := "2.11.11",
    organization := "edu.umass.cs.iesl",
    scalacOptions ++= scalaOptionList,

    autoCompilerPlugins  := true,
    addCompilerPlugin("org.spire-math" %% "kind-projector"   % "0.9.3"),
    addCompilerPlugin("org.scalamacros" % "paradise"         % "2.1.0" cross CrossVersion.full),


    logBuffered in Test := false,
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1", "-verbosity", "1"),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    testFrameworks := Seq(TestFrameworks.ScalaTest, TestFrameworks.ScalaCheck)
  )

}

object SensibleThisBuild {
  def colorPrompt = { s: State =>
    val c = scala.Console
    val blue = c.RESET + c.CYAN + c.BOLD
    val white = c.RESET + c.BOLD
    val projectName = Project.extract(s).currentProject.id

    "[" + blue + projectName + white + "]>> " + c.RESET
  }

  lazy val settings =  Seq(

    resolvers in ThisBuild ++= List(
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases"),
      Resolver.jcenterRepo
    ),

    shellPrompt in ThisBuild := colorPrompt

    // organization in ThisBuild := "edu.umass.cs.iesl",
    // scalaVersion in ThisBuild := "2.11.9",
    // scalacOptions in ThisBuild ++= SensibleProject.scalaOptionList,

    // autoCompilerPlugins in ThisBuild := true,
    // ivyLoggingLevel in ThisBuild := UpdateLogging.Quiet
  )


}
