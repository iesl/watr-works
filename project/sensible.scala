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
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-target:jvm-1.6",
    "-unchecked",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-Ypatmat-exhaust-depth", "40",
    "-Xlint",
    "-Yinline-warnings",
    "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
    "-Ywarn-inaccessible",
    "-Ywarn-unused-import", // noisy, but good to run occasionally
    "-Ywarn-dead-code",
    "-Xfuture"
    //
    // "-Ypartial-unification", // typelevel.org scala specific
    // "-language:postfixOps",
    // "-Xcheckinit", // runtime error when a val is not initialized due to trait hierarchies (instead of NPE somewhere else)
    // "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
    // "-Ywarn-numeric-widen", // noisy
  )



  lazy val settings =  Seq(
    scalaVersion := "2.11.8",
    // scalaVersion := "2.12.1",
    organization := "edu.umass.cs.iesl",
    scalacOptions ++= scalaOptionList,
    // javacOptions in (Compile, compile) ++= Seq(
    //   "-source", "1.6", "-target", "1.6", "-Xlint:all", "-Werror",
    //   "-Xlint:-options", "-Xlint:-path", "-Xlint:-processing"
    // ),

    // javacOptions in doc ++= Seq("-source", "1.6"),

    // javaOptions := Seq(
    //   "-Xss2m", "-Xms1g", "-Xmx2g", "-Dfile.encoding=UTF8"
    // ),

    autoCompilerPlugins  := true,

    addCompilerPlugin("org.spire-math" %% "kind-projector"   % "0.9.3"),
    addCompilerPlugin("org.scalamacros" % "paradise"         % "2.1.0" cross CrossVersion.full),
    addCompilerPlugin("com.milessabin"  % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full),

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
      // "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public",
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases"),
      Resolver.jcenterRepo
    ),

    organization in ThisBuild := "edu.umass.cs.iesl",
    scalaVersion in ThisBuild := "2.11.8",
    // scalaVersion in ThisBuild := "2.12.1",
    // scalaOrganization in ThisBuild := "org.typelevel",
    scalacOptions in ThisBuild ++= SensibleProject.scalaOptionList,

    shellPrompt in ThisBuild := colorPrompt,
    autoCompilerPlugins in ThisBuild := true,
    ivyLoggingLevel in ThisBuild := UpdateLogging.Quiet
  )


}
