import scala.util.{ Properties, Try }
import sbt._
import Keys._

object SensibleProject extends CommonLibs {

  def noColorIfEmacs = {
    // WORKAROUND: https://github.com/scalatest/scalatest/issues/511
    if (sys.env.get("INSIDE_EMACS").isDefined)
      Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oWF"))
    else
      Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oF"))
  }

  lazy val acyclicPlugin =  Seq(
    addCompilerPlugin("com.lihaoyi" %% "acyclic" % acyclicVersion),
    scalacOptions += "-P:acyclic:force",
    libraryDependencies ++= Seq(acyclic)
  )

  lazy val settings =  Seq(
    scalaVersion := "2.11.8",
    addCompilerPlugin("org.spire-math" %% "kind-projector"   % "0.9.3"),
    addCompilerPlugin("org.scalamacros" % "paradise"         % "2.1.0" cross CrossVersion.full),
    addCompilerPlugin("com.milessabin"  % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)
  )

  lazy val testSettings = Seq(
    logBuffered in Test := false,
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1", "-verbosity", "1"),
    testFrameworks := Seq(TestFrameworks.ScalaTest, TestFrameworks.ScalaCheck)
  )
}

object SensibleThisBuild {
  def colorPrompt = { s: State =>
    val c = scala.Console
    val blue = c.RESET + c.BLUE + c.BOLD
    val white = c.RESET + c.BOLD
    val projectName = Project.extract(s).currentProject.id

    "[" + blue + projectName + white + "]>> " + c.RESET
  }

  lazy val settings =  Seq(

    resolvers in ThisBuild ++= List(
      "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public",
      // "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases"),
      Resolver.jcenterRepo
    ),

    organization in ThisBuild := "edu.umass.cs.iesl",
    // scalaOrganization in ThisBuild := "org.typelevel",
    scalaVersion in ThisBuild := "2.11.8",
    shellPrompt in ThisBuild := colorPrompt,
    autoCompilerPlugins in ThisBuild := true,
    ivyLoggingLevel in ThisBuild := UpdateLogging.Quiet,

    scalacOptions in ThisBuild ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-target:jvm-1.6",
      "-unchecked",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      // "-Ypartial-unification", // typelevel.org scala specific
      // "-language:postfixOps",
      "-Ypatmat-exhaust-depth", "40",
      "-Xlint",
      "-Yinline-warnings",
      "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
      "-Ywarn-inaccessible",
      "-Ywarn-unused-import", // noisy, but good to run occasionally
      "-Ywarn-dead-code",
      "-Xfuture"

      // "-Xcheckinit", // runtime error when a val is not initialized due to trait hierarchies (instead of NPE somewhere else)
      // "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
      //"-Ywarn-numeric-widen", // noisy
    ),

    javacOptions in (Compile, compile) ++= Seq(
      "-source", "1.6", "-target", "1.6", "-Xlint:all", "-Werror",
      "-Xlint:-options", "-Xlint:-path", "-Xlint:-processing"
    ),
    javacOptions in doc ++= Seq("-source", "1.6"),

    javaOptions := Seq(
      "-Xss2m", "-Xms1g", "-Xmx1g", "-Dfile.encoding=UTF8"
    ),

    concurrentRestrictions in Global := Seq(Tags.limitAll(4))

  )


}


