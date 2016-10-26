import scala.util.{ Properties, Try }
import sbt._
import Keys._

// Copyright 2016 Sam Halliday
// Licence: http://www.apache.org/licenses/LICENSE-2.0

import com.github.fedragon.todolist.TodoListPlugin.autoImport._
import com.lihaoyi.workbench.Plugin._

trait LibVersions {
  val scalazVersion       = "7.2.6"
  val scalaTagsVersion    = "0.6.2"
  val scalaAsyncVersion   = "0.9.6-RC6"
  val scalaModulesVersion = "1.0.4"
  val akkaVersion         = "2.3.14"
  val streamsVersion      = "1.0"
  val scalatestVersion    = "3.0.0"
  val logbackVersion      = "1.7.21"
  val quasiquotesVersion  = "2.0.1"
  val guavaVersion        = "18.0"
  val specs2Version       = "3.7"

}

object LibVersions extends LibVersions

object LogLibs extends LibVersions {
  val logback = Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.7",
    "org.slf4j" % "slf4j-api" % logbackVersion,
    "org.slf4j" % "jul-to-slf4j" % logbackVersion,
    "org.slf4j" % "jcl-over-slf4j" % logbackVersion
  )
}

object TestLibs extends LibVersions {
}

object ThisBuildDefault {
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
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      Resolver.jcenterRepo
    ),

    scalaVersion in ThisBuild := "2.11.8",

    shellPrompt in ThisBuild := colorPrompt,

    autoCompilerPlugins in ThisBuild := true,

    addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.5"),

    ivyLoggingLevel in ThisBuild := UpdateLogging.Quiet,

    todosTags in ThisBuild := Set("FIXME", "TODO", "WIP", "XXX", "\\?\\?\\?"),

    scalacOptions in ThisBuild ++= Seq(
      "-encoding", "UTF-8",
      "-target:jvm-1.6",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-Xlint",
      "-Yinline-warnings",
      "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
      "-Ywarn-inaccessible",
      "-Ywarn-dead-code",
      "-Xfuture"
        // "-Ywarn-unused-import", // noisy, but good to run occasionally
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

    // 4 x 1GB = 4GB
    concurrentRestrictions in Global := Seq(Tags.limitAll(4))

  )

}


object Sensible extends LibVersions {



  lazy val settings =  Seq(


    // addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.4"),


    javacOptions in (Compile, compile) ++= Seq(
      "-source", "1.6", "-target", "1.6", "-Xlint:all", "-Werror",
      "-Xlint:-options", "-Xlint:-path", "-Xlint:-processing"
    ),
    javacOptions in doc ++= Seq("-source", "1.6"),

    javaOptions := Seq(
      "-Xss2m", "-Xms1g", "-Xmx1g", "-Dfile.encoding=UTF8"
    ),

    // 4 x 1GB = 4GB
    concurrentRestrictions in Global := Seq(Tags.limitAll(4)),


    dependencyOverrides ++= Set(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-library" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scalap" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-xml" % scalaModulesVersion,
      "org.scala-lang.modules" %% "scala-parser-combinators" % scalaModulesVersion,
      "org.scalamacros" %% "quasiquotes" % quasiquotesVersion
    ) ++ LogLibs.logback
  ) ++ inConfig(Test)(testSettings)

  def testSettings = Seq(
    parallelExecution := true,

    // one JVM per test suite
    // fork := true,
    testForkedParallel := true,
    testGrouping <<= (
      definedTests,
      baseDirectory,
      javaOptions,
      outputStrategy,
      envVars,
      javaHome,
      connectInput
    ).map { (tests, base, options, strategy, env, javaHomeDir, connectIn) =>
        val opts = ForkOptions(
          bootJars = Nil,
          javaHome = javaHomeDir,
          connectInput = connectIn,
          outputStrategy = strategy,
          runJVMOptions = options,
          workingDirectory = Some(base),
          envVars = env
        )
        tests.map { test =>
          Tests.Group(test.name, Seq(test), Tests.SubProcess(opts))
        }
      },

    testOptions ++= noColorIfEmacs,
    logBuffered in Test := false,
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1", "-verbosity", "1"),
    testFrameworks := Seq(TestFrameworks.ScalaTest, TestFrameworks.ScalaCheck)
  )

  // WORKAROUND: https://github.com/scalatest/scalatest/issues/511
  def noColorIfEmacs =
    if (sys.env.get("INSIDE_EMACS").isDefined)
      Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oWF"))
    else
      Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oF"))


}
