import scala.util.{ Properties, Try }
import sbt._
import Keys._

// Copyright 2016 Sam Halliday
// Licence: http://www.apache.org/licenses/LICENSE-2.0

/**
 * A bunch of sensible defaults
 */
object Sensible {

  // shellPrompt in ThisBuild := { s: State =>
  def colorPrompt = { s: State =>
    val c = scala.Console
    val blue = c.RESET + c.BLUE + c.BOLD
    val white = c.RESET + c.BOLD
    val projectName = Project.extract(s).currentProject.id

    "[" + blue + projectName + white + "]>> " + c.RESET
  }


  import com.github.fedragon.todolist.TodoListPlugin.autoImport._

  lazy val settings =  Seq(

    ivyLoggingLevel := UpdateLogging.Quiet,

    todosTags := Set("FIXME", "TODO", "WIP", "XXX", "\\?\\?\\?"),

    scalacOptions in Compile ++= Seq(
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
      "-Xcheckinit", // runtime error when a val is not initialized due to trait hierarchies (instead of NPE somewhere else)
      "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
      "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
      "-Ywarn-inaccessible",
      "-Ywarn-dead-code",
      //"-Ywarn-numeric-widen", // noisy
      "-Ywarn-unused-import", // noisy, but good to run occasionally
      "-Xfuture"
    ),

    javacOptions in (Compile, compile) ++= Seq(
      "-source", "1.6", "-target", "1.6", "-Xlint:all", "-Werror",
      "-Xlint:-options", "-Xlint:-path", "-Xlint:-processing"
    ),
    javacOptions in doc ++= Seq("-source", "1.6"),

    javaOptions := Seq("-Xss2m", "-Xms1g", "-Xmx1g"),
    javaOptions += "-Dfile.encoding=UTF8",
    // javaOptions ++= Seq("-XX:+UseConcMarkSweepGC"),

    // maxErrors := 1,
    // fork := true,

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
    ) ++ logback // ++ guava ++ shapeless(scalaVersion.value)
  ) ++ inConfig(Test)(testSettings)
// ) ++ inConfig(Test)(testSettings) ++ scalariformSettings

  // TODO: scalariformSettingsWithIt generalised
  def testSettings = Seq(
    parallelExecution := true,

    // one JVM per test suite
    fork := true,
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
    testFrameworks := Seq(TestFrameworks.ScalaTest, TestFrameworks.JUnit)
  )

  val scalaModulesVersion = "1.0.4"
  val akkaVersion = "2.3.14"
  val streamsVersion = "1.0"
  val scalatestVersion = "3.0.0"
  val logbackVersion = "1.7.21"
  val quasiquotesVersion = "2.0.1"
  val guavaVersion = "18.0"

  val macroParadise = Seq(
    compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
  )
  def shapeless(scalaVersion: String) = {
    if (scalaVersion.startsWith("2.10.")) macroParadise
    else Nil
  } :+ "com.chuusai" %% "shapeless" % "2.2.5"
  val logback = Seq(
    "ch.qos.logback" % "logback-classic" % "1.1.7",
    "org.slf4j" % "slf4j-api" % logbackVersion,
    "org.slf4j" % "jul-to-slf4j" % logbackVersion,
    "org.slf4j" % "jcl-over-slf4j" % logbackVersion
  )
  val guava = Seq(
    "com.google.guava" % "guava" % guavaVersion,
    "com.google.code.findbugs" % "jsr305" % "3.0.1" % "provided"
  )

  def testLibs(config: String = "test") = Seq(
    "org.scalatest" %% "scalatest" % scalatestVersion % config
    // "org.scalamock" %% "scalamock-scalatest-support" % "3.2.2" % config,
    // "org.scalacheck" %% "scalacheck" % "1.13.1" % config,
    // "com.typesafe.akka" %% "akka-testkit" % akkaVersion % config,
    // "com.typesafe.akka" %% "akka-slf4j" % akkaVersion % config
  ) ++ logback.map(_ % config)


  // WORKAROUND: https://github.com/scalatest/scalatest/issues/511
  def noColorIfEmacs =
    if (sys.env.get("INSIDE_EMACS").isDefined)
      Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oWF"))
    else
      Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oF"))


}
