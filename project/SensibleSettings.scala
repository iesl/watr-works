import scala.util.{ Properties, Try }
import sbt._
import Keys._

// import com.github.fedragon.todolist.TodoListPlugin.autoImport._
import com.lihaoyi.workbench.Plugin._

trait LibVersions {
  val scalazVersion       = "7.2.7"
  val scalaTagsVersion    = "0.6.2"
  val scalaAsyncVersion   = "0.9.6"
  val scalaModulesVersion = "1.0.4"
  val akkaVersion         = "2.3.14"
  val streamsVersion      = "1.0"
  val scalatestVersion    = "3.0.0"
  val logbackVersion      = "1.7.21"
  val quasiquotesVersion  = "2.0.1"
  val guavaVersion        = "18.0"
  val specs2Version       = "3.7"
  val scrimageVersion     = "2.1.7"
  val monocleVersion      = "1.3.1"
}

object LibVersions extends LibVersions

object TestLibs extends LibVersions {
  val scalatest = Seq(
    "org.scalatest" %% "scalatest" % scalatestVersion % "test"
    // "org.scalactic" %% "scalactic" % scalatestVersion
  )

  val scalacheck = Seq(
    "org.scalaz"     %% "scalaz-scalacheck-binding" % scalazVersion  % "test",
    "org.scalacheck" %% "scalacheck"                % "1.13.4"       % "test" force()
  )

  val testAndCheck = scalatest ++ scalacheck
}

object LogLibs extends LibVersions {
  val logback = Seq(
    "org.log4s"      %% "log4s"            % "1.3.2",
    "ch.qos.logback"  % "logback-classic"  % "1.1.7",
    "org.slf4j"       % "slf4j-api"        % logbackVersion,
    "org.slf4j"       % "jul-to-slf4j"     % logbackVersion,
    "org.slf4j"       % "jcl-over-slf4j"   % logbackVersion
  )
}

object DatabaseLibs extends LibVersions {
  val slickDb = Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % "1.55",
    "org.bouncycastle" % "bcpkix-jdk15on" % "1.55",
    "com.h2database" % "h2" % "1.4.192",
    "com.zaxxer" % "HikariCP" % "2.5.1",
    "com.typesafe.slick" %% "slick" % "3.1.1"
  )

}
object CommonLibs extends LibVersions {

  val scalazCore       = "org.scalaz"              %% "scalaz-core"      % scalazVersion
  val scalaAsync       = "org.scala-lang.modules"  %% "scala-async"      % scalaAsyncVersion
  val scalatags        = "com.lihaoyi"             %% "scalatags"        % scalaTagsVersion
  val ammonite         = "com.lihaoyi"              % "ammonite"         % "0.7.8" cross CrossVersion.full
  val fastparse        = "com.lihaoyi"             %% "fastparse"        % "0.4.2"
  val playJson         = "com.typesafe.play"       %% "play-json"        % "2.5.9"
  val scopt            = "com.github.scopt"        %% "scopt"            % "3.5.0"
  val machinist        = "org.typelevel"           %% "machinist"        % "0.6.1"
  val shapeless        = "com.chuusai"             %% "shapeless"        % "2.3.2"

  val matryoshkaCore   = "com.slamdata"            %% "matryoshka-core"  % "0.11.1"
  // Needed for matryoshka, not needed when I properly build it (rather than putting in ./lib dir)
  val matryoshkaLibs = Seq(
    "com.github.julien-truffaut" %% "monocle-core" % monocleVersion % "compile, test",
    "org.scalaz"                 %% "scalaz-core"  % scalazVersion  % "compile, test",
    "com.github.mpilquist"       %% "simulacrum"   % "0.10.0"       % "compile, test"
  )

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

    // addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.5"),

    ivyLoggingLevel in ThisBuild := UpdateLogging.Quiet,

    // todosTags in ThisBuild := Set("FIXME", "TODO", "WIP", "XXX", "\\?\\?\\?"),

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
