import scala.util.{ Properties, Try }
import sbt._
import Keys._

object SensibleProject extends CommonLibs {
  def colorPrompt = { s: State =>
    val c = scala.Console
    val blue = c.RESET + c.CYAN + c.BOLD
    val white = c.RESET + c.WHITE + c.BOLD
    val projectName = Project.extract(s).currentProject.id

    "[" + blue + projectName + white + "]>> " + c.RESET
  }

  lazy val acyclicPlugin =  Seq(
    addCompilerPlugin("com.lihaoyi" %% "acyclic" % acyclicVersion),
    scalacOptions += "-P:acyclic:force",
    libraryDependencies ++= Seq(acyclic)
  )

  lazy val scalaV = "2.13.1"

  val scala_2_13_RecommendedOptionList = Seq(
    "-Xcheckinit",                      // +n+ Wrap field accessors to throw an exception on uninitialized access.
    "-Xlint:adapted-args",              // +n+ An argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative", // +n+ By-name parameter of right associative operator.
    "-Xlint:constant",                  // +n+ Constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",        // +n+ Selecting member of DelayedInit.
    "-Xlint:doc-detached",              // +n+ A detached Scaladoc comment.
    "-Xlint:inaccessible",              // +n+ Inaccessible types in method signatures.
    "-Xlint:infer-any",                 // +n+ A type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",      // +n+ A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",          // +n+ Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",              // +n+ Warn when nullary methods return Unit.
    "-Xlint:option-implicit",           // +n+ Option.apply used implicit view.
    "-Xlint:package-object-classes",    // +n+ Class or object defined in package object.
    "-Xlint:poly-implicit-overload",    // +n+ Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",            // +n+ A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",               // +n+ Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",     // +n+ A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",             // +n+ Pattern match may not be typesafe.
    "-Yno-adapted-args",                // +n+ Do not autotuple.
    "-Ywarn-extra-implicit",            // +n+ More than one implicit parameter section is defined.
    "-Ywarn-inaccessible",              // +n+ Inaccessible types in method signatures.
    "-Ywarn-infer-any",                 // +n+ A type argument is inferred to be `Any`.
    "-Ywarn-nullary-override",          // +n+ non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",              // +n+ nullary method returns Unit.
    "-deprecation",                     // +n+ Warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",               // +n+ Specify character encoding used by source files.
    "-explaintypes",                    // +n+ Explain type errors in more detail.
    "-feature",                         // +n+ For features that should be imported explicitly.
    "-unchecked",                       // +n+ Generated code depends on assumptions.

  )


  // (possibly) legacy
  // "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
  // "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
  // "-language:higherKinds",             // Allow higher-kinded types
  // "-language:implicitConversions",     // Allow definition of implicit functions called views
  // "-Ymacro-annotations"                 // 

  // Bothersome:The following are a pain during active dev, but should be on before releasing code:
  // +n+  "-Ywarn-dead-code",  // +n+ Warn when dead code is identified.
  // +n+  "-Ywarn-numeric-widen",  // +n+ Numerics are implicitly widened.

  // "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
  // "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
  // "-Ywarn-unused:locals",              // Warn if a local definition is unused.
  // "-Ywarn-unused:params",              // Warn if a value parameter is unused.
  // "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
  // "-Ywarn-unused:privates",            // Warn if a private member is unused.
  // "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.


  // These settings are required to make Ammonite Repl work properly
  lazy val runForked = Seq(
    Compile / run / fork := true,
    Compile / run / connectInput := true,
    Compile / run / baseDirectory := baseDirectory.value / "..",
    outputStrategy := Some(StdoutOutput),
  )

  lazy val runUnforked = Seq(
    Compile / run / fork := false,
    Compile / run / baseDirectory := baseDirectory.value / "..",
  )

  import com.typesafe.sbt.SbtGit.git
  import sbtbuildinfo.BuildInfoPlugin.autoImport.{buildInfoKeys, buildInfoOptions}
  import sbtbuildinfo.{BuildInfoKey, BuildInfoOption, BuildInfoPlugin}

  val buildInfoSettings = Seq(
    buildInfoKeys := Seq[BuildInfoKey](
      BuildInfoKey.map(name) { case (k, v) => "appName" -> v },
      BuildInfoKey.map(version) { case (k, v) => "appVersion" -> v },
      scalaVersion,
      git.gitHeadCommit,
      git.gitCurrentBranch,
      BuildInfoKey.constant("timezone" -> "UTC")
    ),
    buildInfoOptions ++= Seq(
      BuildInfoOption.ToMap,
      BuildInfoOption.ToJson,
      BuildInfoOption.BuildTime
    )
  )

  lazy val settings = Seq(
    autoCompilerPlugins  := true,

    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),

    scalaVersion := scalaV,
    organization := "org.watrworks",
    scalacOptions ++=  scala_2_13_RecommendedOptionList,

    scalacOptions in (Compile, console) --= Seq(
      "-Ywarn-unused:imports",
      "-Xfatal-warnings"
    ),

    resolvers in ThisBuild ++= List(
      // Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases"),
      Resolver.jcenterRepo,
      Resolver.bintrayRepo("jmcardon", "tsec")
    ),

    shellPrompt in ThisBuild := colorPrompt,

    // The matryoshka dependency uses the org.typelevel version of scala, so without this exclusion 2 vers of the scala library get loaded
    excludeDependencies ++= Seq("org.typelevel" % "scala-library"),

    Test / logBuffered        := false,
    Test / fork               := false,
    Test / connectInput       := false,
    Test / parallelExecution  := false,
    Test / testOptions        += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1", "-verbosity", "1"),
    Test / testOptions        += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    testFrameworks            := Seq(TestFrameworks.ScalaTest, TestFrameworks.ScalaCheck)
  )

}
