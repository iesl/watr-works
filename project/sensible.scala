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

  lazy val scalaV = "2.13.7"

  val scala_2_13_RecommendedOptionList = Seq(
    "-Xcheckinit",                      // Wrap field accessors to throw an exception on uninitialized access.
    "-Xlint:adapted-args",              // An argument list is modified to match the receiver.
    "-Xlint:constant",                  // Constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",        // Selecting member of DelayedInit.
    "-Xlint:doc-detached",              // A detached Scaladoc comment.
    "-Xlint:inaccessible",              // Inaccessible types in method signatures.
    "-Xlint:infer-any",                 // A type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",      // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-unit",              // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",           // Option.apply used implicit view.
    "-Xlint:package-object-classes",    // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",    // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",            // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",               // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    "-Ybackend-parallelism", "4", // Enable paralellisation — change to desired number!
    "-Ycache-macro-class-loader:last-modified", // and macro definitions. This can lead to performance improvements.
    "-Ycache-plugin-class-loader:last-modified", // Enables caching of classloaders for compiler plugins
    "-Ymacro-annotations",                //
    "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Ywarn-extra-implicit",            // More than one implicit parameter section is defined.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals", // Warn if a local definition is unused.
    // "-Ywarn-unused:params", // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates", // Warn if a private member is unused.
    // "-Ywarn-value-discard", // Warn when non-Unit expression results are unused.
    "-deprecation",                     // Warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",               // Specify character encoding used by source files.
    "-explaintypes",                    // Explain type errors in more detail.
    "-feature",                         // For features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                       // Generated code depends on assumptions.
    // "-Vimplicits",         // splain plugin for scalav < 2.13.6
    // "-Vtype-diffs",        // splain plugin for scalav < 2.13.6
  )

  // "-Xfatal-warnings", // Fail the compilation if there are any warnings.


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
     // BuildInfoKey.constant("timezone" -> "UTC")
    ),
    buildInfoOptions ++= Seq(
      BuildInfoOption.ToMap,
      BuildInfoOption.ToJson,
      BuildInfoOption.BuildTime
    )
  )

  lazy val settings = Seq(
    autoCompilerPlugins  := true,

    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    // addCompilerPlugin("io.tryp" % "splain" % "0.5.8" cross CrossVersion.patch),

    scalaVersion := scalaV,
    organization := "org.watrworks",
    scalacOptions ++=  scala_2_13_RecommendedOptionList,

     Compile / console / scalacOptions --= Seq(
      "-Ywarn-unused:imports",
      "-Xfatal-warnings"
    ),

     ThisBuild / resolvers ++= List(
      Resolver.sonatypeRepo("releases"),
      Resolver.jcenterRepo,
      Resolver.bintrayRepo("jmcardon", "tsec")
    ),

     ThisBuild / shellPrompt := colorPrompt,

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
