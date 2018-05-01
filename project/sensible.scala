import scala.util.{ Properties, Try }
import sbt._
import Keys._

object SensibleProject extends CommonLibs {

  lazy val acyclicPlugin =  Seq(
    addCompilerPlugin("com.lihaoyi" %% "acyclic" % acyclicVersion),
    scalacOptions += "-P:acyclic:force",
    libraryDependencies ++= Seq(acyclic)
  )

  val scala_2_12_RecommendedOptionList = Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfuture",                          // Turn on future language features.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification",             // Enable partial unification in type constructor inference
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused:implicits"            // Warn if an implicit parameter is unused.
    // The following are a pain during active dev, but should be on before releasing code:
    // "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    // "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    // "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    // "-Ywarn-unused:params",              // Warn if a value parameter is unused.
    // "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    // "-Ywarn-unused:privates",            // Warn if a private member is unused.
    // "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
  )

  lazy val wartremoverSettings = Seq(
    // wartremoverWarnings in (Compile, compile) ++= Warts.all
    // wartremoverWarnings ++= Warts.unsafe
    // addCompilerPlugin("org.wartremover" %% "wartremover" % "2.1.0"),
    // scalacOptions += "-P:wartremover:only-warn-traverser:org.brianmckenna.wartremover.warts.Unsafe"
    // more at wartremover.org/doc/warts.html
    //    wartremoverErrors ++= Seq(
    //      Wart.AsInstanceOf,              // type conversion hurts typesafety
    //      Wart.EitherProjectionPartial,   // the 'get' method can throw an exception
    //      Wart.Enumeration,               // Scala's enumerations are slow, use ADTs
    //      Wart.ExplicitImplicitTypes,     // implicits must have explicit type annotations
    //      Wart.FinalCaseClass,            // case class must be sealed - they meant to be simple data types
    //      Wart.FinalVal,                  // final vals cause inconsistency during incremental compilation
    //      Wart.ImplicitConversion,        // implicit conversions are dangerous
    //      Wart.IsInstanceOf,              // pattern matching is safer
    //      Wart.JavaConversions,           // use java collections explicitly
    //      Wart.LeakingSealed,             // the subclasses of sealed traits must be final to avoid leaking
    //      Wart.MutableDataStructures,     // mutable data structures in Scala are generally useless
    //      Wart.Null,                      // null is unsafe and useless in Scala
    //      Wart.OptionPartial,             // don't use Option's get method, it might throw exceptions
    //      Wart.Return,                    // return is spaghetti(and breaks referential transparency)
    //      Wart.StringPlusAny,             // concatenate only a String with an other String
    //      Wart.Throw,                     // don't throw exceptions, use Either or Option
    //      Wart.TraversableOps,            // get, head, tail etc. are unsafe - possible exceptions
    //      Wart.TryPartial,                // Try's get is unsafe
    //      Wart.Var, Wart.While            // these are only useful at micro-optimizations, use tail recursion instead
    // )
  )

  // These settings are required to make Ammonite Repl work properly
  lazy val runForked = Seq(
    fork := true,
    connectInput := true,
    outputStrategy := Some(StdoutOutput),
    baseDirectory in run in Compile := baseDirectory.value / "..",
  )

  // lazy val runForked = Seq(
  //   Compile / run / fork := true,
  //   Compile / run / connectInput := false,
  //   Compile / run / baseDirectory := baseDirectory.value / "..",
  //   outputStrategy := Some(StdoutOutput),
  // )

  lazy val scalaMacroParadiseSettings = Seq(
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
  )

  lazy val scalaMetaSettings = Seq(
    addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full),
    scalacOptions += "-Xplugin-require:macroparadise",
    scalacOptions in (Compile, console) ~= (_ filterNot (_ contains "paradise")) // macroparadise plugin doesn't work in repl yet.
  )

  lazy val macroSettings = scalaMacroParadiseSettings

  lazy val settings =  macroSettings ++ Seq(
    scalaVersion := "2.12.4",
    organization := "edu.umass.cs.iesl",
    scalacOptions ++=  scala_2_12_RecommendedOptionList,

    scalacOptions in (Compile, console) --= Seq(
      "-Ywarn-unused:imports",
      "-Xfatal-warnings"
    ),


    autoCompilerPlugins  := true,
    addCompilerPlugin("org.spire-math" %% "kind-projector"   % "0.9.6"),


    // The matryoshka dependency uses the org.typelevel version of scala, so without this exclusion 2 vers of the scala library get loaded
    excludeDependencies ++= Seq("org.typelevel" % "scala-library"),

    logBuffered in Test       := false,
    fork in Test              := false,
    connectInput in Test      := false,
    parallelExecution in Test := false,
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
      Resolver.jcenterRepo,
      Resolver.bintrayRepo("jmcardon", "tsec")
    ),

    shellPrompt in ThisBuild := colorPrompt
  )


}
