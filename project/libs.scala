import sbt._
import Keys._
// import scala.util.{ Properties, Try }


trait LibVersions {
  val scalazVersion       = "7.2.8"
  val scalaTagsVersion    = "0.6.2"
  val scalaAsyncVersion   = "0.9.6"
  val scalaModulesVersion = "1.0.4"
  val akkaVersion         = "2.3.14"
  val streamsVersion      = "1.0"
  val scalatestVersion    = "3.0.1"
  val logbackVersion      = "1.7.22"
  val quasiquotesVersion  = "2.0.1"
  val guavaVersion        = "18.0"
  val specs2Version       = "3.7"
  val scrimageVersion     = "2.1.8"
  val monocleVersion      = "1.2.2"
  val aspectjVersion      = "1.8.9"
  val acyclicVersion      = "0.1.7"
  val doobieVersion       = "0.3.0"

}

object SensibleLib extends LibVersions {
  lazy val settings = Seq(
    dependencyOverrides ++= Set(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-library" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scalap" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-xml" % scalaModulesVersion,
      "org.scala-lang.modules" %% "scala-parser-combinators" % scalaModulesVersion
      // "org.scalamacros" %% "quasiquotes" % quasiquotesVersion
    )
  )
}

object LibVersions extends LibVersions

object TestLibs extends LibVersions {
  val scalatest = Seq(
    "org.scalatest" %% "scalatest" % scalatestVersion % "test"
    // "org.scalactic" %% "scalactic" % scalatestVersion
  )

  val scalacheck = Seq(
    "org.scalaz"     %% "scalaz-scalacheck-binding" % scalazVersion  % "test",
    "org.scalacheck" %% "scalacheck"                % "1.12.5"       % "test" force()
  )

  val testAndCheck = scalatest ++ scalacheck
}

object LogLibs extends LibVersions {
  val logback = Seq(
    "org.log4s"      %% "log4s"            % "1.3.4",
    "ch.qos.logback"  % "logback-classic"  % "1.1.8",
    "org.slf4j"       % "slf4j-api"        % logbackVersion,
    "org.slf4j"       % "jul-to-slf4j"     % logbackVersion,
    "org.slf4j"       % "jcl-over-slf4j"   % logbackVersion
  )
}

object DatabaseLibs extends LibVersions {
  val slickDb = Seq(
    "org.bouncycastle" % "bcprov-jdk15on" % "1.56",
    "org.bouncycastle" % "bcpkix-jdk15on" % "1.56",
    "com.h2database" % "h2" % "1.4.193",
    "com.zaxxer" % "HikariCP" % "2.5.1",
    "com.typesafe.slick" %% "slick" % "3.1.1"
  )

  val doobieDb = Seq(
    "org.tpolecat" %% "doobie-core"               % doobieVersion,
    "org.tpolecat" %% "doobie-contrib-postgresql" % doobieVersion,
    "org.tpolecat" %% "doobie-contrib-hikari"     % doobieVersion,
    "org.tpolecat" %% "doobie-contrib-specs2"     % doobieVersion
  )

}

// trait JsLib extends LibVersions {
//   import org.scalajs.sbtplugin.ScalaJSPlugin
//   import ScalaJSPlugin._
//   import ScalaJSPlugin.autoImport._
//   val scalatags        = "com.lihaoyi"            %%% "scalatags"        % scalaTagsVersion
// }
// object JsLib extends JsLib

trait CommonLibs extends LibVersions {

  val scalazCore       = "org.scalaz"              %% "scalaz-core"      % scalazVersion
  val scalaAsync       = "org.scala-lang.modules"  %% "scala-async"      % scalaAsyncVersion
  val scalatags        = "com.lihaoyi"             %% "scalatags"        % scalaTagsVersion
  val ammonite         = "com.lihaoyi"              % "ammonite"         % "0.8.1" cross CrossVersion.full
  val fastparse        = "com.lihaoyi"             %% "fastparse"        % "0.4.2"
  val sourcecode       = "com.lihaoyi"             %% "sourcecode"       % "0.1.3"
  val playJson         = "com.typesafe.play"       %% "play-json"        % "2.5.10"
  val scopt            = "com.github.scopt"        %% "scopt"            % "3.5.0"
  val machinist        = "org.typelevel"           %% "machinist"        % "0.6.1"
  val shapeless        = "com.chuusai"             %% "shapeless"        % "2.3.2"
  val aspectJ          = "org.aspectj"              % "aspectjweaver"    % aspectjVersion
  val acyclic          = "com.lihaoyi"             %% "acyclic"          % acyclicVersion % "provided"

  // val matryoshkaCore   = "com.slamdata"            %% "matryoshka-core"  % "0.11.1"
  // Needed for matryoshka, not needed when I properly build it (rather than putting in ./lib dir)
  val matryoshkaLibs = Seq(
    "com.github.julien-truffaut" %% "monocle-core" % monocleVersion % "compile, test",
    "org.scalaz"                 %% "scalaz-core"  % scalazVersion  % "compile, test",
    "com.github.mpilquist"       %% "simulacrum"   % "0.10.0"       % "compile, test"
  )

  val scrimageCore = "com.sksamuel.scrimage" %% "scrimage-core"   % scrimageVersion

  val scrimage = Seq(
    scrimageCore,
    "com.sksamuel.scrimage" %% "scrimage-io-extra" % scrimageVersion,
    "com.sksamuel.scrimage" %% "scrimage-filters"  % scrimageVersion
  )
}

object CommonLibs extends CommonLibs
