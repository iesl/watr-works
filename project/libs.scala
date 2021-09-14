import sbt._
import Keys._

trait LibVersions {
  val cats = Seq(
    // "org.typelevel" %% "cats-effect" % "3.2.2",
    "org.typelevel" %% "cats-effect" % "2.5.3",
    "org.typelevel" %% "cats-core" % "2.6.1"
  )
}

object LibVersions extends LibVersions

object TestLibs extends LibVersions {
  val scalatestVersion = "3.2.9"
  val scalatest        = Seq(
    "org.scalatest" %% "scalatest" % scalatestVersion % "test"
  )

  val scalacheck = Seq(
    "org.scalaz"     %% "scalaz-scalacheck-binding" % "7.3.5"  % Test,
    "org.scalacheck" %% "scalacheck"                % "1.15.4" % "test" //  force()
  )

  val testAndCheck = scalatest ++ scalacheck
}

object LogLibs extends LibVersions {
  val logbackVersion = "1.7.32"
  val logback        = Seq(
    "org.log4s"     %% "log4s"           % "1.10.0",
    "ch.qos.logback" % "logback-classic" % "1.2.6",
    "org.slf4j"      % "slf4j-api"       % logbackVersion,
    "org.slf4j"      % "jul-to-slf4j"    % logbackVersion,
    "org.slf4j"      % "jcl-over-slf4j"  % logbackVersion
  )
}

object DatabaseLibs extends LibVersions {
  val doobieVersion = "0.7.0"
  val postgresqlV   = "42.2.6"

  val doobieDb = Seq(
    "org.tpolecat"           %% "doobie-core"     % doobieVersion,
    "org.tpolecat"           %% "doobie-postgres" % doobieVersion,
    "org.tpolecat"           %% "doobie-hikari"   % doobieVersion,
    "org.tpolecat"           %% "doobie-specs2"   % doobieVersion % "test",
    "org.postgresql"          % "postgresql"      % postgresqlV,
    "org.javassist"           % "javassist"       % "3.25.0-GA",
    "com.impossibl.pgjdbc-ng" % "pgjdbc-ng"       % "0.8.2"
  )

}

trait CommonLibs extends LibVersions {
  val ammoniteVersion = "2.4.0"
  val ammonite        = "com.lihaoyi"  % "ammonite"     % ammoniteVersion cross CrossVersion.full
  val ammoniteOps     = "com.lihaoyi" %% "ammonite-ops" % ammoniteVersion

  val scalaGraph                 = "org.scala-graph" %% "graph-core" % "1.13.3"

  val scopt = "com.github.scopt" %% "scopt" % "4.0.1"

  val shapelessV = "2.3.7"
  val shapeless  = "com.chuusai" %% "shapeless" % shapelessV

  val acyclicVersion = "0.1.7"
  val acyclic        = "com.lihaoyi" %% "acyclic" % acyclicVersion % "provided"

  // val guavaV = "23.0"
  val guavaV = "30.1.1-jre"
  val guava  = "com.google.guava" % "guava" % guavaV

  // val lucene4s         = "com.outr"                %% "lucene4s"         % luceneV
  val pdfbox = "org.apache.pdfbox" % "pdfbox" % "2.0.24"

  val smile = Seq(
    "com.github.haifengl" %% "smile-scala" % "2.6.0"
  )

  val featranV = "0.7.0"
  val featran  = Seq(
    "com.spotify" %% "featran-core" % featranV
  )

  val zioV = "1.0.11"
  val zio  = Seq(
    "dev.zio" %% "zio"         % zioV,
    "dev.zio" %% "zio-streams" % zioV
  )

  val scalazVersion = "7.3.5"
  val scalaz        = Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion
  )

  val circeJsonVersion = "0.14.1"
  val circeJson        = Seq(
    "io.circe" %% "circe-generic" % circeJsonVersion,
    "io.circe" %% "circe-parser"  % circeJsonVersion,
    "io.circe" %% "circe-literal" % circeJsonVersion
  )

  val consoleUtils = Seq(
    "com.lihaoyi" %% "fansi"      % "0.2.14",
    "com.lihaoyi" %% "sourcecode" % "0.2.7",
    "com.lihaoyi" %% "pprint"     % "0.6.6"
  )

  val rtrees = Seq(
    "com.github.davidmoten" % "rtree"            % "0.8.7",
    "com.github.davidmoten" % "flatbuffers-java" % "1.10.0.2"
  )

  val http4sVersion = "0.22.1"
  val http4s = Seq(
    "org.http4s" %% "http4s-core" % http4sVersion,
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-circe" % http4sVersion,
    "org.http4s" %% "http4s-scalatags" % http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion
  )

  val doodleVersion = "0.9.25"
  // val doodle = "org.creativescala" %% "doodle" % doodleVersion
  val doodle = Seq(
    "org.creativescala" %%  "doodle-core" % doodleVersion,
    "org.creativescala" %%  "doodle-java2d" % doodleVersion,
    "org.creativescala" %%  "doodle-image" % doodleVersion,
    //"org.creativescala" %%  "doodle" % doodleVersion,
    // "org.creativescala" %%  "doodle-explore" % doodleVersion,
    //"org.creativescala" %%  "doodle-interact" % doodleVersion,
    //"org.creativescala" %%  "doodle-reactor" % doodleVersion,
    // "org.creativescala" %%  "doodle-svg" % doodleVersion,
    // "org.creativescala" %%  "doodle-turtle" % doodleVersion,
    // "org.creativescala" %%  "doodle-golden" % doodleVersion,
  )


  val platform = org.bytedeco.javacpp.Loader.Detector.getPlatform

  val javacvLibs = Seq(
    "org.bytedeco"   % "javacpp"    % "1.5.5"        withSources() withJavadoc(),
    "org.bytedeco"   % "javacpp"    % "1.5.5"        classifier platform,
    "org.bytedeco"   % "javacv"     % "1.5.5"        withSources() withJavadoc(),
    "org.bytedeco"   % "opencv"     % "4.5.1-1.5.5"  withSources() withJavadoc(),
    "org.bytedeco"   % "opencv"     % "4.5.1-1.5.5"  classifier platform,
    "org.bytedeco"   % "openblas"     % "0.3.13-1.5.5"  withSources() withJavadoc(),
    "org.bytedeco"   % "openblas"     % "0.3.13-1.5.5"  classifier platform
  )


}

object CommonLibs extends CommonLibs
