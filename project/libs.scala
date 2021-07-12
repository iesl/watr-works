import sbt._
import Keys._

trait LibVersions {
  val catsV       = "2.0.0"
  val catsEffectV = "2.1.3"
}

object LibVersions extends LibVersions

object TestLibs extends LibVersions {
  val scalatestVersion = "3.2.9"
  val scalatest        = Seq(
    "org.scalatest" %% "scalatest" % scalatestVersion % "test"
  )

  val scalacheck = Seq(
    "org.scalaz"     %% "scalaz-scalacheck-binding" % "7.3.4"  % Test,
    "org.scalacheck" %% "scalacheck"                % "1.15.4" % "test" //  force()
  )

  val testAndCheck = scalatest ++ scalacheck
}

object LogLibs extends LibVersions {
  val logbackVersion = "1.7.31"
  val logback        = Seq(
    "org.log4s"     %% "log4s"           % "1.10.0",
    "ch.qos.logback" % "logback-classic" % "1.2.3",
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
  // val ammoniteVersion = "2.3.8-58-aa8b2ab1"
  // vl ammoniteVersion = "2.3.8-122-9be39deb"
  val ammoniteVersion = "2.4.0"
  val ammonite        = "com.lihaoyi"  % "ammonite"     % ammoniteVersion cross CrossVersion.full
  val ammoniteOps     = "com.lihaoyi" %% "ammonite-ops" % ammoniteVersion

  val scalaGraphVersion = "1.13.2"
  val scalaGraph                 = "org.scala-graph" %% "graph-core" % scalaGraphVersion
  val scalaGraphConstrained      = "org.scala-graph" %% "graph-constrained" % scalaGraphVersion

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

  val zioV = "1.0.9"
  val zio  = Seq(
    "dev.zio" %% "zio"         % zioV,
    "dev.zio" %% "zio-streams" % zioV
  )

  val scalazVersion = "7.3.4"
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

  val http4sVersion = "0.21.24"
  val http4s = Seq(
    "org.http4s" %% "http4s-core" % http4sVersion,
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-circe" % http4sVersion,
    "org.http4s" %% "http4s-scalatags" % http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion
  )

  val doodle = "org.creativescala" %% "doodle" % "0.9.23"



  lazy val javacppVersion = "1.5.5"
  lazy val javacv        = "org.bytedeco"      % "javacv-platform"         % javacppVersion 

  // // Platform classifier for native library dependencies
  // lazy val platform = org.bytedeco.javacpp.Loader.Detector.getPlatform

  // // JavaCPP-Preset libraries with native dependencies
  // lazy val javaCvLibs = Seq(
  //   "opencv"   -> "4.5.3"
  //   // "ffmpeg"   -> "4.3.1",
  //   // "openblas" -> "0.3.10"
  // ).flatMap {
  //   case (lib, ver) =>
  //     Seq(
  //       "org.bytedeco" % lib % s"$ver-$javacppVersion",
  //       "org.bytedeco" % lib % s"$ver-$javacppVersion" classifier platform
  //     )
  // }


}

object CommonLibs extends CommonLibs
