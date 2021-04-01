import sbt._
import Keys._

trait LibVersions {
  val catsV       = "2.0.0"
  val catsEffectV = "2.1.3"
}

object LibVersions extends LibVersions

object TestLibs extends LibVersions {
  val scalatestVersion = "3.2.6"
  val scalatest        = Seq(
    "org.scalatest" %% "scalatest" % scalatestVersion % "test"
  )

  val scalacheck = Seq(
    "org.scalaz"     %% "scalaz-scalacheck-binding" % "7.3.3"  % Test,
    "org.scalacheck" %% "scalacheck"                % "1.15.3" % "test" //  force()
  )

  val testAndCheck = scalatest ++ scalacheck
}

object LogLibs extends LibVersions {
  val logbackVersion = "1.7.30"
  val logback        = Seq(
    "org.log4s"     %% "log4s"           % "1.9.0",
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
  val ammoniteVersion = "2.3.8-58-aa8b2ab1"
  // val ammoniteVersion = "2.3.8"
  val ammonite        = "com.lihaoyi"  % "ammonite"     % ammoniteVersion cross CrossVersion.full
  val ammoniteOps     = "com.lihaoyi" %% "ammonite-ops" % ammoniteVersion

  val scopt = "com.github.scopt" %% "scopt" % "4.0.1"

  val shapelessV = "2.3.3"
  val shapeless  = "com.chuusai" %% "shapeless" % shapelessV

  val acyclicVersion = "0.1.7"
  val acyclic        = "com.lihaoyi" %% "acyclic" % acyclicVersion % "provided"

  val guavaV = "23.0"
  val guava  = "com.google.guava" % "guava" % guavaV

  // val lucene4s         = "com.outr"                %% "lucene4s"         % luceneV
  val pdfbox = "org.apache.pdfbox" % "pdfbox" % "2.0.23"

  val smile = Seq(
    "com.github.haifengl" %% "smile-scala" % "2.6.0"
  )

  val featranV = "0.7.0"
  val featran  = Seq(
    "com.spotify" %% "featran-core" % featranV
  )

  val zioV = "1.0.5"
  val zio  = Seq(
    "dev.zio" %% "zio"         % zioV,
    "dev.zio" %% "zio-streams" % zioV
  )

  val scalazVersion = "7.3.3"
  val scalaz        = Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion
  )

  val circeJsonVersion = "0.13.0"
  val circeJson        = Seq(
    "io.circe" %% "circe-generic" % circeJsonVersion,
    "io.circe" %% "circe-parser"  % circeJsonVersion,
    "io.circe" %% "circe-literal" % circeJsonVersion
  )

  val consoleUtils = Seq(
    "com.lihaoyi" %% "fansi"      % "0.2.11",
    "com.lihaoyi" %% "sourcecode" % "0.2.4",
    "com.lihaoyi" %% "pprint"     % "0.6.3"
  )

  val rtrees = Seq(
    "com.github.davidmoten" % "rtree"            % "0.8.7",
    "com.github.davidmoten" % "flatbuffers-java" % "1.10.0.2"
  )

  val http4sVersion = "0.21.21"
  val http4s = Seq(
    "org.http4s" %% "http4s-core" % http4sVersion,
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-circe" % http4sVersion,
    "org.http4s" %% "http4s-scalatags" % http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion
  )
}

object CommonLibs extends CommonLibs
