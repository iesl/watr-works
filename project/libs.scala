import sbt._
import Keys._

trait LibVersions {
  val scalazVersion       = "7.3.3"
  val scalatestVersion    = "3.2.3"
  val logbackVersion      = "1.7.30"
  val scrimageVersion     = "2.1.8"
  val acyclicVersion      = "0.1.7"
  val doobieVersion       = "0.7.0"
  val matryoshkaCoreV     = "0.21.3"
  val sourcecodeV         = "0.2.3"
  val fansiV              = "0.2.10"
  val shapelessV          = "2.3.3"
  val scaladgetV          = "0.9.5"
  val http4sVersion       = "0.20.6"
  val fs2Version          = "2.5.0"
  val circeJsonVersion    = "0.13.0"
  val ammoniteVersion     = "2.3.8"
  val catsV               = "2.0.0"
  val catsEffectV         = "2.1.3"
  val postgresqlV         = "42.2.6"
  val freestyleV          = "0.7.0"
  val guavaV              = "23.0"
  val luceneV             = "1.10.0"
  val featranV            = "0.7.0"
}

object LibVersions extends LibVersions

object TestLibs extends LibVersions {
  val scalatest = Seq(
    "org.scalatest" %% "scalatest" % scalatestVersion % "test"
  )

  val scalacheck = Seq(
    "org.scalaz"        %% "scalaz-scalacheck-binding" % "7.3.3" % Test,
    "org.scalacheck" %% "scalacheck"                % "1.15.2"       % "test" //  force()
  )

  val testAndCheck = scalatest ++ scalacheck
}

object LogLibs extends LibVersions {
  val logback = Seq(
    "org.log4s"      %% "log4s"            % "1.9.0",
    "ch.qos.logback"  % "logback-classic"  % "1.2.3",
    "org.slf4j"       % "slf4j-api"        % logbackVersion,
    "org.slf4j"       % "jul-to-slf4j"     % logbackVersion,
    "org.slf4j"       % "jcl-over-slf4j"   % logbackVersion
  )
}

object DatabaseLibs extends LibVersions {

  val doobieDb = Seq(
    "org.tpolecat"      %% "doobie-core"       % doobieVersion,
    "org.tpolecat"      %% "doobie-postgres"   % doobieVersion,
    "org.tpolecat"      %% "doobie-hikari"     % doobieVersion,
    "org.tpolecat"      %% "doobie-specs2"     % doobieVersion % "test",
    "org.postgresql"     % "postgresql"        % postgresqlV,
    "org.javassist"      % "javassist"         % "3.25.0-GA",
    "com.impossibl.pgjdbc-ng" % "pgjdbc-ng"  % "0.8.2"
  )

}

trait CommonLibs extends LibVersions {
  val ammonite         = "com.lihaoyi"             % "ammonite"          % ammoniteVersion cross CrossVersion.full
  val ammoniteOps      = "com.lihaoyi"             %% "ammonite-ops"     % ammoniteVersion
  val scopt            = "com.github.scopt"        %% "scopt"            % "4.0.0"
  val shapeless        = "com.chuusai"             %% "shapeless"        % shapelessV
  val acyclic          = "com.lihaoyi"             %% "acyclic"          % acyclicVersion % "provided"
  val guava            = "com.google.guava"         % "guava"            % guavaV
  val lucene4s         = "com.outr"                %% "lucene4s"         % luceneV
  val fs2Core          = "co.fs2"                  %% "fs2-core" % fs2Version
  val fs2IO            = "co.fs2"                  %% "fs2-io" % fs2Version
  val pdfbox           = "org.apache.pdfbox"        % "pdfbox" % "2.0.22"
  val smile            = Seq("com.github.haifengl" %% "smile-scala" % "2.6.0")
  val featran          = Seq("com.spotify"         %% "featran-core" % featranV)

  val fs2 = Seq(
    "co.fs2" %% "fs2-core" % fs2Version,
    "co.fs2" %% "fs2-io" % fs2Version,
    "org.typelevel" %% "cats-effect" % catsEffectV
  )

  val scrimageCore = "com.sksamuel.scrimage"   %% "scrimage-core"    % scrimageVersion

  val scrimageAll = Seq(
    scrimageCore,
    "com.sksamuel.scrimage" %% "scrimage-io-extra" % scrimageVersion,
    "com.sksamuel.scrimage" %% "scrimage-filters"  % scrimageVersion
  )

  val circeJson = Seq(
    "io.circe" %% "circe-generic"  % circeJsonVersion,
    "io.circe" %% "circe-parser"   % circeJsonVersion,
    "io.circe" %% "circe-literal"  % circeJsonVersion
  )

  val http4s = Seq(
    "org.reactormonk" %% "cryptobits"           % "1.3",
    "org.http4s"      %% "http4s-dsl"           % http4sVersion,
    "org.http4s"      %% "http4s-blaze-server"  % http4sVersion,
    "org.http4s"      %% "http4s-blaze-client"  % http4sVersion,
    "org.http4s"      %% "http4s-circe"         % http4sVersion,
    "org.typelevel"   %% "cats-core"            % catsV
  )
}

object CommonLibs extends CommonLibs
