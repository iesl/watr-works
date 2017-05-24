import sbt._
import Keys._


trait LibVersions {
  val scalazVersion       = "7.2.12"
  val scalaTagsVersion    = "0.6.5"
  val scalaAsyncVersion   = "0.9.6"
  val scalaModulesVersion = "1.0.4"
  val akkaVersion         = "2.4.17"
  val streamsVersion      = "1.0"
  val scalatestVersion    = "3.0.3"
  val logbackVersion      = "1.7.25"
  val quasiquotesVersion  = "2.0.1"
  val guavaVersion        = "18.0"
  val specs2Version       = "3.7"
  val scrimageVersion     = "2.1.8"
  val monocleVersion      = "1.4.0"
  val aspectjVersion      = "1.8.9"
  val acyclicVersion      = "0.1.7"
  val doobieVersion       = "0.4.1"
  val matryoshkaCoreV     = "0.19.0"
  val sourcecodeV         = "0.1.3"
  val fansiV              = "0.2.4"
  val shapelessV          = "2.3.2"
  val scaladgetV          = "0.9.4"
  val http4sVersion       = "0.16.0a-M2"
  val fs2Version          = "0.9.6"


}


object LibVersions extends LibVersions

object TestLibs extends LibVersions {
  val scalatest = Seq(
    "org.scalatest" %% "scalatest" % scalatestVersion % "test"
  )

  val scalacheck = Seq(
    "org.scalaz"     %% "scalaz-scalacheck-binding" % scalazVersion  % "test",
    "org.scalacheck" %% "scalacheck"                % "1.14.0"       % "test" //  force()
  )

  val testAndCheck = scalatest // ++ scalacheck
}

object LogLibs extends LibVersions {
  val logback = Seq(
    "org.log4s"      %% "log4s"            % "1.3.4",
    "ch.qos.logback"  % "logback-classic"  % "1.2.3",
    "org.slf4j"       % "slf4j-api"        % logbackVersion,
    "org.slf4j"       % "jul-to-slf4j"     % logbackVersion,
    "org.slf4j"       % "jcl-over-slf4j"   % logbackVersion
  )
}

object DatabaseLibs extends LibVersions {

  val doobieDb = Seq(
    "org.tpolecat" %% "doobie-core"       % doobieVersion,
    "org.tpolecat" %% "doobie-postgres"   % doobieVersion,
    "org.tpolecat" %% "doobie-hikari"     % doobieVersion,
    "org.tpolecat" %% "doobie-specs2"     % doobieVersion,
    "org.postgresql"          % "postgresql" % "42.1.1",
    "org.javassist" % "javassist" % "3.21.0-GA",
    "com.impossibl.pgjdbc-ng" % "pgjdbc-ng"  % "0.7"
  )

}

trait CommonLibs extends LibVersions {

  val scalazCore       = "org.scalaz"              %% "scalaz-core"      % scalazVersion
  val scalaAsync       = "org.scala-lang.modules"  %% "scala-async"      % scalaAsyncVersion
  val scalatags        = "com.lihaoyi"             %% "scalatags"        % scalaTagsVersion
  val ammonite         = "com.lihaoyi"             % "ammonite_2.11.11"  % "0.9.1"
  val fastparse        = "com.lihaoyi"             %% "fastparse"        % "0.4.2"
  val sourcecode       = "com.lihaoyi"             %% "sourcecode"       % sourcecodeV
  val playJson         = "com.typesafe.play"       %% "play-json"        % "2.5.15"
  val scopt            = "com.github.scopt"        %% "scopt"            % "3.5.0"
  val machinist        = "org.typelevel"           %% "machinist"        % "0.6.1"
  val shapeless        = "com.chuusai"             %% "shapeless"        % shapelessV
  val aspectJ          = "org.aspectj"              % "aspectjweaver"    % aspectjVersion
  val acyclic          = "com.lihaoyi"             %% "acyclic"          % acyclicVersion % "provided"
  val matryoshkaCore   = "com.slamdata"            %% "matryoshka-core"  % matryoshkaCoreV
  val scrimageCore     = "com.sksamuel.scrimage"   %% "scrimage-core"    % scrimageVersion

  val fs2 = Seq(
    "co.fs2" %% "fs2-core" % fs2Version,
    "co.fs2" %% "fs2-io" % fs2Version
  )

  val scrimage = Seq(
    scrimageCore,
    "com.sksamuel.scrimage" %% "scrimage-io-extra" % scrimageVersion,
    "com.sksamuel.scrimage" %% "scrimage-filters"  % scrimageVersion
  )

  val http4s = Seq(
    // "org.http4s" %% "http4s-circe" % http4sVersion,
    "org.reactormonk" %% "cryptobits" % "1.1",
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion
  )
}

object CommonLibs extends CommonLibs
