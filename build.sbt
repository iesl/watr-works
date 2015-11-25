organization in ThisBuild := "edu.umass.cs.iesl"

name in ThisBuild := "watr-works"

version in ThisBuild := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.7"

shellPrompt in ThisBuild := { s: State =>
  val c = scala.Console
  val blue = c.RESET + c.BLUE + c.BOLD
  val white = c.RESET + c.BOLD
  val projectName = Project.extract(s).currentProject.id

  "[" + blue + projectName + white + "]>> " + c.RESET
}

scalacOptions in ThisBuild ++= Seq(
  "-target:jvm-1.7",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard"
)

libraryDependencies in ThisBuild ++= Seq(
  "org.jdom" % "jdom2" % "2.0.6",
  "org.scalatest" % "scalatest_2.11" % "2.2.5" % "test"
)


resolvers in ThisBuild += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

val imports = """import ammonite.ops._
pl.edu.icm.cermine
cermine.ExtractionUtils
cermine.{ComponentConfiguration => Conf}
ammonite.ops.ImplicitWd._
edu.umass.cs.iesl.watr
edu.umass.cs.iesl.watr._
edu.umass.cs.iesl.watr.shell._
edu.umass.cs.iesl.watr.shell.ops._""".split("\n").mkString(",")


lazy val works = (project in file("."))
  .settings(libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.1.5",
    "pl.edu.icm.cermine" % "cermine-impl" % "1.8-SNAPSHOT",
    "com.lihaoyi" %% "ammonite-ops" % "0.5.0",
    "com.lihaoyi" % "ammonite-repl" % "0.5.0" cross CrossVersion.full,
    "com.lihaoyi" %% "scalatags" % "0.5.3",
    "org.scalatest" % "scalatest_2.11" % "2.2.5" % "test"
  ))
  .settings(initialCommands := s""" ammonite.repl.Repl.run("${imports}") """)
  .dependsOn(markup)
  .aggregate(markup)

lazy val markup = (project in file("watr-markup"))
