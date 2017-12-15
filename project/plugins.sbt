resolvers += "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

logLevel := Level.Warn

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.21")
addSbtPlugin("com.github.gseitz"    % "sbt-release"         % "1.0.6")
addSbtPlugin("com.typesafe.sbt"     % "sbt-native-packager" % "1.3.1")
addSbtPlugin("com.47deg"            % "sbt-microsites"      % "0.7.4")

// addSbtPlugin("com.lihaoyi"          % "workbench"           % "0.3.0")
// addSbtPlugin("com.lihaoyi"          % "scalatex-sbt-plugin" % "0.3.7")
// addSbtPlugin("com.github.tkawachi"  % "sbt-doctest"         % "0.6.0")
// addSbtPlugin("org.wartremover"      % "sbt-wartremover" % "2.1.0")
// addSbtPlugin("com.github.fedragon"  % "sbt-todolist"        % "0.6")
// addSbtPlugin("org.scoverage"        % "sbt-scoverage"  % "1.5.0")
// addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.0")




lazy val jsProjects = Seq[ProjectReference](
  watrmarksJS // , watrcolorsJS
)

lazy val jvmProjects = Seq[ProjectReference](
  prelude, watrmarksJVM, textworks // , watrshed, watrcolorsJVM
)
lazy val root = (project in file("."))
  .enablePlugins(ScalaJSPlugin)
  .aggregate( (jsProjects ++ jvmProjects): _*)

lazy val prelude = (project in file("watr-prelude"))
  .settings(SensibleProject.settings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value
  ))

lazy val watrmarks = (crossProject in file("watr-marks"))
  .settings(SensibleProject.settings: _*)
  .settings(Release.settings :_*)
  .settings(libraryDependencies ++= Seq(
    "org.scalaz"                 %%% "scalaz-core"            % Lib.scalazVersion,
    "com.chuusai"                %%% "shapeless"              % "2.3.2",
    "com.lihaoyi"                %%% "scalatags"              % Lib.scalaTagsVersion,
    "com.lihaoyi"                %%% "fansi"                  % Lib.fansiV,
    "com.lihaoyi"                %%% "sourcecode"             % Lib.sourcecodeV,
    "com.slamdata"               %%% "matryoshka-core"        % Lib.matryoshkaCoreV,
    "com.slamdata"               %%% "matryoshka-scalacheck"  % Lib.matryoshkaCoreV % "compile, test"
  ))
  .jvmSettings(libraryDependencies ++=
    LogLibs.logback ++
    TestLibs.testAndCheck ++ Seq(
      Lib.ammoniteOps,
      Lib.playJson,
      "com.github.davidmoten" % "rtree" % "0.8.0.2",
      "com.github.davidmoten" % "flatbuffers-java" % "1.7.0.1",
      "ichi.bench" % "thyme" % "0.1.1" from "http://plastic-idolatry.com/jars/thyme-0.1.1.jar"
    ))

lazy val watrmarksJS = watrmarks.js

lazy val watrmarksJVM = watrmarks.jvm
