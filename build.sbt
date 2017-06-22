import sbt.Keys._
import ReleaseTransformations._

SensibleThisBuild.settings
SensibleProject.settings

enablePlugins(ScalaJSPlugin, WorkbenchPlugin)

val Lib = CommonLibs

lazy val jsProjects = Seq[ProjectReference](
  watrmarksJS, watrcolorsJS
)

lazy val jvmProjects = Seq[ProjectReference](
  prelude, watrmarksJVM, watrshed, watrcolorsJVM
)

lazy val root = (project in file("."))
  .settings(Release.settings :_*)
  .aggregate( (jsProjects++jvmProjects): _*)

lazy val prelude = (project in file("watr-prelude"))
  .settings(SensibleProject.settings: _*)
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value
  ))

lazy val watrmarks = (crossProject in file("watr-marks"))
  .settings(SensibleProject.settings: _*)
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
      Lib.ammonite,
      Lib.playJson,
      // Needed by jsi
      "net.sf.trove4j" % "trove4j" % "3.0.3"
    ))

lazy val watrmarksJS = watrmarks.js

lazy val watrmarksJVM = watrmarks.jvm

lazy val watrshed = (project in file("watr-shed"))
  .enablePlugins(JavaAppPackaging)
  .settings(SensibleProject.settings: _*)
  .settings(libraryDependencies ++=
    LogLibs.logback ++
    DatabaseLibs.doobieDb ++
    TestLibs.testAndCheck ++
    Lib.fs2 ++ Seq(
      Lib.scrimageCore,
      Lib.ammonite,
      Lib.playJson,
      Lib.shapeless
    ))
  .dependsOn(prelude, watrmarksJVM)


lazy val watrcolors = (crossProject in file("watr-colors"))
  .settings(SensibleProject.settings: _*)
  .settings(skip in packageJSDependencies := false)
  .settings(libraryDependencies ++= Seq(
    Lib.scalaAsync,
    "com.lihaoyi"       %%% "scalatags"       % LibVersions.scalaTagsVersion,
    "com.lihaoyi"       %%% "scalarx"         % "0.3.2",
    "com.lihaoyi"       %%% "upickle"         % "0.4.4",
    "com.lihaoyi"       %%% "autowire"        % "0.2.6"
  ))
  .jsSettings(libraryDependencies ++= Seq(
    "fr.iscpif"         %%% "scaladget"       % LibVersions.scaladgetV,
    "org.singlespaced"  %%% "scalajs-d3"      % "0.3.4",
    "org.querki"        %%% "jquery-facade"   % "1.0",
    "org.scala-js"      %%% "scalajs-dom"     % "0.9.2"),
    scalacOptions -= "-Ywarn-dead-code" // doesn't play well with ScalaJS native binding declarations
  )
  .jvmSettings(libraryDependencies ++= Lib.http4s ++ TestLibs.testAndCheck ++ Seq(
    "com.typesafe.akka"  %% "akka-actor"                % "2.5.3",
    "org.webjars"        %  "bootstrap"                 % "3.3.7", // only used for css (bootstrap native is used instead)
    "org.webjars"        %  "jquery"                    % "2.2.4",
    "org.webjars"        %  "d3js"                      % "3.5.17",
    "org.webjars"        %  "mousetrap"                 % "1.6.0"
  ))

lazy val watrcolorsJS = watrcolors.js
  .dependsOn(watrmarksJS)

lazy val watrcolorsJVM = watrcolors.jvm
  .enablePlugins(JavaAppPackaging)
  .dependsOn(watrmarksJVM, watrshed)
  .settings(mainClass := Some("edu.umass.cs.iesl.watr.watrcolors.server.WatrColorTable"))
  .settings((resources in Compile) ++= Seq(
    (fastOptJS in (watrcolorsJS, Compile)).value.data,
    (artifactPath in (watrcolorsJS, Compile, fastOptJS)).value,
    ((classDirectory in (watrcolorsJS, Compile)).value / ".." / "watrcolors-fastopt.js.map").get.head
  ))





// import java.nio.file.Files
// import java.nio.file.StandardCopyOption._
// lazy val copyDocs = TaskKey[Unit]("copyDocs")
// lazy val watrdocs = scalatex.ScalatexReadme(
//   projectId = "watr-docs",
//   wd = file("watr-docs"),
//   url = "https://github.com/iesl/watr-works/tree/master",
//   source = "Readme")
//   .settings(SensibleProject.settings: _*)
//   .settings(copyDocs <<= (baseDirectory, target) map ({ (base, trg) =>
//     println("copying doc files..")
//       (trg / "scalatex").listFiles().foreach({file =>
//         val from = file.toPath
//         val to = base/".."/"docs"/file.getName()
//         println(s"copying files from ${from} to ${to}")
//         if (file.isDirectory) {
//           sbt.IO.copyDirectory(file, to, overwrite = true)
//         } else {
//           Files.copy(from, to.toPath, REPLACE_EXISTING)
//         }
//       })
//   }))
