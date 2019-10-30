import sbt._
import sbt.Keys._

import scala.sys.process._

import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.ReleaseStateTransformations._
import com.typesafe.sbt.SbtNativePackager._
import com.typesafe.sbt.packager.{Keys => PKeys}
import PKeys._

object Release {

  lazy val textworks = (project in file("text-works"))

  val uploadAssetsStep = ReleaseStep(action = st => {
    val extracted = Project.extract(st)
    val version = extracted.get(Keys.version)
    val baseDir = extracted.get(Keys.baseDirectory)
    val shell: Seq[String] = if (sys.props("os.name").contains("Windows")) Seq("cmd", "/c") else Seq("bash", "-c")
    val uploadAssets: Seq[String] = shell :+ s"ghr v${version} text-works/target/universal/textworks-${version}.tgz"
    println(s"uploading assets to release version v${version}")
    if((uploadAssets !) == 0) {
      println("upload successful!")
    } else {
      throw new IllegalStateException("upload failed!")
    }

    st
  })


  val pkgTgzStep: ReleaseStep = releaseStepTask(textworks / Universal / PKeys.packageZipTarball)

  val settings = Seq(
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      pushChanges,
      pkgTgzStep,
      uploadAssetsStep,
      setNextVersion,
      commitNextVersion,
      pushChanges,
  ))

      // copyVersionToFile,

}




  // lazy val uploadAssetsTask = taskKey[Unit]("Upload binary assets to github")

  // uploadAssetsTask := {
  //   val s: TaskStreams = streams.value
  //   val shell: Seq[String] = if (sys.props("os.name").contains("Windows")) Seq("cmd", "/c") else Seq("bash", "-c")
  //   // val npmInstall: Seq[String] = shell :+ "npm install"
  //   val ver = version.value
  //   val uploadAssets: Seq[String] = shell :+ s"echo ghr v${ver} text-works/target/universal/textworks-${ver}.tgz"
  //   s.log.info(s"uploading assets to release version v${ver}")
  //   if((uploadAssets !) == 0) {
  //     s.log.success("upload successful!")
  //   } else {
  //     throw new IllegalStateException("upload failed!")
  //   }
  // }

  // val copyVersionToFile = ReleaseStep(action = st => {
  //   val extracted = Project.extract(st)
  //   val version = extracted.get(Keys.version)
  //   val baseDir = extracted.get(Keys.baseDirectory)
  //   val versionFile: File = baseDir / "set-release-versions"

  //   val ver = s"""|VERSION_STR=v$version
  //                 |VERSION_NUM=$version
  //                 |""".stripMargin

  //   IO.write(versionFile, ver)

  //   st
  // })
