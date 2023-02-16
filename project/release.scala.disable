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
    publish / skip := true,
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
}




