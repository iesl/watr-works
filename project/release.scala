import sbt._
// import sbt.Keys._

import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.ReleaseStateTransformations._
import com.typesafe.sbt.SbtNativePackager._
import com.typesafe.sbt.packager.{Keys => PKeys}
import PKeys._

object Release {


  val copyVersionToFile = ReleaseStep(action = st => {
    val extracted = Project.extract(st)
    val version = extracted.get(Keys.version)
    val baseDir = extracted.get(Keys.baseDirectory)
    val versionFile: File = baseDir / "set-release-versions"

    val ver = s"""|VERSION_STR=v$version
                  |VERSION_NUM=$version
                  |"""
    IO.write(versionFile, version)

    st
  })

  val pkgTgzStep: ReleaseStep = releaseStepTask(Universal / PKeys.packageZipTarball)

  val settings = Seq(
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      copyVersionToFile,
      setNextVersion,
      commitNextVersion,
      pushChanges,
  ))


}
