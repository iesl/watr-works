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
    val versionFile: File = baseDir / "last-release-version.txt"

    val ver = s"v$version"
    IO.write(versionFile, version)

    st
  })


  // .settings(mappings in Universal in (Compile, packageDoc) := Seq())
  val steps = Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      copyVersionToFile,
      setNextVersion,
      commitNextVersion,
      // pushChanges,
      // pushChanges
  )
      // releaseStepTask(Universal / PKeys.packageZipTarball),

  val releaseStep: ReleaseStep = releaseStepTask(Universal / PKeys.packageZipTarball)


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
  ))

  val pkgZipSettings = Seq(
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      releaseStepTask(Universal / PKeys.packageZipTarball),
      copyVersionToFile,
      setNextVersion,
      commitNextVersion,
      // pushChanges,
      // pushChanges
    ))


}
