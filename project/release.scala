import sbt._
import sbt.Keys._

import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.ReleaseStateTransformations._
import com.typesafe.sbt.SbtNativePackager._
import com.typesafe.sbt.packager.Keys._

object Release {


  // val checkOrganization = ReleaseStep(action = st => {
  //   // extract the build state
  //   val extracted = Project.extract(st)
  //   // retrieve the value of the organization SettingKey
  //   val org = extracted.get(Keys.organization)

  //   if (org.startsWith("com.acme"))
  //     sys.error("Hey, no need to release a toy project!")

  //   st
  // })


  // Define task to  copy html files
  // val copyHtml = taskKey[Unit]("Copy html files from src/main/html to cross-version target directory")

  // Implement task
  // copyHtml := {
  //   import Path._

  //   val src = (Compile / sourceDirectory).value / "html"

  //   // get the files we want to copy
  //   val htmlFiles: Seq[File] = (src ** "*.html").get()

  //   // use Path.rebase to pair source files with target destination in crossTarget
  //   val pairs = htmlFiles pair rebase(src, (Compile / crossTarget).value)

  //   // Copy files to source files to target
  //   IO.copy(pairs, CopyOptions.apply(overwrite = true, preserveLastModified = true, preserveExecutable = false))
  // }

  // Ensure task is run before package
  // `package` := (`package` dependsOn copyHtml).value

  // val copyReleaseAssets = ReleaseStep(action = st => {
  //   import Path._

  //   val buildDir: File = baseDirectory.value / "textworks" / "target" / "universal"
  //   val rootBuildDir = baseDirectory.value / "build"

  //   // val tarFile: Seq[File] = (buildDir ** "*.tar.gz").get()
  //   val tarFile: Seq[File] = (buildDir ** "*.tar.gz").get()
  //   val pairs = tarFile pair rebase(tarFile, rootBuildDir)

  //   // Copy files to source files to target
  //   IO.copy(pairs, CopyOptions.apply(overwrite = true, preserveLastModified = true, preserveExecutable = false))

  //   st
  // })

  val settings = Seq(

    releaseProcess  := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      // stage binary artifacts
      releaseStepTask(stage in Universal),
      releaseStepTask(packageZipTarball in Universal),
      // copyReleaseAssets,
      // pushChanges
      // add binary asset textworks-app.tar.gz via ghr
      //



      // publishArtifacts,
      // setNextVersion,
      // commitNextVersion,
      // pushChanges
    )
  )



}
