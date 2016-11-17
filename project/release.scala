import sbt._
import sbt.Keys._

import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.ReleaseStateTransformations._

object Release {

  val settings = Seq(

    releaseProcess  := Seq[ReleaseStep](
      // checkSnapshotDependencies,            // : ReleaseStep
      inquireVersions,                        // : ReleaseStep
                                              // runTest,                             // : ReleaseStep
      setReleaseVersion,                      // : ReleaseStep
      commitReleaseVersion,                   // : ReleaseStep, performs the initial git checks
      tagRelease,                             // : ReleaseStep
                                              // publishArtifacts,                    // : ReleaseStep, checks whether `publishTo` is properly set up
      setNextVersion,                         // : ReleaseStep
      commitNextVersion,                      // : ReleaseStep
      pushChanges                             // : ReleaseStep, also checks that an upstream branch is properly configured
    )
  )



}
