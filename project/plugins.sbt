resolvers += "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public"

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.11")

addSbtPlugin("com.lihaoyi" % "workbench" % "0.2.3")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.8.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")

addSbtPlugin("com.github.fedragon" % "sbt-todolist" % "0.6")

// addSbtPlugin("com.zavakid.sbt" % "sbt-one-log" % "1.0.1")
// addSbtPlugin("com.sksamuel.scapegoat" %% "sbt-scapegoat" % "1.0.4")
// libraryDependencies <+= sbtVersion("org.scala-sbt" % "scripted-plugin" % _)
// Freek

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.3")
