// resolvers += "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public"

logLevel := Level.Warn

addSbtPlugin("org.portable-scala"   % "sbt-scalajs-crossproject"      % "0.6.0")
addSbtPlugin("org.portable-scala"   % "sbt-crossproject"      % "0.6.0")
// addSbtPlugin("org.portable-scala" % "sbt-crossproject"         % "0.3.1")
// addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.3.1")

addSbtPlugin("org.scala-js"         % "sbt-scalajs"         % "0.6.25")
addSbtPlugin("com.github.gseitz"    % "sbt-release"         % "1.0.7")
addSbtPlugin("com.typesafe.sbt"     % "sbt-native-packager" % "1.3.2")
addSbtPlugin("com.47deg"            % "sbt-microsites"      % "0.7.4")
// addSbtPlugin("org.wartremover"      % "sbt-wartremover"     % "2.3.5")

addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.0.0-M10")
