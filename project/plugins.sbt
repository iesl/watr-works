// resolvers += "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public"

logLevel := Level.Warn

addSbtPlugin("org.portable-scala"   % "sbt-scalajs-crossproject"      % "0.6.0")
addSbtPlugin("org.portable-scala"   % "sbt-crossproject"      % "0.6.0")

addSbtPlugin("org.scala-js"         % "sbt-scalajs"         % "0.6.31")
addSbtPlugin("com.github.gseitz"    % "sbt-release"         % "1.0.12")
addSbtPlugin("com.typesafe.sbt"     % "sbt-native-packager" % "1.3.2")
addSbtPlugin("com.47deg"            % "sbt-microsites"      % "0.7.4")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")
addSbtPlugin("org.duhemm" % "sbt-errors-summary" % "0.6.0")
