resolvers += "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

logLevel := Level.Warn

addSbtPlugin("org.scala-js"         % "sbt-scalajs"         % "0.6.16")
addSbtPlugin("com.lihaoyi"          % "workbench"           % "0.3.0")
addSbtPlugin("com.lihaoyi"          % "scalatex-sbt-plugin" % "0.3.7")
addSbtPlugin("me.lessis"            % "bintray-sbt"         % "0.3.0")
addSbtPlugin("com.github.gseitz"    % "sbt-release"         % "1.0.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.0-M8")

// addSbtPlugin("com.github.fedragon"  % "sbt-todolist"        % "0.6")
// addSbtPlugin("com.typesafe.sbt" % "sbt-aspectj" % "0.10.6")
// addSbtPlugin("org.scoverage"        % "sbt-scoverage"  % "1.5.0")
// addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.0")

