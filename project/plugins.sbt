resolvers += "IESL Public Releases" at "https://dev-iesl.cs.umass.edu/nexus/content/groups/public"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

logLevel := Level.Warn

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.21")
addSbtPlugin("com.github.gseitz"    % "sbt-release"         % "1.0.6")
addSbtPlugin("com.typesafe.sbt"     % "sbt-native-packager" % "1.3.1")
addSbtPlugin("com.47deg"            % "sbt-microsites"      % "0.7.4")

// addSbtPlugin("com.lihaoyi"          % "workbench"           % "0.3.0")
// addSbtPlugin("com.lihaoyi"          % "scalatex-sbt-plugin" % "0.3.7")
// addSbtPlugin("com.github.tkawachi"  % "sbt-doctest"         % "0.6.0")
// addSbtPlugin("org.wartremover"      % "sbt-wartremover" % "2.1.0")
// addSbtPlugin("com.github.fedragon"  % "sbt-todolist"        % "0.6")
// addSbtPlugin("org.scoverage"        % "sbt-scoverage"  % "1.5.0")
// addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.0")


