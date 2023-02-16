logLevel := Level.Warn

addSbtPlugin("com.github.sbt"    % "sbt-release"         % "1.1.0")
// addSbtPlugin("com.github.sbt"     % "sbt-native-packager" % "1.9.4")
// addSbtPlugin("com.47deg"            % "sbt-microsites"      % "1.3.4")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.10.4")

// `javacpp` are packaged with maven-plugin packaging, we need to make SBT aware that it should be added to class path.
classpathTypes += "maven-plugin"

// javacpp `Loader` is used to determine `platform` classifier in the project`s `build.sbt`
// We define dependency here (in folder `project`) since it is used by the build itself.
libraryDependencies += "org.bytedeco" % "javacpp" % "1.5.5"
// resolvers += Resolver.sonatypeRepo("snapshots")
