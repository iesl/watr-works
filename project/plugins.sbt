logLevel := Level.Warn

addSbtPlugin("com.github.gseitz"    % "sbt-release"         % "1.0.13")
addSbtPlugin("com.typesafe.sbt"     % "sbt-native-packager" % "1.7.2")
addSbtPlugin("com.47deg"            % "sbt-microsites"      % "0.7.4")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")

// addSbtPlugin("org.duhemm" % "sbt-errors-summary" % "0.6.0")
/// addSbtPlugin("org.bytedeco" % "sbt-javacv" % "1.17")

// `javacpp` are packaged with maven-plugin packaging, we need to make SBT aware that it should be added to class path.
classpathTypes += "maven-plugin"

// javacpp `Loader` is used to determine `platform` classifier in the project`s `build.sbt`
// We define dependency here (in folder `project`) since it is used by the build itself.
libraryDependencies += "org.bytedeco" % "javacpp" % "1.5.5"
// resolvers += Resolver.sonatypeRepo("snapshots")

