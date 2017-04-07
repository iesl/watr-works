
scalacOptions in (ensimeCompileOnly) ++= Seq(
  "-Xshow-phases"
    , "-Xlog-reflective-calls"
)


// JVM flag to enable remote debugging of forked tasks.
// ensimeDebuggingFlag in ThisBuild :=  "" // settingKey[String]

// Port for remote debugging of forked tasks.
// ensimeDebuggingPort in ThisBuild :=  9090 // settingKey[Int]

// Compiles a single scala file
// ensimeCompileOnly in ThisBuild :=  // inputKey[Unit]

// Run user specified env/args/class/params e.g. `ensimeRunMain FOOin ThisBuild :BAR -Xmx2g foo.Bar baz'
//    ensimeRunMain

// Run user specified env/args/class/params with debugging flags added
//    ensimeRunDebug

// Named applications with canned env/args/class/params
// ensimeLaunchConfigurations in ThisBuild :=  // settingKey[Seq[LaunchConfig]]

// Launch a named application in ensimeLaunchConfigurations
//    ensimeLaunch

// Formats a single scala file
//    ensimeScalariformOnly
