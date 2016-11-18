import sbt._
import sbt.Keys._
import com.typesafe.sbt.SbtAspectj._
import com.typesafe.sbt.SbtAspectj.AspectjKeys._


object AspectJ {

  lazy val settings = aspectjSettings ++ Seq(
    inputs in Aspectj <+= compiledClasses,

    binaries in Aspectj <<=  update map { report =>
      report.matching(moduleFilter(organization = "matryoshka"))
    },

    products in Compile <<= products in Aspectj,
    products in Runtime <<= products in Compile,
    fullClasspath in Test <<= useInstrumentedClasses(Test),
    fullClasspath in Runtime <<= useInstrumentedClasses(Runtime)
  )




  // lazy val settings = inConfig(Aspectj)(defaultAspectjSettings) ++ aspectjDependencySettings ++ Seq(
  //   aspectjVersion in Aspectj   := CommonLibs.aspectjVersion,
  //   // compileOnly in Aspectj      := true,
  //   // fork in Test                := true,
  //   javaOptions in Test       <++= weaverOptions in Aspectj,
  //   javaOptions in run        <++= weaverOptions in Aspectj,
  //   // lintProperties in Aspectj   += "invalidAbsoluteTypeName = ignore",
  //   // lintProperties in Aspectj   += "adviceDidNotMatch = ignore",

  //   // input compiled scala classes
  //   inputs in Aspectj <+= compiledClasses,

  //   // replace regular products with compiled aspects
  //   products in Compile <<= products in Aspectj,
  //   products in Runtime <<= products in Compile,
  //   products in Test <<= products in Runtime
  // )

  // def aspectjDependencySettings = Seq(
  //   ivyConfigurations += Aspectj,
  //   libraryDependencies <++= (aspectjVersion in Aspectj) { version => Seq(
  //     "org.aspectj" % "aspectjtools" % version % Aspectj.name,
  //     "org.aspectj" % "aspectjweaver" % version % Aspectj.name,
  //     "org.aspectj" % "aspectjrt" % version % Aspectj.name
  //   )}
  // )




  // ++ Seq(

  //     inputs in Aspectj <+= compiledClasses,

  //     inputs in Aspectj <<=  update map { report =>
  //       report.matching(moduleFilter(organization = "com.typesafe.akka", name = "akka-actor"))
  //     },

  //     // binaries in Aspectj <++= update map { report =>
//     //   report.matching(
//     //     moduleFilter(organization = "org.springframework", name = "spring-aspects")
//     //   )
//     // }

//     // aspectFilter in Aspectj := {
//     //   (jar, aspects) =>
//     //   {
//     //     if (jar.name.contains("akka-actor") )
//     //       aspects filter (jar => (jar.name.startsWith("Actor")))
//     //     else Seq.empty[File]
//     //   }
//     // },

//     products in Compile <<= products in Aspectj,
//     products in Runtime <<= products in Compile,
//     products in Test    <<= products in Compile,
//     fullClasspath in Test <<= useInstrumentedClasses(Test),
//     fullClasspath in Runtime <<= useInstrumentedClasses(Runtime)
//   )
}
