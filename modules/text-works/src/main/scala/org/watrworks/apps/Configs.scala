package org.watrworks
package apps

import java.nio.{file => nio}
import scopt._
import scala.util.matching.Regex

object OptHelpers {
  implicit val NioPath: Read[nio.Path] =
    Read.reads { v =>
      nio.Paths.get(v).toAbsolutePath().normalize()
    }
}

object BuildMeta {
  val appName    = buildinfo.BuildInfo.appName
  val appVersion = buildinfo.BuildInfo.appVersion
  val commit     = buildinfo.BuildInfo.gitHeadCommit.getOrElse("?")
  val buildTime  = buildinfo.BuildInfo.builtAtMillis
  // val timezone   = buildinfo.BuildInfo.timezone

  val dtFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
  // dtFormat.setTimeZone(java.util.TimeZone.getTimeZone(timezone))
  val timeStr = dtFormat.format(buildTime)

  val buildStr = s"""| version    : ${appVersion}
                     | git commit : ${commit}
                     | build time : ${timeStr}
                     |""".stripMargin
}

object CorpusRoot {
  trait Like[R] {
    def corpusRoot: String
    def pathFilter: Option[String]
    def numToRun: Int
    def numToSkip: Int
    def withCorpusRoot(value: String): R
    def withPathFilter(value: String): R
    def withNumToRun(value: Int): R
    def withNumToSkip(value: Int): R
  }
  case class Conf(
    corpusRoot: Either[String, String] = Left(""),
    pathFilter: Option[String] = None,
    numToRun: Int = Int.MaxValue,
    numToSkip: Int = 0
  )

  def validateRegex(str: String): Either[String, Unit] = try {
    new Regex(str)
    Right((): Unit)
  } catch {
    case err: Error =>
      Left(err.getMessage())
  }

  def parseToCC: OParser[_, Conf] = {
    val builder = OParser.builder[Conf]
    import builder._
    OParser.sequence(
      opt[String]("corpus-root")
        .action((value, c) => c.copy(corpusRoot = Right(value))), // TODO make left/right for existing/valid corpus directory
      opt[String]("filter")
        .validate(validateRegex(_))
        .action((value, c) => c.copy(pathFilter = Some(value))),
      opt[Int]("num-to-run")
        .action((value, c) => c.copy(numToRun = value)),
      opt[Int]("num-to-skip")
        .action((value, c) => c.copy(numToSkip = value))
    )
  }

  def parser[R <: Like[R]]: OParser[_, R] = {
    val builder = OParser.builder[R]
    import builder._
    OParser.sequence(
      opt[String]("corpus-root")
        .action((value, c) => c.withCorpusRoot(value)),
      opt[String]("filter")
        .validate(validateRegex(_))
        .action((value, c) => c.withPathFilter(value)),
      opt[Int]("num-to-run")
        .action((value, c) => c.withNumToRun(value)),
      opt[Int]("num-to-skip")
        .action((value, c) => c.withNumToSkip(value))
    )
  }
}

// sealed trait IOMode

// object IOMode {
//   case class CorpusIO(
//     corpusRoot: nio.Path
//   )
//   case class FileIO(
//     filePath: nio.Path,
//     artifactRoot: nio.Path,
//     persistArtifacts: Boolean
//   )
// }

trait AppConfig[+R]

case object EmptyConf extends AppConfig[Nothing]

trait SubCommand[+R]

case object EmptyCommand extends SubCommand[Nothing]

object CorpusConf {
  trait Like[+R] extends AppConfig[R] {
    def withCorpusRoot(value: String): R
  }

  def option[R <: Like[R]]: OParser[String, R] = {
    val builder = OParser.builder[R]
    import builder._
    opt[String]("corpus-root")
      .action((s, c) => c.withCorpusRoot(s))
  }

  def parser[R <: Like[R]]: OParser[_, R] = {
    val builder = OParser.builder[R]
    import builder._
    OParser.sequence(
      opt[String]("corpus-root")
        .action((s, c) => c.withCorpusRoot(s))
    )
  }
}

// object InitCorpusCmd {
//   case class Conf(
//     corpusRoot: String = ""
//   ) extends AppConfig[Conf]
//     with CorpusConf.Like[Conf] {
//     def withCorpusRoot(value: String) = copy(corpusRoot = value)
//   }

//   // def parser: OParser[_, Conf] = {
//   def parser[C <: AppConfig[_]]: OParser[_, C] = {

//     val builder = OParser.builder[AppConfig[_]]
//     import builder._
//     val init: AppConfig[Conf] = Conf()

//     OParser.sequence(
//       cmd("init-corpus")
//         .action((v: Unit, conf) => init)
//         // .children(
//         //   CorpusConf.option
//         // )
//     )
//   }

// }

// object ExtractCmd {
//   case class Config(
//     pathFilter: Option[String] = None,
//     numToRun: Int = Int.MaxValue,
//     numToSkip: Int = 0
//   ) extends AppConfig[Config]
//     with PathFilterConf.Like[Config] {
//     def withPathFilter(value: String): Config =
//       copy(pathFilter = Some(value))
//     def withNumToRun(value: Int): Config =
//       copy(numToRun = value)
//     def withNumToSkip(value: Int): Config =
//       copy(numToSkip = value)
//   }
// }

object ConfigTestApp {
  OParser

  // val cmd1: OParser[Unit, InitCorpusCmd.Conf] = InitCorpusCmd.parser
  // val cmd2: OParser[_, AppConfig[_]]  = InitCorpusCmd.parser

  // val builder = OParser.builder[]
  // import builder._

  // val parser = OParser.sequence[Any, EmptyConf](
  //   programName("ConfigApp"),
  //   head("ConfigApp", "v0.1")
  //   // cmd1
  // )

  // def main(args: Array[String]) = {
  //   for {
  //     config <- OParser.parse(parser, args, EmptyConf)
  //   } {
  //     println(config)
  //   }
  // }
}

// object OptionParser {
//   import OptConversions._

//   case class Config(
//     mode: String = "",
//     pathFilter: Option[String] = None,
//     numToRun: Int = Int.MaxValue,
//     numToSkip: Int = 0
//   ) extends PathFilterConf.Like[Config] {

//     def withPathFilter(value: String): Config =
//       copy(pathFilter = Some(value))
//     def withNumToRun(value: Int): Config =
//       copy(numToRun = value)
//     def withNumToSkip(value: Int): Config =
//       copy(numToSkip = value)
//   }

//   val rootParser = {
//     val builder = OParser.builder[Config]
//     import builder._

//     opt[nio.Path]("corpus-root")

//     OParser.sequence(
//       programName("TextWorks"),
//       head("TextWorks"),
//       cmd("init-corpus")
//         .action((_, c) => c.copy(mode = "update"))
//         .text("update is a command.")
//         .children(
//           opt[Unit]("not-keepalive")
//             .abbr("nk")
//             // .action((_, c) => c.copy(keepalive = false))
//             .text("disable keepalive"),
//           checkConfig(c => success)
//         )
//     )
//   }

//   // val parser = OParser.sequence(
//   //   rootParser,
//   //   PathFilterConfig.parser
//   // )
//   def apply(args: Array[String]): Option[Config] = {
//     OParser.parse(rootParser, args, Config())
//   }

// }
