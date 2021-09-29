package org.watrworks
package apps

import java.nio.file.{Path => JPath}
import zio.cli.{Args, Command, Exists, HelpDoc, Options, CliConfig, CliApp}
import zio.cli.HelpDoc.Span.text
// import zio._
import zio.console.{
  // Console,
  putStrLn
}
import zio.{
  // ExitCode,
  // ZIO,
  Runtime
}
import zio.cli.CommandDirective

sealed trait Subcommand extends Product with Serializable
object Subcommand {
  final case class Add(modified: Boolean, directory: JPath) extends Subcommand
  final case class Remote(verbose: Boolean)                 extends Subcommand
  ///
  final case class ExtractFile() extends Subcommand
  final case class ExtractCorpus(
    corpusRoot: String,
    pathFilter: Option[String],
    numToRun: Int,
    numToSkip: Int
  )                             extends Subcommand
  final case class InitCorpus() extends Subcommand
  final case class ImageSeg()   extends Subcommand
}

class CLITest extends utils.SimpleTest {
  behavior of "Command-line arg parsers"

  val modifiedFlag: Options[Boolean] = Options.boolean("m")

  val addHelp: HelpDoc = HelpDoc.p("Add subcommand description")
  val add =
    Command("add", modifiedFlag, Args.directory("directory", Exists.No), addHelp).map { case (modified, directory) =>
      Subcommand.Add(modified, directory)
    }

  val verboseFlag: Options[Boolean] = Options.boolean("verbose").alias("v")
  val configPath: Options[JPath]    = Options.directory("c", Exists.Yes)

  val remoteHelp: HelpDoc = HelpDoc.p("Remote subcommand description")
  val remote = Command("remote", verboseFlag, Args.none, remoteHelp).map { case (verbose, _) =>
    Subcommand.Remote(verbose)
  }

  val git: Command[Subcommand] =
    Command("git", Options.none, Args.none).subcommands(remote | add).map(_._2)

  it should "smokescreen" in {
    val examples = List(
      // List("git", "remote", "-v"),
      List("git", "add", "-x", "my/dir")
    )
    for {
      args: List[String] <- examples
    } yield {

      val maybeResult = Runtime.default.unsafeRun(
        git.parse(args, CliConfig.default).either
      )

      println(s"running ${maybeResult} on ${args}")

      val prog = maybeResult match {
        case Left(validationError) =>
          val asText = validationError.error.toPlaintext()
          println(s"Error: ${asText}")
          val synopsis = git.synopsis.helpDoc.toPlaintext()
          println(s"Help ${synopsis}")
          "Error"

        case Right(cmd) =>
          println(s"success: ${cmd}")

          "Success"
      }
      //   val result = Runtime.default
      //     .withReportFailure(cause => println(s"error ${cause}"))
      //     .withFatal(error => { println(s"fatal ${error}"); true })
      //     .unsafeRun(uio)
    }
  }
}
