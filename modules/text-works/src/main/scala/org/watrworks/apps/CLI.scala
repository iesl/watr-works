package org.watrworks
package apps

import java.nio.file.{Path => JPath}
import zio._
import zio.cli._
import zio.cli.{HelpDoc => Help}

sealed trait Input extends Product with Serializable
object Input {
  final case class PathFilters(
    pathFilter: Option[String],
    numToRun: Int,
    numToSkip: Int,
    overwrite: Boolean
  )
}

sealed trait Subcommand extends Product with Serializable
object Subcommand {
  final case class ExtractFile() extends Subcommand

  final case class ExtractCorpus(
    corpusRoot: JPath,
    pathFilters: Input.PathFilters
  ) extends Subcommand

  final case class InitCorpus() extends Subcommand

  final case class ImageSeg() extends Subcommand
}

object CLI {
  def dir(name: String, exists: Exists = Exists.Either): Options[JPath] =
    Options.Single(name, Vector.empty, PrimType.Path(PathType.Directory, exists))

  val corpusRoot: Options[JPath] = Options
    .directory("corpus-root", Exists.Yes) ?? "Root Directory for input Corpus"

  val pathFilter: Options[Option[String]] = Options
    .text("filter")
    .optional("Leave empty to include all files") ?? "Regex to filter inputs"

  val numToRun: Options[Int] = Options
    .integer("num-to-run")
    .map(_.toInt)
    .withDefault(Int.MaxValue, "Number of inputs to process")

  val numToSkip: Options[Int] = Options
    .integer("num-to-skip")
    .map(_.toInt)
    .withDefault(0, "Number of inputs to skip")

  val overwrite = Options.boolean("overwrite", true)

  val pathFilters = (pathFilter ++ numToRun ++ numToSkip ++ overwrite)
    .as(Input.PathFilters)

  val extractCorpus = Command(
    "extract",
    (corpusRoot ++ pathFilters),
    Args.Empty,
    Help.p("Run Text Extraction over specified corpus")
  ).map { case ((croot, pf), _) =>
    Subcommand.ExtractCorpus(croot, pf)
  }

  val appCommands = extractCorpus

  def cliParse(
    args: List[String]
  ): Either[String, Subcommand] = {
    val maybeResult = Runtime.default.unsafeRun(
      appCommands.parse(args, CliConfig.default).either
    )

    println(s"running ${maybeResult} on ${args}")

    maybeResult match {
      case Left(validationError) =>
        val asText = validationError.error.toPlaintext()
        println(s"Error: ${asText}")
        // val synopsis = appCommands.synopsis.helpDoc.toPlaintext()
        val helpText = appCommands.helpDoc.toPlaintext()
        println(helpText)
        Left("Error")

      case Right(commandDirective) =>
        commandDirective match {
          case CommandDirective.UserDefined(leftovers @ _, cmd) =>
            Right(cmd)
          case _ =>
            Left("no command found")
        }

    }
  }
}

object App extends SharedConfig {
  val Pipe = ProcessPipelineSteps
  import Subcommand._

  def main(args: Array[String]) = {
    CLI.cliParse(args.to(List)) match {

      case Right(cmd) =>
        cmd match {
          case ExtractCorpus(corpusRoot, pathFilters) =>
          // Pipe.runTextExtractionPipeline

          case _ =>

        }
      case Left(err @ _) =>
    }
    // parser.parse(args, Config()).foreach { config =>
    //   config.exec.foreach { _.apply(config) }
    // }
  }

}
