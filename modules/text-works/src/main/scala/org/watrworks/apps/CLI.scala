package org.watrworks
package apps

import java.nio.file.{Files => JFiles, Path => JPath, Paths => JPaths}
import com.monovore.decline._
import cats.implicits._

object CLI {
  def parsePath(path: String): JPath = JPaths.get(path)
  def exists(path: JPath)            = JFiles.exists(path)
  def isDirectory(path: JPath)       = JFiles.isDirectory(path)
  def isRegularFile(path: JPath)     = JFiles.isRegularFile(path)

  def existingDirectory(name: String, help: String): Opts[JPath] = {
    Opts
      .option[JPath](name, help)
      .map(p => JPaths.get("").resolve(p).toAbsolutePath())
      .validate("Directory must exist")(p => isDirectory(p))
  }

  def validCorpusRoot(name: String): Opts[JPath] = {
    existingDirectory(name, "Corpus root directory")
      .validate("Directory must have a valid corpus structure")(p => true) // TODO check validity
  }

  val pathFilter: Opts[Option[String]] = Opts
    .option[String]("filter", "(optional) regex to filter inputs")
    .orNone

  val numToRun: Opts[Int] = Opts
    .option[Int]("num-to-run", "Number of document to process. Default to all.")
    .withDefault(Int.MaxValue)

  val numToSkip: Opts[Int] = Opts
    .option[Int]("num-to-skip", "Number of document to process. Default to 0")
    .withDefault(0)

  val overwriteFlag = Opts.flag("overwrite", help = "Overwrite existing outputs. Default to false").orFalse

  val pathFilters = (pathFilter, numToRun, numToSkip, overwriteFlag).mapN {
    InputConfig.PathFilters
  }

  val corpusRoot     = validCorpusRoot("corpus-root")
  val filteredCorpus = (corpusRoot, pathFilters).mapN(InputConfig.FilteredCorpus)

  val extract: Command[Action.ExtractCorpus] = Command(
    name = "extract",
    header = "Run Text Extraction over specified corpus"
  ) {
    filteredCorpus.map(Action.ExtractCorpus)
  }

  val allCommands = Command(
    name = "App",
    header = "TextWorks PDF text extraction, part of WatrWorks"
  ) {
    Opts.subcommands(extract)
  }

}

object App {
  val Pipe = Pipelines

  def main(args: Array[String]) = {
    CLI.allCommands.parse(args.to(List)) match {
      case Left(help) =>
        println(help.toString())

      case Right(cmd) =>
        println(s"Success! ${cmd}")

        cmd match {
          case action: Action.ExtractCorpus =>
            Pipe.runTextExtractionPipeline(action)

          case _ =>
        }
    }
  }

}
