package org.watrworks
package table

import cats.effect._
import ammonite.{ops => fs}
// import watrcolors.WatrTableServer

object SharedInit extends {

  val predef =
    s"""|import org.watrworks._
        |import corpora._
        |import corpora.filesys._
        |import watrmarks.{StandardLabels => LB}
        |import TypeTags._
        |import table._
        |import ShellCommands._
        |""".stripMargin

  val welcomeBanner = s""">> WatrTable Shell <<"""

  val replColors = ammonite.util.Colors(
    prompt = fansi.Color.Magenta,
    ident = fansi.Color.Cyan,
    `type` = fansi.Color.Green,
    literal = fansi.Color.Green,
    prefix = fansi.Color.Yellow,
    comment = fansi.Color.LightGreen,
    keyword = fansi.Color.Yellow,
    selected = fansi.Reversed.On,
    error = fansi.Color.Red,
    warning = fansi.Color.Yellow,
    info = fansi.Color.LightGray
  )
}

object WatrTable extends IOApp {
  import SharedInit._

  override def run(args: List[String]): IO[ExitCode] = {

    // val fiber = WatrTableServer
    //   .resource[IO]
    //   .use(_ => IO.never)
    //   .as(ExitCode.Success)
    //   .start
    //   .unsafeRunSync()

    // replMain().run()

    // fiber.cancel.unsafeRunSync()
    IO(ExitCode.Success)
  }

  def replMain() = ammonite.Main(
    //   storageBackend = new Storage.Folder(Defaults.ammoniteHome),
    predefCode = predef,
    defaultPredef = true,
    wd = fs.pwd,
    welcomeBanner = Some(SharedInit.welcomeBanner),
    inputStream = System.in,
    outputStream = System.out,
    errorStream = System.err,
    // verboseOutput = true,
    colors = replColors
  )
}
