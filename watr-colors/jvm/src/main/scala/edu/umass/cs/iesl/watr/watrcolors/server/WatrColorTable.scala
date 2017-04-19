package edu.umass.cs.iesl.watr
package watrcolors
package server

import ammonite.ops._
import edu.umass.cs.iesl.watr.table._

object WatrColorTable {

  def main(args: Array[String]): Unit = {
    val dbname = args(0)

    val corpus = ShellCommands.initCorpus()
    val reflowDB = ShellCommands.initReflowDB(dbname)

    val server =  new EmbeddedServer(reflowDB, corpus, "localhost", 9999)

    server.httpserver.run()

    val predef = (
      s"""|${WatrTable.predef}
          |""".stripMargin)

    ammonite.Main(
      // storageBackend = new Storage.Folder(Defaults.ammoniteHome)
      predef = predef,
      defaultPredef = true,
      wd = pwd,
      welcomeBanner = Some(s""">> WatrTable+WatrColors @ http://localhost:9999/  <<"""),
      inputStream = System.in,
      outputStream  = System.out,
      errorStream = System.err,
      verboseOutput = false
    ).run(
      "server" -> server,
      "corpus" -> corpus,
      "db" -> reflowDB
    )

    server.httpserver.kill()
  }
}
