package edu.umass.cs.iesl.watr
package watrcolors
package server

import ammonite.ops._
import edu.umass.cs.iesl.watr.table._
import corpora._

object WatrColorTable extends App {

  def run(args: Array[String]): Unit = {
    val dbname = args(0)

    val corpus = ShellCommands.initCorpus()
    val reflowDB = ShellCommands.initReflowDB(dbname)
    val corpusAccessApi = CorpusAccessApi(reflowDB, corpus)

    val httpService =  new Http4sService(corpusAccessApi, "localhost", 9999)
    val httpServer = httpService.run()

    ammonite.Main(
      // storageBackend = new Storage.Folder(Defaults.ammoniteHome)
      predefCode = SharedInit.predef,
      defaultPredef = true,
      wd = pwd,
      welcomeBanner = Some(s""">> WatrTable+WatrColors @ http://localhost:9999/  <<"""),
      inputStream = System.in,
      outputStream  = System.out,
      errorStream = System.err,
      verboseOutput = false,
      colors = SharedInit.replColors
    ).run(
      "corpusAccessApi" -> corpusAccessApi
    )

    // server.httpserver.kill()
    httpServer.shutdownNow()
    reflowDB.shutdown()
    httpService.shutdown()
  }

  run(args)
}
