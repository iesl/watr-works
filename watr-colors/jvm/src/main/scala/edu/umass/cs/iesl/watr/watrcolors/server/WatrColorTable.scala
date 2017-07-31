package edu.umass.cs.iesl.watr
package watrcolors
package server

import ammonite.ops._
import edu.umass.cs.iesl.watr.table._
import corpora._

object WatrColorTable extends App {

  def run(args: Array[String]): Unit = {
    val dbname = args(0)
    val passwd = args(1)
    val port = args(2)

    val portNum = port.toInt

    val corpus = ShellCommands.initCorpus()
    val reflowDB = ShellCommands.initReflowDB(dbname, passwd)
    val corpusAccessApi = CorpusAccessApi(reflowDB, corpus)

    val httpService =  new Http4sService(corpusAccessApi, "localhost", portNum)
    val httpServer = httpService.run()

    ammonite.Main(
      // storageBackend = new Storage.Folder(Defaults.ammoniteHome)
      predefCode = SharedInit.predef,
      defaultPredef = true,
      wd = pwd,
      welcomeBanner = Some(s""">> WatrTable+WatrColors @ http://localhost:portNum/  <<"""),
      inputStream = System.in,
      outputStream  = System.out,
      errorStream = System.err,
      verboseOutput = false,
      colors = SharedInit.replColors
    ).run(
      "corpusAccessApi" -> corpusAccessApi
    )

    httpServer.shutdownNow()
    reflowDB.shutdown()
    httpService.shutdown()
  }

  run(args)
}
