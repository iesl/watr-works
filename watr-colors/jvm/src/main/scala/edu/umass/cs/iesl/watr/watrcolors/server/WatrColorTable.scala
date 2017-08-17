package edu.umass.cs.iesl.watr
package watrcolors
package server

import ammonite.ops._
import edu.umass.cs.iesl.watr.table._
import corpora._
import corpora.filesys.Corpus

object WatrColorTable extends App with utils.AppMainBasics {

  def run(args: Array[String]): Unit = {
    val argMap = argsToMap(args)
    println(argMap)

    val dbname = argMap.get("db").flatMap(_.headOption)
      .getOrElse(sys.error("no db supplied (--db ...)"))

    val passwd = argMap.get("passwd").flatMap(_.headOption)
      .getOrElse(sys.error("no password supplied (--passwd ...)"))

    val corpusRoot = argMap.get("corpus").flatMap(_.headOption)
      .getOrElse(sys.error("no corpus path supplied (--corpus ...)"))

    val port = argMap.get("port").flatMap(_.headOption)
      .getOrElse(sys.error("no port supplied (--port ...)"))

    val portNum = port.toInt

    val corpus = Corpus(pwd / corpusRoot)
    val reflowDB = ShellCommands.initReflowDB(dbname, passwd)
    val corpusAccessApi = CorpusAccessApi(reflowDB, corpus)

    val httpService =  new Http4sService(corpusAccessApi, "localhost", portNum)
    val httpServer = httpService.run()

    ammonite.Main(
      // storageBackend = new Storage.Folder(Defaults.ammoniteHome)
      predefCode = SharedInit.predef,
      defaultPredef = true,
      wd = pwd,
      welcomeBanner = Some(s""">> WatrTable+WatrColors @ http://localhost:${port}/  <<"""),
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
