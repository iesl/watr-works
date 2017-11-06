package edu.umass.cs.iesl.watr
package watrcolors
package server

import ammonite.{ops => fs}

import ammonite.runtime.Storage
import ammonite.main.Defaults
import edu.umass.cs.iesl.watr.table._
import utils.{PathUtils => P}

object WatrColorTable extends App with utils.AppMainBasics {

  def run(args: Array[String]): Unit = {

    val corpusAccessApi = SharedInit.initCorpusAccessApi(args)

    val argMap = argsToMap(args.toArray)

    val port = argMap.get("port").flatMap(_.headOption)
      .getOrElse(sys.error("no port supplied (--port ...)"))

    val distRoot = argMap.get("dist").flatMap(_.headOption)
      .getOrElse(sys.error("no dist dir specified (--dist ...); "))

    val portNum = port.toInt

    val distDir = P.strToAmmPath(distRoot)

    val httpService =  new Http4sService(corpusAccessApi, "localhost", portNum, distDir)

    val httpServer = httpService.run()

    ammonite.Main(
      storageBackend = new Storage.Folder(Defaults.ammoniteHome),
      predefCode = SharedInit.predef,
      defaultPredef = true,
      wd = fs.pwd,
      welcomeBanner = Some(s""">> WatrTable+WatrColors @ http://localhost:${port}/  <<"""),
      inputStream = System.in,
      outputStream  = System.out,
      errorStream = System.err,
      verboseOutput = false,
      colors = SharedInit.replColors
    ).run(
      "corpusAccessApi" -> corpusAccessApi
    )

    corpusAccessApi.corpusAccessDB.shutdown()
    httpServer.shutdownNow()
    httpService.shutdown()
  }

  run(args)
}



















// object WatrColorServer extends StreamApp with utils.AppMainBasics {
//   override def stream(args: List[String]): Stream[Task, Nothing] = {

//     val argMap = argsToMap(args.toArray)
//     println(argMap)

//     val dbname = argMap.get("db").flatMap(_.headOption)
//       .getOrElse(sys.error("no db supplied (--db ...)"))

//     val passwd = argMap.get("passwd").flatMap(_.headOption)
//       .getOrElse(sys.error("no password supplied (--passwd ...)"))

//     val corpusRoot = argMap.get("corpus").flatMap(_.headOption)
//       .getOrElse(sys.error("no corpus path supplied (--corpus ...)"))

//     val port = argMap.get("port").flatMap(_.headOption)
//       .getOrElse(sys.error("no port supplied (--port ...)"))

//     val portNum = port.toInt

//     val corpus = Corpus(fs.pwd / corpusRoot)
//     val reflowDB = ShellCommands.initReflowDB(dbname, passwd)
//     val corpusAccessApi = CorpusAccessApi(reflowDB, corpus)

//     val httpService =  new Http4sService(corpusAccessApi, "localhost", portNum)

//     httpService.serve()

//   }
// }
