package edu.umass.cs.iesl.watr
package table

// import ammonite.util._
import ammonite.ops._
import ammonite.runtime.Storage
import ammonite.main.Defaults
import corpora._
import corpora.filesys.Corpus
import utils.{PathUtils => P}

object SharedInit extends utils.AppMainBasics {

  val predef =
    s"""|import edu.umass.cs.iesl.watr
        |import watr._
        |import corpora._
        |import workflow._
        |import corpora.filesys._
        |import corpora.database._
        |import watrmarks.{StandardLabels => LB}
        |import TypeTags._
        |import watr._, rtrees._, geometry._, table._
        |import InitialSegmentationCommands._
        |import ammonite.util._
        |implicit val corpusAccessApi0: CorpusAccessApi = corpusAccessApi
        |implicit val docStore: DocumentZoningApi = corpusAccessApi.docStore
        |implicit val corpusDb: CorpusAccessDB = corpusAccessApi.corpusAccessDB
        |ammonite.repl.ReplBridge.value0.pprinter.update(SharedInit.pprinter)
        |""".stripMargin


  val welcomeBanner = s""">> WatrTable Shell <<"""


  val replColors = ammonite.util.Colors(
    prompt   = fansi.Color.Magenta,
    ident    = fansi.Color.Cyan,
    `type`   = fansi.Color.Green,
    literal  = fansi.Color.Green,
    prefix   = fansi.Color.Yellow,
    comment  = fansi.Color.LightGreen,
    keyword  = fansi.Color.Yellow,
    selected = fansi.Reversed.On,
    error    = fansi.Color.Red,
    warning  = fansi.Color.Yellow,
    info     = fansi.Color.LightGray
  )

  val pprinter: pprint.PPrinter = pprint.PPrinter.Color.copy(
    additionalHandlers = ShellPrettyPrinters.additionalHandlers
  )

  def initCorpusAccessApi(args: Array[String]): CorpusAccessApi = {
    val argMap = argsToMap(args)
    val dbname = argMap.get("db").flatMap(_.headOption)
      .getOrElse(sys.error("no db supplied (--db ...)"))

    val passwd = argMap.get("passwd").flatMap(_.headOption)
      .getOrElse(sys.error("no password supplied (--passwd ...)"))

    val corpusRoot = argMap.get("corpus").flatMap(_.headOption)
      .getOrElse(sys.error("no corpus path supplied (--corpus ...)"))

    val corpus = Corpus(P.strToAmmPath(corpusRoot))

    val corpusAccessDB = InitialSegmentationCommands.initReflowDB(dbname, passwd)

    CorpusAccessApi(corpusAccessDB, corpus)
  }
}

object WatrTable extends App with utils.AppMainBasics {
  import SharedInit._

  def run(args: Array[String]): Unit = {
    val corpusAccessApi = SharedInit.initCorpusAccessApi(args)


    replMain().run(
      "corpusAccessApi" -> corpusAccessApi
    )

    corpusAccessApi.corpusAccessDB.shutdown()
  }


  def replMain() = ammonite.Main(
    storageBackend = new Storage.Folder(Defaults.ammoniteHome),
    predefCode = predef ,
    defaultPredef = true,
    wd = pwd,
    welcomeBanner = Some(SharedInit.welcomeBanner),
    inputStream = System.in,
    outputStream  = System.out,
    errorStream = System.out,
    verboseOutput = true,
    colors = replColors
  )

  run(args)

  // Kill any daemon threads
  System.exit(0)

}
