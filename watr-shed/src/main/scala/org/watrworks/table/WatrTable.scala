package edu.umass.cs.iesl.watr
package table

// import ammonite.util._
import ammonite.ops._
import ammonite.runtime.Storage
import ammonite.main.Defaults
import corpora._
import corpora.filesys.Corpus
import utils.{PathUtils => P}

import cats.effect._

class SharedInit()(implicit cs: ContextShift[IO]) extends utils.AppMainBasics {
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

object SharedConsts extends utils.AppMainBasics {

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
        |ammonite.repl.ReplBridge.value0.pprinter.update(SharedConsts.pprinter)
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

}

object WatrTable extends IOApp with utils.AppMainBasics {
  import SharedConsts._

  def run(args: List[String]): IO[ExitCode] = {

    val argMap = argsToMap(args)

    val sharedInit = new SharedInit()
    implicit val corpusAccessApi = sharedInit.initCorpusAccessApi(args.toArray)

    val doQuickRun = argMap.get("quick-run").nonEmpty
    if (doQuickRun) {
      val doc = "101021ja070464y.pdf.d"
      InitialSegmentationCommands.extractTextToFile(1, 0, Some(doc))

      // InitialSegmentationCommands.extractTextToFile()
      // RefBlockLabelConversion.convertAllRefLabels(1, 0, Some(doc), Some("ReferenceBlock"))
      // AnnotationExporters.writeAllLabeledTexts("tmp.d", watrmarks.Label("Reference"))

    } else {

      replMain()

      replMain().run(
        "corpusAccessApi" -> corpusAccessApi
      )
    }

    IO.pure(ExitCode.Success)

    // corpusAccessApi.corpusAccessDB.shutdown()


  }


  def replMain() = ammonite.Main(
    storageBackend = new Storage.Folder(Defaults.ammoniteHome),
    predefCode = predef ,
    defaultPredef = true,
    wd = pwd,
    welcomeBanner = Some(SharedConsts.welcomeBanner),
    inputStream = System.in,
    outputStream  = System.out,
    errorStream = System.out,
    verboseOutput = true,
    colors = replColors
  )

  // run(args)
  // Kill any daemon threads
  // System.exit(0)

}
