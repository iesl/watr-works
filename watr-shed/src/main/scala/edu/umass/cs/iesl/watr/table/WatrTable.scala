package edu.umass.cs.iesl.watr
package table

import ammonite.ops._

import corpora._
import bioarxiv._


object SharedInit {

  val predef =
    s"""|import edu.umass.cs.iesl.watr
        |import watr._, spindex._, geometry._, table._
        |import corpora._
        |import corpora.filesys._
        |import corpora.database._
        |import textreflow._
        |import textreflow.data._
        |import bioarxiv._, BioArxiv._, BioArxivOps._
        |import watrmarks.{StandardLabels => LB}
        |import TypeTags._
        |import ShellCommands._
        |import labeling.SampleLabelWidgets
        |implicit val corpusAccessApi0: CorpusAccessApi = corpusAccessApi
        |implicit val docStore: DocumentZoningApi = corpusAccessApi.docStore
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

}

object WatrTable extends App {
  import SharedInit._

  import ShellCommands._

  def run(args: Array[String]): Unit = {
    val dbname = args(0)
    val passwd = args(1)

    val db = initReflowDB(dbname, passwd)

    val corpus = initCorpus()
    val corpusAccessApi = CorpusAccessApi(db, corpus)

    replMain().run(
      "corpusAccessApi" -> corpusAccessApi,
      "barx" -> BioArxivOps
    )

    db.shutdown()
  }


  def replMain() = ammonite.Main(
    // storageBackend = new Storage.Folder(Defaults.ammoniteHome)
    predefCode = predef,
    defaultPredef = true,
    wd = pwd,
    welcomeBanner = Some(SharedInit.welcomeBanner),
    inputStream = System.in,
    outputStream  = System.out,
    errorStream = System.err,
    verboseOutput = false,
    colors = replColors
  )


  run(args)

}
