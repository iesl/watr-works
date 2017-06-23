package edu.umass.cs.iesl.watr
package table

import ammonite.ops._

import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
import watrmarks.{StandardLabels => LB}
import corpora._
import corpora.filesys._
import corpora.database._
import segment._
import bioarxiv._

import TypeTags._

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
        |import watrmarks.StandardLabels._
        |import TypeTags._
        |import ShellCommands._
        |import labeling.SampleLabelWidgets
        |implicit val corpusAccessApi0: CorpusAccessApi = corpusAccessApi
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

    val db = initReflowDB(dbname)

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

object ShellCommands extends CorpusEnrichments with DocumentZoningApiEnrichments {

  def initReflowDB(dbname: String): CorpusAccessDB = {
    // val doLogging = false
    // val loggingProp = if (doLogging) "?loglevel=2" else ""
    val tables = new CorpusAccessDBTables()
    new CorpusAccessDB(tables,
      dbname=dbname,
      dbuser="watrworker",
      dbpass="watrpasswd"
    )
  }

  def initCorpus(): Corpus = {
    initCorpus(pwd)
  }


  import fs2._
  import fs2.util.Async
  implicit val S = Strategy.fromCachedDaemonPool()
  val T = implicitly[Async[Task]]

  def chooseEntries(n: Int, skip: Int)(implicit corpusAccessApi: CorpusAccessApi): Stream[Task, CorpusEntry] = {
    val corpus = corpusAccessApi.corpus
    val allEntries = corpus.entryStream()
    val skipped = if (skip > 0) allEntries.drop(skip.toLong) else allEntries
    val entries = if (n > 0) skipped.take(n.toLong) else skipped
    entries
  }

  def segmentAll(n: Int=0, skip: Int=0)(implicit corpusAccessApi: CorpusAccessApi): Unit = {

    val prog = chooseEntries(n, skip)
      .through(pipe.zipWithIndex)
      .evalMap { case (corpusEntry, i) =>
        Task.delay {
          println(s"processing entry ${i}")
          segment(corpusEntry)
          println(s"done entry ${i}")
        }
      }

    val _ = prog.run.unsafeRun
  }

  def segmentAllParallel(n: Int=0, skip: Int=0)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val entries = chooseEntries(n, skip)

    val prog0 = entries.take(1)
      .map { corpusEntry =>
        println(s"processing first entry ")
        segment(corpusEntry)
      }
    println(s"running first entry")
    prog0.run.unsafeRun()


    println(s"running remaining entries")
    val chunked = entries.drop(1)
      .through(pipe.zipWithIndex)
      .chunkN(1, allowFewer=true)
      .map { chunks =>
        chunks.map(Stream.chunk)
          .reduce(_ ++ _)
          .covary[Task]
          .evalMap { case (corpusEntry, i) =>
            Task.delay {
              println(s"processing entry ${i}")
              segment(corpusEntry)
              println(s"done entry ${i}")
            }
          }
      }

    concurrent.join(10)(chunked)
      .run.unsafeRun
  }

  val tmpSampleDocs = Seq(
    // processed 3 times??
    "10.1101-021006.d",
    // Started and finished
    "10.1101-095851.d",
    "10.1101-093740.d",
    "10.1101-093492.d",
    "10.1101-093138.d",
    "10.1101-091470.d",
    "10.1101-090910.d",
    "10.1101-089359.d",
    "10.1101-086165.d",
    "10.1101-085670.d",
    "10.1101-084780.d",
    "10.1101-082461.d",
    "10.1101-080903.d",
    "10.1101-080028.d",
    "10.1101-075721.d",
    "10.1101-075465.d",
    "10.1101-074880.d",
    "10.1101-074864.d",
    "10.1101-065680.d",
    "10.1101-064873.d",
    "10.1101-057828.d",
    "10.1101-056457.d",
    "10.1101-056143.d",
    "10.1101-053629.d",
    "10.1101-050096.d",
    "10.1101-049114.d",
    "10.1101-048892.d",
    "10.1101-035667.d",
    "10.1101-034843.d",
    "10.1101-034066.d",
    "10.1101-032458.d",
    "10.1101-031443.d",
    "10.1101-029207.d",
    "10.1101-028597.d",
    "10.1101-026252.d",
    "10.1101-022889.d",
    "10.1101-022111.d",
    "10.1101-021691.d",
    "10.1101-019901.d",
    "10.1101-014985.d",
    "10.1101-014233.d",
    "10.1101-009670.d",
    "10.1101-007310.d",
    "10.1101-005611.d",
    "10.1101-003889.d",
    "10.1101-001750.d",
    "10.1101-097840.d",
    "10.1101-094904.d",
    "10.1101-075275.d",
    "10.1101-054015.d",
    "10.1101-025221.d",
    "10.1101-006098.d",
    "10.1101-006080.d"
  )

  def segmentEntry(stableId: String@@DocumentID, commitToDb: Boolean=false)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val docStore = corpusAccessApi.docStore

    if (commitToDb && docStore.getDocument(stableId).isDefined) {
      println(s"document ${stableId} already exists in database, skipping.")
    } else if (corpusAccessApi.corpus.hasEntry(stableId.unwrap)) {

      println(s"segmenting ${stableId}")


      for {
        corpusEntry    <- corpusAccessApi.corpus.entry(stableId.unwrap)
        pdfArtifact    <- corpusEntry.getPdfArtifact
        pdfPath        <- pdfArtifact.asPath.toOption
      } {
        val memZoneApi = new MemDocZoningApi
        val segmenter = DocumentSegmenter
          .createSegmenter(stableId, pdfPath, memZoneApi)

        segmenter.runPageSegmentation()

        if (commitToDb) {
          println(s"Importing ${stableId} into database.")
          corpusAccessApi.corpusAccessDB.docStore.batchImport(memZoneApi)
          println(s"Done importing ${stableId}")
        }

      }
    }
  }

  def segment(corpusEntry: CorpusEntry)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val docStore = corpusAccessApi.docStore

    for {
      pdfArtifact    <- corpusEntry.getPdfArtifact
      pdfPath        <- pdfArtifact.asPath.toOption
      stableId = DocumentID(corpusEntry.entryDescriptor)
    } {

      if (docStore.getDocument(stableId).isDefined) {
        println(s"document ${stableId} already exists in database, skipping.")

      } else {
        println(s"segmenting ${stableId}")

        val memZoneApi = new MemDocZoningApi

        val segmenter = DocumentSegmenter
          .createSegmenter(stableId, pdfPath, memZoneApi)

        segmenter.runPageSegmentation()

        println(s"Importing ${stableId} into database.")
        corpusAccessApi.corpusAccessDB.docStore.batchImport(memZoneApi)
        println(s"Done importing ${stableId}")
        println()
      }
    }
  }

  def addUserAndLockTables(db: CorpusAccessDB)(): Unit = {
    db.runqOnce{ db.tables.UserTables.create.run }
    db.runqOnce{ db.tables.workflowTables.create.run }
  }

  def createDocumentPagesLabels()(implicit docStore: DocumentZoningApi): Unit = {
    for {
      stableId <- docStore.getDocuments()
      docId <- docStore.getDocument(stableId)
    } {
      val regions = docStore.getPages(docId)
        .map{ pageId =>
          val pageGeometry = docStore.getPageGeometry(pageId)
          docStore.getTargetRegion(
            docStore.addTargetRegion(pageId, pageGeometry)
          ).toPageRegion()
        }
      docStore.labelRegions(LB.DocumentPages, regions)
    }

  }

}
