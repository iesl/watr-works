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

object ShellCommands extends CorpusEnrichments with DocumentZoningApiEnrichments {

  def initReflowDB(dbname: String, dbpass: String): CorpusAccessDB = {
    // val doLogging = false
    // val loggingProp = if (doLogging) "?loglevel=2" else ""
    val tables = new CorpusAccessDBTables()
    new CorpusAccessDB(tables,
      dbname=dbname,
      dbuser="watrworker",
      dbpass=dbpass
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
          segment(corpusEntry, commitToDb=true, writeRTrees=false)
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
              segment(corpusEntry, commitToDb=true, writeRTrees=false)
              println(s"done entry ${i}")
            }
          }
      }

    concurrent.join(10)(chunked)
      .run.unsafeRun
  }

  def segmentEntry(stableId: String@@DocumentID, commitToDb: Boolean=false, writeRTrees: Boolean=false)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val docStore = corpusAccessApi.docStore

    if (commitToDb && docStore.getDocument(stableId).isDefined) {
      println(s"document ${stableId} already exists in database, skipping.")
    } else if (corpusAccessApi.corpus.hasEntry(stableId.unwrap)) {

      println(s"segmenting ${stableId}")


      for {
        corpusEntry    <- corpusAccessApi.corpus.entry(stableId.unwrap)
        // pdfArtifact    <- corpusEntry.getPdfArtifact
        // pdfPath        <- pdfArtifact.asPath.toOption
      } {
        segment(corpusEntry, commitToDb, writeRTrees)
        // val memZoneApi = new MemDocZoningApi
        // val segmenter = DocumentSegmenter
        //   .createSegmenter(stableId, pdfPath, memZoneApi)

        // segmenter.runPageSegmentation()

        // if (commitToDb) {
        //   println(s"Importing ${stableId} into database.")
        //   corpusAccessApi.corpusAccessDB.docStore.batchImport(memZoneApi)
        //   println(s"Done importing ${stableId}")
        // }

      }
    }
  }

  def segment(
    corpusEntry: CorpusEntry,
    commitToDb: Boolean=true,
    writeRTrees: Boolean=false
  )(implicit corpusAccessApi: CorpusAccessApi): Unit = {

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

        if (writeRTrees) {
          println("Writing RTrees")
          val rtreeGroup = corpusEntry.ensureArtifactGroup("rtrees")

          for {
            pageNum <- segmenter.mpageIndex.getPages
          } {
            val pageIndex = segmenter.mpageIndex.getPageIndex(pageNum)
            val rtree = pageIndex.componentIndex.spatialIndex

            val rtreeSerializer = DocumentSegmenter.createRTreeSerializer()

            val baos = new java.io.ByteArrayOutputStream()
            rtreeSerializer.write(rtree, baos)
            baos.toByteArray()
            val rtreeArtifactName = s"page-${pageNum}.rtree"
            rtreeGroup.putArtifactBytes(rtreeArtifactName, baos.toByteArray())
          }
        }

        if (commitToDb) {
          println(s"Importing ${stableId} into database.")
          corpusAccessApi.corpusAccessDB.docStore.batchImport(memZoneApi)
          println(s"Done importing ${stableId}")
        } else {
          println(s"DB Importing disabled.")
        }

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
