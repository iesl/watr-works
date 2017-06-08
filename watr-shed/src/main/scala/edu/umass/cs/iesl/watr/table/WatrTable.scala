package edu.umass.cs.iesl.watr
package table

import ammonite.ops._

import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
// import extract.images._
import corpora._
import segment._
import bioarxiv._

import TypeTags._

import docstore._
object SharedInit {
  val predef =
    s"""|import edu.umass.cs.iesl.watr
        |import ammonite.ops._
        |import ammonite.ops.ImplicitWd._
        |import watr._, spindex._, geometry._, table._
        |import corpora._
        |import textreflow._
        |import textreflow.data._
        |import docstore._
        |import bioarxiv._, BioArxiv._, BioArxivOps._
        |import watrmarks.StandardLabels._
        |import ShellCommands._
        |import labeling.SampleLabelWidgets
        |implicit val db0: TextReflowDB = db
        |implicit val corpus0: Corpus = corpus
        |implicit val docStore: DocumentCorpus = db.docStore
        |""".stripMargin

  val welcomeBanner = s""">> WatrTable Shell <<"""

}


object WatrTable extends App {
  import SharedInit._

  import ShellCommands._

  def run(args: Array[String]): Unit = {
    val dbname = args(0)

    val db = initReflowDB(dbname)

    // replMain().run(replArgs: Bind[Any]*)
    replMain().run(
      "corpus" -> initCorpus(),
      "db" -> db,
      "barx" -> BioArxivOps
    )

    db.shutdown()
  }


  def replMain() = ammonite.Main(
    // storageBackend = new Storage.Folder(Defaults.ammoniteHome)
    predef = predef,
    defaultPredef = true,
    wd = pwd,
    welcomeBanner = Some(SharedInit.welcomeBanner),
    inputStream = System.in,
    outputStream  = System.out,
    errorStream = System.err,
    verboseOutput = false
  )


  run(args)

}

object ShellCommands extends CorpusEnrichments with DocumentCorpusEnrichments {

  def initReflowDB(dbname: String): TextReflowDB = {
    val doLogging = false
    val loggingProp = if (doLogging) "?loglevel=2" else ""

    val tables = new TextReflowDBTables()
    new TextReflowDB(tables,
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
  // import scala.concurrent.ExecutionContext.Implicits.global
  // import scala.concurrent.duration._
  // import fs2.util.syntax._

  // implicit val S = Strategy.fromExecutor(global)
  implicit val S = Strategy.fromCachedDaemonPool()
  // implicit val Sch = Scheduler.fromFixedDaemonPool(3, "timeouts")
  // implicit val Sch = Scheduler.fromScheduledExecutorService(S)
  val T = implicitly[Async[Task]]

  def segmentAll(n: Int=0, skip: Int=0)(implicit docStore: DocumentCorpus, corpus: Corpus): Unit = {

    val allEntries = corpus.entryStream()
    val skipped = if (skip > 0) allEntries.drop(skip.toLong) else allEntries
    val entries = if (n > 0) skipped.take(n.toLong) else skipped

    val prog0 = entries.take(1)
      .map { corpusEntry =>
        println(s"processing first entry ")
        segment(corpusEntry)
      }

    // val chunked0 = entries
    //   .drop(1)
    //   .through(pipe.zipWithIndex)
    //   .chunkN(1, allowFewer=true)
    //   .map { chunks =>
    //     chunks.map(Stream.chunk)
    //       .reduce(_ ++ _)
    //       .covary[Task]
    //       .map { case (corpusEntry, i) =>
    //         println(s"processing entry ${i}")
    //         segment(corpusEntry)
    //       }
    //   }

    val chunked = entries
      .drop(1)
      .through(pipe.zipWithIndex)
      .chunkN(1, allowFewer=true)
      .map { chunks =>
        chunks.map(Stream.chunk)
          .reduce(_ ++ _)
          .covary[Task]
          .evalMap { case (corpusEntry, i) =>
            val t = Task.delay {
              println(s"processing entry ${i}")
              segment(corpusEntry)
              println(s"done entry ${i}")
            }
            t
          }
      }



    val prog = concurrent.join(10)(chunked)
    println(s"constructed program")

    println(s"running first entry")
    prog0.run.unsafeRun()

    println(s"running remaining entries")
    val retval = prog.run.unsafeRun
  }


  def segment(corpusEntry: CorpusEntry)(implicit docStore: DocumentCorpus): Unit = {
    for {
      pdfArtifact    <- corpusEntry.getPdfArtifact
      pdfPath        <- pdfArtifact.asPath.toOption
      stableId = DocumentID(corpusEntry.entryDescriptor)
    } yield {

      if (docStore.getDocument(stableId).isDefined) {
        println(s"segmenting ${stableId} failed. Already exists.")

      } else {
        println(s"segmenting ${stableId}")

        val segmenter = DocumentSegmenter
          .createSegmenter(stableId, pdfPath, docStore)

        segmenter.runPageSegmentation()
        println()
      }
    }
  }


}
