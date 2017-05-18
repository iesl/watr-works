package edu.umass.cs.iesl.watr
package table

import ammonite.ops._

import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
import extract.images._
import corpora._
import segment._
import bioarxiv._

import TypeTags._

import docstore._
object SharedInit {
  val predef =
    s"""| import edu.umass.cs.iesl.watr
        | import ammonite.ops._
        |import ammonite.ops.ImplicitWd._
        |import watr._, spindex._, geometry._, table._
        |import corpora._
        |import textreflow._
        |import textreflow.data._
        |import docstore._
        |import bioarxiv._, BioArxiv._, BioArxivOps._
        |import watrmarks.StandardLabels._
        |import ShellPrettyPrinters._
        |import ShellCommands._
        |import labeling.SampleLabelWidgets
        |implicit val pp0 = pprintComponent
        |implicit val pp1 = pprintBox
        |implicit val pp2 = pprintTextReflow
        |implicit val pp3 = pprintLabelWidget
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



  def extractPageImages(corpusEntry: CorpusEntry): Either[String, Path] = {
    println(s"extracting ${corpusEntry}")
    for {
      pdfArtifact    <- corpusEntry.getPdfArtifact.toRight[String](s"pdf file not found ${corpusEntry}").right
      pdfPath        <- pdfArtifact.asPath.toOption.toRight(s"pdf path invalid ${pdfArtifact}").right

      pageImageArtifacts <- Right(corpusEntry.ensureArtifactGroup("page-images")).right

      res <- if (pageImageArtifacts.getArtifacts.isEmpty) {
        ExtractImages.extract(pdfPath, pageImageArtifacts.rootPath)
          .left.map{ err => s"Error: ${err}" }
          .right.map{ succ => pageImageArtifacts.rootPath }
          .right

      } else {
        println(s"Images already extracted")
        Right(pageImageArtifacts.rootPath).right
      }

    } yield { res }
  }

  def setPageImages(corpusEntry: CorpusEntry)(implicit docStore: DocumentCorpus): Unit = {
    for {
      imagesPath <-  extractPageImages(corpusEntry).right
      stableId <- Right(DocumentID(corpusEntry.entryDescriptor)).right
      docId <- docStore.getDocument(stableId).toRight(s"no docStore document found for ${stableId}").right
    } yield {

      val pageImages = ExtractImages.load(imagesPath)

      pageImages.images.zipWithIndex
        .foreach { case (image, i) =>
          docStore.getPage(docId, PageNum(i))
            .foreach{ pageId =>
              docStore.setPageImage(pageId, image.bytes)
            }
        }
    }
  }

  import fs2._
  import fs2.util.Async
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val S = Strategy.fromExecutor(global)
  val T = implicitly[Async[Task]]

  def segmentAll(n: Int=0, skip: Int=0, extractImages: Boolean=false)(implicit docStore: DocumentCorpus, corpus: Corpus): Unit = {

    val allEntries = corpus.entryStream()
    val skipped = if (skip > 0) allEntries.drop(skip.toLong) else allEntries
    val entries = if (n > 0) skipped.take(n.toLong) else skipped

    val prog0 = entries.take(1)
      .map { corpusEntry =>
        println(s"processing first entry ")
        segment(corpusEntry, extractImages=false)
      }

    val chunked = entries
      .drop(1)
      .through(pipe.zipWithIndex)
      .chunkN(5, allowFewer=true)
      .map { chunks =>
        chunks.map(Stream.chunk)
          .reduce(_ ++ _)
          .covary[Task]
          .map { case (corpusEntry, i) =>
            println(s"processing entry ${i}")
            segment(corpusEntry, extractImages)
          }
      }

    val prog = concurrent.join(12)(chunked)
    println(s"constructed program")

    println(s"running first entry")
    prog0.run.unsafeRun()

    println(s"running remaining entries")
    val retval = prog.run.unsafeRun
  }

  def extractAllPageImages(n: Int=0, skip: Int=0)(implicit corpus: Corpus): Unit = {

    val allEntries = corpus.entryStream()
    val skipped = if (skip > 0) allEntries.drop(skip.toLong) else allEntries
    val entries = if (n > 0) skipped.take(n.toLong) else skipped


    val chunked = entries
      .through(pipe.zipWithIndex)
      .chunkN(5, allowFewer=true)
      .map { chunks =>
        chunks.map(Stream.chunk)
          .reduce(_ ++ _)
          .covary[Task]
          .map { case (corpusEntry, i) =>
            println(s"processing entry ${i}")
            extractPageImages(corpusEntry)
          }
      }

    val prog = concurrent.join(12)(chunked)
    val retval = prog.run.unsafeRun
  }


  def segment(corpusEntry: CorpusEntry, extractImages: Boolean=true)(implicit docStore: DocumentCorpus): Unit = {
    for {
      pdfArtifact    <- corpusEntry.getPdfArtifact
      pdfPath        <- pdfArtifact.asPath.toOption
    } yield {

      val stableId = DocumentID(corpusEntry.entryDescriptor)

      docStore.getDocument(stableId)
        .flatMap({docId =>
          println(s"segmenting ${stableId} ## ${docId} failed. Already exists.")
          None
        })
        .getOrElse {

          println(s"segmenting ${stableId}")

          val segmenter = DocumentSegmenter
            .createSegmenter(stableId, pdfPath, docStore)

          segmenter.runPageSegmentation()
          if (extractImages) {
            setPageImages(corpusEntry)
          }
          println()
        }
    }
  }


}
