package edu.umass.cs.iesl.watr
package table

import corpora._
import corpora.database._
import apps._
import segment.DocumentSegmentation
// import utils.Threading
import cats.effect._
import utils.Timer.time
import TypeTags._

object ShellCommands extends DocumentZoningApiEnrichments with LabeledPageImageWriter {

  def initReflowDB(dbname: String, dbpass: String): CorpusAccessDB = {
    // val doLogging = false
    // val loggingProp = if (doLogging) "?loglevel=2" else ""
    new CorpusAccessDB(
      dbname=dbname,
      dbuser="watrworker",
      dbpass=dbpass
    )
  }


  def segmentAll(n: Int=Int.MaxValue, skip: Int=0)(implicit corpusAccessApi: CorpusAccessApi): Unit = {

    val processStream = TextWorksActions.buildProcessStream(TextWorksConfig.Config(
      IOConfig(
        inputMode = Some(InputMode.CorpusFile(corpusAccessApi.corpus.corpusRoot.toNIO, None)),
        outputPath= None, // : Option[nio.Path] = None,
        overwrite = true, // : Boolean = false,
        // pathFilter= Some(), // : Option[String] = None,
        // numToRun  = Some(), // : Int = 0,
        // numToSkip = Some(), // : Int = 0

      )
    ))

    val st = processStream.drop(skip.toLong).take(n.toLong).map{ _ match {
      case Right(segmenter) =>
        println(s"Importing ${segmenter.stableId} into database.")

        time("batch import") {
          corpusAccessApi.corpusAccessDB.docStore.batchImport(segmenter.docStore.asInstanceOf[MemDocZoningApi])
        }

      case Left(message) =>
        println(s"Error: ${message}")

    }}

    st.compile.drain.unsafeRunSync()
  }

  // import Threading._
  private[this] val log = org.log4s.getLogger

  def markIfUnprocessed()(implicit corpusAccessApi: CorpusAccessApi): fs2.Pipe[IO, InputMode, Either[InputMode, InputMode]] = {
    s => s.map { input =>
      input match {
        case m@ InputMode.CorpusFile(_, Some(corpusEntry)) =>
          val stableId = DocumentID(corpusEntry.entryDescriptor)
          val isUnprocessed = corpusAccessApi.docStore.getDocument(stableId).isEmpty
          if (isUnprocessed) {
            Right(input)
          } else {
            log.info(s"Already processed ${input}")
            Left(input)
          }

        case _ => Left(input)
      }
    }
  }

  def importPaperIntoDB()(implicit corpusAccessApi: CorpusAccessApi): fs2.Sink[IO, Either[String, DocumentSegmentation]] = {
    s => s.map {
      case Right(segmenter) =>
        log.info(s"Importing ${segmenter.stableId} into database.")

        time("DB Batch Import") {
          corpusAccessApi.corpusAccessDB.docStore.batchImport(segmenter.docStore.asInstanceOf[MemDocZoningApi])
        }
        println(s"done ${segmenter.stableId}")

      case x =>  ()
    }

  }

  import TextWorksActions._

  def segmentAllParallel(n: Int=Int.MaxValue, skip: Int=0)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val conf = TextWorksConfig.Config(IOConfig(
      inputMode = Some(InputMode.CorpusFile(corpusAccessApi.corpus.corpusRoot.toNIO, None)),
      outputPath= None,
      overwrite = true
    ))

    val parProg0 = TextWorksActions.getInputStream(conf)
      .through(markIfUnprocessed())
      .through(cleanOldArtifacts(conf))
      .through(runSegmentation(conf))
      .through(importPaperIntoDB())

    parProg0.compile.drain.unsafeRunSync()

  }

}
