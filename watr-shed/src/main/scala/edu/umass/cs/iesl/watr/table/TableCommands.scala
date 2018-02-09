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
  // import TextWorksActions._
  // import Threading._

  private[this] val log = org.log4s.getLogger

  def initReflowDB(dbname: String, dbpass: String): CorpusAccessDB = {
    new CorpusAccessDB(
      dbname=dbname,
      dbuser="watrworker",
      dbpass=dbpass
    )
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

  import apps.ProcessPipes._

  def segmentAll(n: Int=Int.MaxValue, skip: Int=0)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
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
