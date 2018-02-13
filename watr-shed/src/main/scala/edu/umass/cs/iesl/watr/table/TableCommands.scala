package edu.umass.cs.iesl.watr
package table

import corpora._
import corpora.database._
import apps._
import cats.effect._
import utils.Timer.time
import TypeTags._


object ShellCommands {

  private[this] val log = org.log4s.getLogger

  import apps.ProcessPipelineSteps._

  def initReflowDB(dbname: String, dbpass: String): CorpusAccessDB = {
    new CorpusAccessDB(
      dbname=dbname,
      dbuser="watrworker",
      dbpass=dbpass
    )
  }


  private def importPaperIntoDB()(implicit
    corpusAccessApi: CorpusAccessApi
  ):fs2.Pipe[IO, MarkedOutput, MarkedOutput] = {
    _.map {
      case m@ Right(Processable.ExtractedFile(segmentation, input)) =>
        log.info(s"Importing ${segmentation.stableId} into database.")

        time("DB Batch Import") {
          corpusAccessApi.corpusAccessDB.docStore.batchImport(segmentation.docStore.asInstanceOf[MemDocZoningApi])
        }
        println(s"done ${segmentation.stableId}")
        m

      case x => x
    }
  }

  private def cleanDBArtifacts(conf: TextWorksConfig.Config)(implicit
    corpusAccessApi: CorpusAccessApi
  ): fs2.Pipe[IO, MarkedInput, MarkedInput] = {
    _.map {
      case m@ Right(input) => input match {
        case p@ Processable.CorpusFile(corpusEntry) =>
          val stableId = DocumentID(corpusEntry.entryDescriptor)
          val maybeDoc = corpusAccessApi.docStore.getDocument(stableId)
          val isInDb = maybeDoc.isDefined

          if (isInDb) {
            log.info(s"Skipping ${corpusEntry}; already in DB")
            Left(input)
            // if (conf.ioConfig.overwrite) {
            //   log.info(s"Overwriting DB Entry for ${corpusEntry}")
            //   Right(p)
            // } else {
            //   log.info(s"Skipping ${corpusEntry}; already in DB")
            //   Left(input)
            // }
          } else {
            Right(input)
          }

        case x => Left(x)
      }
      case x => x
    }
  }


  def segmentAll(n: Int=Int.MaxValue, skip: Int=0)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val conf = TextWorksConfig.Config(IOConfig(
      inputMode = Some(InputMode.CorpusFile(corpusAccessApi.corpus.corpusRoot.toNIO)),
      outputPath= None,
      overwrite = true
    ))

    val processStream = createInputStream[IO](conf.ioConfig)
      .drop(skip.toLong).take(n.toLong)
      .through(initMarkedInput())
      .through(cleanDBArtifacts(conf))
      .through(cleanFileArtifacts(conf))
      .through(markUnextractedProcessables(conf))
      .through(runSegmentation(conf))
      .through(writeExtractedTextFile(conf))
      .through(importPaperIntoDB())

    processStream.compile.drain
      .unsafeRunSync()

  }

  def cleanDatabaseDocuments()(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    corpusAccessApi.corpusAccessDB.deleteAllDocuments()
  }

}
