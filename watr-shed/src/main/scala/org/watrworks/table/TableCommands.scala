package edu.umass.cs.iesl.watr
package table

import TypeTags._
import apps._
import cats.effect._
import corpora._
import corpora.database._
import utils.Timer.time
import formats.TextGridOutputFormats._
import doobie.implicits._

object InitialSegmentationCommands {

  private[this] val log = org.log4s.getLogger

  import apps.ProcessPipelineSteps._

  def initReflowDB(dbname: String, dbpass: String)(implicit cs: ContextShift[IO]): CorpusAccessDB = {
    new CorpusAccessDB(
      dbname=dbname,
      dbuser="watrworker",
      dbpass=dbpass
    )
  }


  private def importTextgridIntoDB()(implicit
    corpusAccessApi: CorpusAccessApi
  ):fs2.Pipe[IO, MarkedOutput, MarkedOutput] = {
    _.map {
      case m@ Right(Processable.ExtractedTextGridFile(textGridFile, input)) =>
        log.info(s"Importing ${textGridFile} into database.")
        val docDef = readDocumentJsonDef(textGridFile)

        val desc = docDef.description
        val stableId = desc.drop("Extracted pages for ".length())

        val docId = corpusAccessApi.corpusAccessDB.runq {
          corpusAccessApi.corpusAccessDB.updateGetKey[Int]("document",
            sql"""insert into document (stableId) values (${stableId})""".update
          )
        }


        docDef.pages.zipWithIndex.foreach { case (page, pageNum) =>
          val (l, t, w, h) = page.pageGeometry

          corpusAccessApi.corpusAccessDB.runq {
            corpusAccessApi.corpusAccessDB.updateGetKey("page",
              sql"""|insert into page (document, pagenum, bleft, btop, bwidth, bheight)
                    |values ($docId, $pageNum, $l, $t, $w, $h)
                    |""".stripMargin.update
            )
          }
        }

        m

      case x => x
    }
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

  private def skipEntriesAlreadyInDB(conf: TextWorksConfig.Config)(implicit
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
          } else {
            log.info(s"Importing ${corpusEntry}")
            Right(input)
          }

        case x => Left(x)
      }
      case x => x
    }
  }

  def extractTextToFile(n: Int=Int.MaxValue, skip: Int=0, regexFilter: Option[String]=None)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val conf = TextWorksConfig.Config(IOConfig(
      inputMode = Some(InputMode.CorpusFile(corpusAccessApi.corpus.corpusRoot.toNIO)),
      numToRun=n, numToSkip=skip,
      outputPath= None,
      overwrite = true,
      pathFilter = regexFilter
    ))

    runTextExtractionPipeline(conf)
  }

  def segmentAll(n: Int=Int.MaxValue, skip: Int=0, regexFilter: Option[String]=None)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val conf = TextWorksConfig.Config(IOConfig(
      inputMode = Some(InputMode.CorpusFile(corpusAccessApi.corpus.corpusRoot.toNIO)),
      numToRun=n, numToSkip=skip,
      outputPath= None,
      overwrite = true,
      pathFilter = regexFilter
    ))

    val processStream = createInputStream[IO](conf.ioConfig)
      .through(initMarkedInput())
      .through(dropSkipAndRun(conf.ioConfig))
      .through(cleanDBArtifacts(conf))
      .through(cleanFileArtifacts(conf))
      .through(markUnextractedProcessables(conf))
      .through(runSegmentation(conf))
      .through(writeExtractedTextFile(conf))
      .through(importPaperIntoDB())

    processStream.compile.drain
      .unsafeRunSync()

  }

  def importPreSegmentedPapers(n: Int=Int.MaxValue, skip: Int=0)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val conf = TextWorksConfig.Config(IOConfig(
      inputMode = Some(InputMode.CorpusFile(corpusAccessApi.corpus.corpusRoot.toNIO)),
      outputPath= None,
      overwrite = true
    ))

    val processStream = createInputStream[IO](conf.ioConfig)
      .drop(skip.toLong).take(n.toLong)
      .through(initMarkedInput())
      .through(skipEntriesAlreadyInDB(conf))
      .through(pickupTextgridFiles(conf))
      .through(importTextgridIntoDB())

    processStream.compile.drain
      .unsafeRunSync()
  }

}
