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

  def setPageImages(corpusEntry: CorpusEntry)(implicit docStore: DocumentCorpus): Unit = {
    for {
      pdfArtifact    <- corpusEntry.getPdfArtifact
      pdfPath        <- pdfArtifact.asPath.toOption
    } yield {

      print(s"extracting page images")
      val stableId = DocumentID(corpusEntry.entryDescriptor)

      docStore.getDocument(stableId).foreach({docId =>
        val pageImageArtifacts = corpusEntry.ensureArtifactGroup("page-images")
        val pageImages = if (pageImageArtifacts.getArtifacts.isEmpty) {
          ExtractImages.extract(pdfPath, pageImageArtifacts.rootPath)
        } else {
          ExtractImages.load(pageImageArtifacts.rootPath)
        }

        pageImages.images.zipWithIndex.foreach {
          case (image, i) =>
            docStore
              .getPage(docId, PageNum(i))
              .foreach{ pageId =>
                docStore.setPageImage(pageId, image.bytes)
              }
        }
      })
    }
  }

  import fs2._
  import fs2.util.Async
  // import fs2.util.syntax._
  // import fs2.async._
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val S = Strategy.fromExecutor(global)
  val T = implicitly[Async[Task]]

  def segmentAll()(implicit docStore: DocumentCorpus, corpus: Corpus): Unit = {

    // val t2 = for {
    //   e <- corpus.entryStream()
    //   f <- Stream.emit(Task.start( Task.delay {
    //     println(s"segmenting ${e}")
    //     segment(e, extractImages=false)
    //   }))
    //   // g <- f
    // } yield ()

    // t2.run.unsafeRun()

    // corpus.entryStream().parallelTraverse{ e =>
    //     Task.start {
    //       // Task.delay{
    //         segment(e, extractImages=false)
    //       // }
    //     }
    // }

    // corpus.entryStream().flatMap{ e =>
    //   Stream.emit(
    //     Task.start({
    //       Task.delay{
    //         segment(e, extractImages=false)
    //       }
    //     })
    //   )
    // }

    // val qwer = corpus.entryStream()
    //   .through(pipe.take(10))
    //   .through(pipe.zipWithIndex)
    //   .through(pipe.split(_._2 % 3 == 0))
    //   .map { v =>
    //     Stream.emits(v).covary[Task].map { case (corpusEntry, i) =>
    //       println(s"running # $i")
    //       segment(corpusEntry, extractImages=false)
    //     }
    //   }
    val qwer = corpus.entryStream()
      .through(pipe.take(6))
      .through(pipe.zipWithIndex)
      .chunkN(5, allowFewer=true)
      .map { chunks =>
        chunks.map(Stream.chunk)
          .reduce(_ ++ _)
          .covary[Task]
          .map { case (corpusEntry, i) =>
            println(s"processing entry ${i}")
            segment(corpusEntry, extractImages=false)
          }
      }

    val asdf = concurrent.join(3)(qwer)

    // asdf.runLog.unsafeRunAsync { case e =>
    //   println(s"callback? $e")
    // }

    val retval = asdf.runLog.unsafeRun

    println(s"got: $retval")

    // // // corpus.entryStream.flatMap{ a => Stream.eval{ Task.delay{ segment(a, false) } } }
    // val wer = corpus.entryStream().flatMap { corpusEntry =>
    //   val t = Task.start{
    //     Task.now{
    //       println(s"segmenting ${corpusEntry}")
    //       segment(corpusEntry, extractImages=false)
    //     }
    //   }
    //   Stream.eval_(t)
    // }

    // wer.take(3).run.unsafeRun()


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
