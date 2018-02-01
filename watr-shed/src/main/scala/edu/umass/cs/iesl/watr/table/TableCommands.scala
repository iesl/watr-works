package edu.umass.cs.iesl.watr
package table

// import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
import corpora._
import corpora.filesys._
import corpora.database._
// import segment._
// import geometry.syntax._
// import ammonite.{ops => fs}

// import TypeTags._
import apps._

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


  import fs2._
  import cats.effect._
  import utils.Timer.time
  // import fs2.async
  val T = implicitly[Async[IO]]
  import scala.concurrent.ExecutionContext.Implicits.global
  // implicit val strategy = Strategy.fromExecutionContext(global)

  def chooseEntries(n: Int, skip: Int)(implicit corpusAccessApi: CorpusAccessApi): Stream[IO, CorpusEntry] = {
    val corpus = corpusAccessApi.corpus
    val allEntries = corpus.entryStream()
    val skipped = if (skip > 0) allEntries.drop(skip.toLong) else allEntries
    val entries = if (n > 0) skipped.take(n.toLong) else skipped
    entries
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

  def segmentAllParallel(n: Int=Int.MaxValue, skip: Int=0)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val processStream = TextWorksActions.buildProcessStream(TextWorksConfig.Config(
      IOConfig(
        inputMode = Some(InputMode.CorpusFile(corpusAccessApi.corpus.corpusRoot.toNIO, None)),
        outputPath= None, // : Option[nio.Path] = None,
        overwrite = true, // : Boolean = false,
      )
    ))

    val parProg0 = processStream
      .zipWithIndex
      .segmentN(10, allowFewer=true)
      .map{ segment =>

        segment.map { case (maybeSegmenter, i) =>
          val xx = IO {
            println(s"processing entry ${i}")
            maybeSegmenter match {
              case Right(segmenter) =>
                println(s"${i}.Importing ${segmenter.stableId} into database.")
                corpusAccessApi.corpusAccessDB.docStore.batchImport(segmenter.docStore.asInstanceOf[MemDocZoningApi])
              case Left(message) =>
                println(s"${i}. Error ${message}")
            }
            println(s"done entry ${i}")
          }
          fs2.async.fork(xx)
        }
      }
    parProg0.compile.drain.unsafeRunSync()


    // val parProg = processStream
    //   .zipWithIndex
    //   .chunkLimit(10).chunks
    //   .map{ chunks => chunks.map(Stream.chunk) }
    //   .map{streams => streams.map { str =>
    //     str.covary[IO]
    //       .evalMap { case (maybeSegmenter, i) =>
    //         IO {
    //           println(s"processing entry ${i}")
    //           maybeSegmenter match {
    //             case Right(segmenter) =>
    //               println(s"${i}.Importing ${segmenter.stableId} into database.")
    //               corpusAccessApi.corpusAccessDB.docStore.batchImport(segmenter.docStore.asInstanceOf[MemDocZoningApi])
    //             case Left(message) =>
    //               println(s"${i}. Error ${message}")
    //           }
    //           println(s"done entry ${i}")
    //         }
    //       }
    //   }}

    // parProg.compile.drain.unsafeRunSync()

  }

}
