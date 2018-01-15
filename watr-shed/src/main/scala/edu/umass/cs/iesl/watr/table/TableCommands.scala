package edu.umass.cs.iesl.watr
package table

// import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
import corpora._
import corpora.filesys._
import corpora.database._
import segment._
// import geometry.syntax._
import ammonite.{ops => fs}

import TypeTags._
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
  // import fs2.util.Async
  // implicit val S = Strategy.fromCachedDaemonPool()
  // val T = implicitly[Async[IO]]

  def chooseEntries(n: Int, skip: Int)(implicit corpusAccessApi: CorpusAccessApi): Stream[IO, CorpusEntry] = {
    val corpus = corpusAccessApi.corpus
    val allEntries = corpus.entryStream()
    val skipped = if (skip > 0) allEntries.drop(skip.toLong) else allEntries
    val entries = if (n > 0) skipped.take(n.toLong) else skipped
    entries
  }

  def segmentAll(n: Int=Int.MaxValue, skip: Int=0)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    // jkl
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
        corpusAccessApi.corpusAccessDB.docStore.batchImport(segmenter.docStore.asInstanceOf[MemDocZoningApi])

      case Left(message) =>

    }}

    st.run.unsafeRunSync()
  }

  def segmentAllParallel(n: Int=Int.MaxValue, skip: Int=0)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val processStream = TextWorksActions.buildProcessStream(TextWorksConfig.Config(
      IOConfig(
        inputMode = Some(InputMode.CorpusFile(corpusAccessApi.corpus.corpusRoot.toNIO, None))
      )
    ))
    val stN = processStream.drop(skip.toLong)
    val st0 = stN.take(1).map{ _ match {
      case Right(segmenter) =>
        println(s"Importing ${segmenter.stableId} into database.")
        corpusAccessApi.corpusAccessDB.docStore.batchImport(segmenter.docStore.asInstanceOf[MemDocZoningApi])

      case Left(message) =>

    }}
    st0.run.unsafeRunSync()

    val st1 = stN.drop(1).take(n).map{ _ match {
      case Right(segmenter) =>
        println(s"Importing ${segmenter.stableId} into database.")
        corpusAccessApi.corpusAccessDB.docStore.batchImport(segmenter.docStore.asInstanceOf[MemDocZoningApi])

      case Left(message) =>

    }}

    st0.run.unsafeRunSync()
    // val chunked = entries.drop(1)
    //   .through(pipe.zipWithIndex)
    //   .chunkN(1, allowFewer=true)
    //   .map { chunks =>
    //     chunks.map(Stream.chunk)
    //       .reduce(_ ++ _)
    //       .covary[IO]
    //       .evalMap { case (corpusEntry, i) =>
    //         IO {
    //           println(s"processing entry ${i}")
    //           segment(corpusEntry, commitToDb=true)
    //           println(s"done entry ${i}")
    //         }
    //       }
    //   }
  }

  def segmentEntry(stableId: String@@DocumentID, commitToDb: Boolean=false)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val docStore = corpusAccessApi.docStore

    if (commitToDb && docStore.getDocument(stableId).isDefined) {
      println(s"document ${stableId} already exists in database, skipping.")
    } else if (corpusAccessApi.corpus.hasEntry(stableId.unwrap)) {

      println(s"segmenting ${stableId}")

      for {
        corpusEntry    <- corpusAccessApi.corpus.entry(stableId.unwrap)
      } {
        segment(corpusEntry, commitToDb)
      }
    }
  }

  def segment(
    corpusEntry: CorpusEntry,
    commitToDb: Boolean=true
  )(implicit corpusAccessApi: CorpusAccessApi): Option[DocumentSegmentation] = {

    // val docStore = corpusAccessApi.docStore

    // val maybeSegmenter = for {
    //   pdfArtifact    <- corpusEntry.getPdfArtifact
    //   pdfPath        <- pdfArtifact.asPath.toOption
    // } yield {
    //   val stableId = DocumentID(corpusEntry.entryDescriptor)

    //   val traceLogGroup = corpusEntry.ensureArtifactGroup("tracelogs")
    //   traceLogGroup.deleteGroupArtifacts()

    //   if (commitToDb && docStore.getDocument(stableId).isDefined) {
    //     println(s"document ${stableId} already exists in database, skipping.")

    //     None
    //   } else {
    //     println(s"segmenting ${stableId}")
    //     val traceLogs = corpusEntry.ensureArtifactGroup("tracelogs")
    //     val traceLogRoot = traceLogs.rootPath
    //     val textOutputPath = corpusEntry.getRootPath() / "richtext.json"

    //     val segmenter = TextWorksActions.extractText(stableId, pdfPath,
    //       textOutputFile = textOutputPath,
    //       rtreeOutputRoot = None,
    //       traceLogRoot = Some(traceLogRoot)
    //     )
    //     val memZoningApi = segmenter.docStore.asInstanceOf[MemDocZoningApi]
    //     // val memZoneApi = new MemDocZoningApi

    //     // val segmenter = DocumentSegmenter
    //     //   .createSegmenter(stableId, pdfPath, memZoneApi)

    //     // segmenter.runDocumentSegmentation()


    //     if (commitToDb) {
    //       println(s"Importing ${stableId} into database.")
    //       corpusAccessApi.corpusAccessDB.docStore.batchImport(memZoningApi)
    //       println(s"Done importing ${stableId}")
    //     } else {
    //       println(s"DB Importing disabled.")
    //     }

    //     println()
    //     Some(segmenter)
    //   }
    // }
    // maybeSegmenter.flatten
    ???
  }



}
