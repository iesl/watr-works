package edu.umass.cs.iesl.watr
package table

import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
import corpora._
import corpora.filesys._
import corpora.database._
import segment._
// import geometry.syntax._
// import ammonite.{ops => fs}, fs._

import TypeTags._

object ShellCommands extends DocumentZoningApiEnrichments {

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
      } {
        segment(corpusEntry, commitToDb, writeRTrees)
      }
    }
  }

  def segment(
    corpusEntry: CorpusEntry,
    commitToDb: Boolean=true,
    writeRTrees: Boolean=false
  )(implicit corpusAccessApi: CorpusAccessApi): Option[DocumentSegmenter] = {

    val docStore = corpusAccessApi.docStore

    val maybeSegmenter = for {
      pdfArtifact    <- corpusEntry.getPdfArtifact
      pdfPath        <- pdfArtifact.asPath.toOption
    } yield {
      val stableId = DocumentID(corpusEntry.entryDescriptor)

      if (commitToDb && docStore.getDocument(stableId).isDefined) {
        println(s"document ${stableId} already exists in database, skipping.")

        None
      } else {
        println(s"segmenting ${stableId}")

        val memZoneApi = new MemDocZoningApi

        val segmenter = DocumentSegmenter
          .createSegmenter(stableId, pdfPath, memZoneApi)

        segmenter.runDocumentSegmentation()

        if (writeRTrees) {
          println("Writing RTrees")
          val rtreeGroup = corpusEntry.ensureArtifactGroup("rtrees")
          for {
            pageNum <- segmenter.mpageIndex.getPages
          } {
            val pageIndex = segmenter.mpageIndex.getPageIndex(pageNum)
            val bytes = pageIndex.saveToBytes()
            rtreeGroup.putArtifactBytes(pageIndex.rtreeArtifactName, bytes)
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
        Some(segmenter)
      }
    }
    maybeSegmenter.flatten
  }

  // import watrmarks._
  // import play.api.libs.json, json._


  def addUserAndLockTables(db: CorpusAccessDB)(): Unit = {
    db.runqOnce{ db.tables.UserTables.create.run }
    db.runqOnce{ db.tables.workflowTables.create.run }
  }



  // def reorderVisualLines(n: Int, skip: Int)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
  //   import watrmarks.{StandardLabels => LB}
  //   val docStore = corpusAccessApi.docStore


  //   for {
  //     stableId <- docStore.documents(n, skip)
  //     corpusEntry    <- corpusAccessApi.corpus.entry(stableId.unwrap)
  //   } {
  //     println(s"processing entry ${stableId}")
  //     val documentSegmenter = segment(corpusEntry, commitToDb=false, writeRTrees=false)
  //     println("segmented")
  //     documentSegmenter.foreach { segmenter =>
  //       val stableId = segmenter.stableId
  //       val mpageIndex = segmenter.mpageIndex

  //       var nextOrder = 0
  //       println("computing reading order")
  //       for {
  //         pageNum      <- mpageIndex.getPages
  //         pageIndex    <- List(mpageIndex.getPageIndex(pageNum))
  //         readingOrder <- pageIndex.getClusters(LB.ReadingOrder)
  //       }  {
  //         val visualLineZones = docStore.getPageVisualLines(stableId, pageNum)
  //         println(s"aligning reading order for ${visualLineZones.length} lines")

  //         val vlineCCsToZones = readingOrder
  //           .map{ vlineCC =>
  //             val maybeZone = visualLineZones.find { zone =>
  //               zone.regions.exists { zregion =>
  //                 vlineCC.bounds.isWithin(zregion.bbox)
  //               }
  //             }
  //             (vlineCC, maybeZone)
  //           }

  //         println(s"Corrected reading order for ${stableId} page ${pageNum}")

  //         vlineCCsToZones.foreach { case (vlineCC, maybeZone) =>
  //           val maybeText = pageIndex.components.getComponentText(vlineCC, LB.VisualLine)
  //           maybeZone match {
  //             case Some(zone) =>
  //               val reflow = docStore.getModelTextReflowForZone(zone.id)

  //               val rtext = reflow.map { tr =>
  //                 tr.astext
  //               } getOrElse("<no text>")

  //               val newText = maybeText.map(_.toText()).getOrElse("<notext>")

  //               println(s"""${nextOrder}>>> ${newText}""")
  //               if (rtext.trim() != newText.trim()) {
  //                 println(s"""  was> ${rtext}""")
  //               }

  //               // db.setZoneOrder(zone.id, nextOrder)
  //               nextOrder = nextOrder + 1
  //             case None =>
  //               println(s"""!!no matching zone> ${maybeText.map(_.toText()).getOrElse("<notext>")}""")
  //           }
  //         }

  //       }
  //     }

  //   }


  // }

}
