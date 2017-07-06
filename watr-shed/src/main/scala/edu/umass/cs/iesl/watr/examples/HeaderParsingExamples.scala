package edu.umass.cs.iesl.watr
package examples


import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}

import corpora._
import corpora.database.CorpusAccessDB
import corpora.database.CorpusAccessDBTables
import watrmarks.{Label, StandardLabels => LB}
import heuristics.GenericHeuristics._
import heuristics.{GenHeuristics => GH}
// import heuristics.AuthorNameHeuristics._
// import heuristics.AffiliationsHeuristics._
// import heuristics.Utils._
import textreflow.data._
import corpora.filesys._
import ammonite.{ops => fs}, fs._
import segment.DocumentSegmenter
import TypeTags._

// import scala.collection.mutable.ListBuffer

class HeaderParsingExamples(dbName: String) {
  val textReflowDBTables = new CorpusAccessDBTables

  lazy val textReflowDB = new CorpusAccessDB(tables = textReflowDBTables, dbname = dbName, dbuser = "watrworker", dbpass = "watrpasswd")
  lazy val docStore: DocumentZoningApi = textReflowDB.docStore
  lazy val corpus = Corpus(pwd / "corpus-test")

  def authorNameSegmentationVariation(targetDocumentStableId: String, targetLabel: Label) = {

    def write(s: String): Unit = {
      println(s)
    }
    val stableId = DocumentID(targetDocumentStableId)

    for {
      corpusEntry    <- corpus.entry(targetDocumentStableId)
      pdfArtifact    <- corpusEntry.getPdfArtifact
      pdfPath        <- pdfArtifact.asPath.toOption
    } {
      val mdocStore = new MemDocZoningApi
      val segmenter = DocumentSegmenter.createSegmenter(stableId, pdfPath, mdocStore)

      segmenter.runPageSegmentation()

      for {
        docId <- mdocStore.getDocument(stableId)
        _ <- Option[Unit]{
          println(s"Applying labeled areas to newly-extracted paper")
          for {
            dbDocId <- docStore.getDocument(stableId)
            dbZone <- docStore.getDocumentZones(dbDocId, targetLabel)
          } {
            val pageRegions = dbZone.regions.map(_.toPageRegion())
            mdocStore.labelRegions(targetLabel, pageRegions)
          }
        }
        zone <- mdocStore.getDocumentZones(docId, targetLabel)
        targetRegion <- zone.regions
        targetRegionTextReflow <- mdocStore.getTextReflowForTargetRegion(targetRegion.id)
      } {
        println("Document: \t\t\t\t\t" + stableId)
        write("Document: \t\t\t\t\t" + stableId + "\n")
        write("Text Reflow: \t\t\t\t" + targetRegionTextReflow.toText() + "\n")
        val tokenizedNames = GH.tokenizeTextReflow(targetRegionTextReflow)

        println(prettyPrintTree(targetRegionTextReflow))

        tokenizedNames.foreach{ n =>
          println(s"Name: ${n.toText}")
        }

        // if (tokenizedNames.nonEmpty) {
        //   write("Tokenized: \t\t\t\t\t" + tokenizedNames + "\n")
        //   val separateAuthorNamesByText = getSeparateAuthorNamesByText(tokenizedNames)
        //   if (separateAuthorNamesByText.nonEmpty) {
        //     write("Text Separated: \t\t\t" + separateAuthorNamesByText + "\n")
        //     val separateAuthorNamesByGeometry = getSeparateComponentsByGeometry(separateAuthorNamesByText, targetRegionTextReflow.get)
        //     write("Geometrically Separated: \t" + separateAuthorNamesByGeometry + "\n")
        //     val separateAuthorNameComponents = separateAuthorNamesByGeometry.map {
        //       authorName => {
        //         getSeparateAuthorNameComponents(authorName)
        //       }
        //     }
        //     var nameIndex = 0
        //     while (nameIndex < separateAuthorNamesByGeometry.length) {
        //       write("Bounding Box info: \t\t\t" + getBoundingBoxesForComponents(separateAuthorNameComponents(nameIndex), separateAuthorNamesByGeometry(nameIndex), textReflow = targetRegionTextReflow.get).toString + "\n")
        //       nameIndex += 1
        //     }
        //   }
        // }

      }
      write("------------------------------------------------------------------------------------------------\n")
      // outputFileWriter.close()
    }

  }

}


object HeaderParsingExamples extends App with utils.AppMainBasics {
  val argMap = argsToMap(args)
  println(argMap)

  val dbname = argMap.get("db").flatMap(_.headOption)
    .getOrElse(sys.error("no db supplied (--db ...)"))

  val stableId = "10.1101-090498.d"
  val entry =  argMap.get("entry").flatMap(_.headOption)
    .getOrElse(stableId)

  val dbCorpus = new HeaderParsingExamples(dbname)
  dbCorpus.authorNameSegmentationVariation(entry, LB.Authors)
}
