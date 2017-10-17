package edu.umass.cs.iesl.watr
package segment

import ammonite.{ops => fs}, fs._


import segment.{SegmentationLabels => LB}
import corpora.DocumentZoningApi
import extract._
import spindex._

import utils.SlicingAndDicing._
// import utils.ExactFloats._
import TypeTags._

trait DocumentLevelFunctions extends DocumentScopeSegmenter

trait DocumentSegmentation extends DocumentLevelFunctions { self =>


  protected[segment] def init(): Unit = {
    initPageIndexes()
    // initStartingComponents()
  }

  def getNumberedPages(): Seq[(Int@@PageID, Int@@PageNum)] =
    docStore.getPages(docId).zipWithIndex.map {
      case (a, b) => (a, PageNum(b))
    }

  def getNumberedPageIndexes(): Seq[(Int@@PageID, PageIndex)] =
    docStore.getPages(docId).zipWithIndex.map {
      case (a, b) => (a, mpageIndex.getPageIndex(PageNum(b)))
    }

  private def initPageIndexes(): Unit = {
    val pageRegions = pageAtomsAndGeometry
      .map { case(extractedItems, pageGeometry)  =>
        val pageId = docStore.addPage(docId, pageGeometry.pageNum)
        docStore.setPageGeometry(pageId, pageGeometry.bounds)
        val tr = docStore.getTargetRegion(
          docStore.addTargetRegion(pageId, pageGeometry.bounds)
        )
        tr
      }

    createZone(LB.DocumentPages, pageRegions)
  }

  lazy val pageSegmenters = {

    def createPageSegmenters(): Seq[PageSegmenter] = for {
      (pageId, pageNum) <- getNumberedPages()
    } yield PageSegmenter(pageId, pageNum, self)

    createPageSegmenters()
  }

  import textgrid._

  protected def joinPageTextGrids(): TextGrid = {
    val textGrids = pageSegmenters.map {
      _.getPageTextGrid()
    }

    val allRows = textGrids.map(_.rows).flatten

    TextGrid.fromRows(allRows)
  }

  def runDocumentSegmentation(): Unit = {

    pageSegmenters.foreach {
      _.runPageSegmentationPass1()
    }

    doCharMetricComputations()

    pageSegmenters.foreach {
      _.runPageSegmentationPass2()
    }

    // val docTextGrid = joinPageTextGrids()
    // val outputBuilder = docTextGrid.buildOutput()
    // val finalOutput = outputBuilder.getOutput

    // println(finalOutput)

  }


  private def doCharMetricComputations(): Unit = {

    docScope.fontDefs.fontProperties.foreach{ fontProps =>
      println("Font properties")
      println(fontProps)
      // val tristr = fontProps.trigramEvidence. mkString("{\n  ", "\n  ", "\n}")
      val bistr = fontProps.bigramEvidence. mkString("{  ", ", ", "  }")
      println(s"Bigrams: ${bistr} ")

      val pageEvidence = fontProps.pagewiseEvidence. mkString("{\n  ", "\n  ", "\n}")
      println("PageEvidence: ")
      println(pageEvidence)

      val _ = fontProps.inferredMetrics()
    }


  }
}


trait DocumentSegmenter extends DocumentSegmentation

object DocumentSegmenter {
  import spindex._

  def createSegmenter(
    stableId0: String@@DocumentID,
    pdfPath: Path,
    docStore0: DocumentZoningApi
  ): DocumentSegmentation = {

    val (pages, fontDefs0) = PdfBoxExtractorMain.extractPages(stableId0, pdfPath)

    val segmenter = new DocumentSegmentation {
      override val pageAtomsAndGeometry = pages
      override val fontDefs = fontDefs0
      override val mpageIndex: MultiPageIndex = new MultiPageIndex(stableId0, docStore0, pages)

      override val docStore: DocumentZoningApi = docStore0
      override val stableId = stableId0
      override val docId = docStore0.addDocument(stableId0)

      override val docStats: DocumentLayoutStats = new DocumentLayoutStats()

    }

    segmenter.init()
    segmenter
  }
}
