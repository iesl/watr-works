package edu.umass.cs.iesl.watr
package segment

import spindex._
// import scalaz.{@@ => _, _}, Scalaz._
import utils.SlicingAndDicing._

import ammonite.{ops => fs}, fs._
// import java.io.InputStream
// import scala.collection.JavaConversions._
// import textboxing.{TextBoxing => TB}, TB._
import watrmarks.{StandardLabels => LB}

import geometry._
import geometry.syntax._

// import textreflow.data._

// import ComponentOperations._
// import PageComponentImplicits._

// import utils.{CompassDirection => CDir, _}
// import tracing.VisualTracer._
// import EnrichNumerics._
// import SlicingAndDicing._
import TypeTags._
// import corpora._
// import extract.PdfTextExtractor
import utils.ExactFloats._

class RTreePageSegmentation(
  val mpageIndex: MultiPageIndex
) {

  val docStore = mpageIndex.docStore
  val stableId = mpageIndex.getStableId
  val docId = docStore.getDocument(stableId)
    .getOrElse(sys.error(s"DocumentSegmenter trying to access non-existent document ${stableId}"))

  def runPageSegmentation(): Unit = {
    runLineDetermination()
  }

  def runLineDetermination(): Unit = {
    val pageRegions = for {
      (pageId, pagenum) <- docStore.getPages(docId).zipWithIndex
    } yield {

      println(s"Page ${pagenum} id=${pageId}")
      runLineDeterminationOnPage(pageId, PageNum(pagenum))

      val pageGeometry = docStore.getPageGeometry(pageId)
      docStore.getTargetRegion(
        docStore.addTargetRegion(pageId, pageGeometry)
      ).toPageRegion()
    }

    docStore.labelRegions(LB.DocumentPages, pageRegions)
  }

  def runLineDeterminationOnPage(pageId: Int@@PageID, pageNum: Int@@PageNum): Unit = {
    val atomicComponents = mpageIndex.getPageAtoms(pageNum)

    determineLines(pageId, pageNum, atomicComponents)
  }
  import images.{ImageManipulation => IM}

  def determineLines(
    pageId: Int@@PageID,
    pageNum: Int@@PageNum,
    components: Seq[AtomicComponent]
  ): Unit = {

    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val rTreeIndex = pageIndex.componentIndex

    approximateLineBins(components)


    val pageBounds = pageIndex.getPageGeometry.bounds
    val pageCanvas = IM.createCanvas(pageBounds)


    for {
      hashedLines <- pageIndex.labelToComponents.get(LBP.LineByHash)
      hashedLine <- hashedLines
      cc <- rTreeIndex.get(hashedLine.unwrap)
    } {
      // Split up lines into strictly non-overlapping regions
      val intersects = rTreeIndex.search(cc.bounds, {cc =>
        cc.roleLabel == LBP.LineByHash
      })


      if (intersects.length > 1) {
        val totalBounds = intersects.map(_.bounds).reduce(_ union _)
        val charsInRegion = rTreeIndex.search(totalBounds, {cc =>
          cc.roleLabel == LB.PageAtom
        })
        // Remove the LineByHash regions
        // iterate over chars left-to-right and group them into non-overlaps and overlaps
        val allChars = charsInRegion.sortBy(_.bounds.left)

        allChars.groupByPairs { (c1, c2) =>
          c1.bounds.bottom == c2.bounds.bottom
        }.foreach{ ccs =>
          mpageIndex.labelRegion(ccs, LBP.LineByHash)
        }

        intersects.foreach { cc =>
          mpageIndex.removeComponent(cc)
        }
      }

    }

    val overlays = rTreeIndex.getItems
      .filter { c => c.roleLabel == LBP.LineByHash }
      .map { c => IM.ltBoundsToDrawables(c.bounds, pageIndex.getPageGeometry, pageBounds) }

    val embossedCanvas = pageCanvas.draw(overlays.flatten)

    val bytes = embossedCanvas.image.bytes
    val outPath = fs.pwd / RelPath(new java.io.File("treevis.png"))
    if (fs.exists(outPath)) {
      fs.rm(outPath)
    }
    fs.write(outPath, bytes)


  }


  // First approximation for text line-groups
  def approximateLineBins(charBoxes: Seq[AtomicComponent]): Unit = {
    charBoxes
      .groupBy{ _.bounds.bottom.unwrap }
      .toSeq
      .map { case (bottomY, charBoxes) =>
        charBoxes.sortBy(_.bounds.left)
      }.foreach{ lineBin =>
        mpageIndex.labelRegion(lineBin, LBP.LineByHash)
      }


  }


}

object LBP {
  import watrmarks.Label

  val LineByHash = Label("LineByHash")

}
