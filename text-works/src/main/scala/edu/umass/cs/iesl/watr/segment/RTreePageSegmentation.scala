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
import org.dianahep.histogrammar._
// import org.dianahep.histogrammar.ascii._

// import textreflow.data._

// import ComponentOperations._
// import PageComponentImplicits._

import utils.{CompassDirection => CDir}
// import tracing.VisualTracer._
// import EnrichNumerics._
// import SlicingAndDicing._

import TypeTags._
// import corpora._
// import extract.PdfTextExtractor
import utils.ExactFloats._
import images.{ImageManipulation => IM}


class RTreePageSegmentation(
  val mpageIndex: MultiPageIndex
) {

  val docStore = mpageIndex.docStore
  val stableId = mpageIndex.getStableId
  val docId = docStore.getDocument(stableId)
    .getOrElse(sys.error(s"DocumentSegmenter trying to access non-existent document ${stableId}"))

  lazy val pageIdMap: Map[Int@@PageID, Int@@PageNum] =
    docStore.getPages(docId).zipWithIndex.map{
      case (pageId, pageNum) => (pageId, PageNum(pageNum))
    }.toMap

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

  def writeRTreeImage(
    pageNum: Int@@PageNum,
    name: String
  ): Unit = {
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val pageBounds = pageIndex.getPageGeometry.bounds
    val pageCanvas = IM.createCanvas(pageBounds)
    val rTreeIndex = pageIndex.componentIndex

    val overlays = rTreeIndex.getItems
      .filter { c =>
       (c.roleLabel == LBP.LineByHash
         || c.roleLabel == LBP.ColByHash
         || c.roleLabel == LBP.WhitespaceCol
       )
      }
      .map { c => IM.ltBoundsToDrawables(c.bounds, pageIndex.getPageGeometry, pageBounds) }

    val embossedCanvas = pageCanvas.draw(overlays.flatten)

    val bytes = embossedCanvas.image.bytes
    val outPath = fs.pwd / RelPath(new java.io.File(s"${name}.png"))
    if (fs.exists(outPath)) {
      fs.rm(outPath)
    }
    fs.write(outPath, bytes)

  }

  def runLineDeterminationOnPage(pageId: Int@@PageID, pageNum: Int@@PageNum): Unit = {
    val atomicComponents = mpageIndex.getPageAtoms(pageNum)

    determineLines(pageId, pageNum, atomicComponents)
  }


  def determineLines(
    pageId: Int@@PageID,
    pageNum: Int@@PageNum,
    components: Seq[AtomicComponent]
  ): Unit = {


    approximateLineBins(components)
    writeRTreeImage(pageNum, "01-lineHashing")

    splitLinesWithOverlaps(pageNum)
    writeRTreeImage(pageNum, "02-splitLineHashing")

    maybeSplitColumns(pageId, components)

    writeRTreeImage(pageNum, "03-columnHashing")

    splitLinesOnWhitespaceColumns(pageNum)

    writeRTreeImage(pageNum, "04-splitOnCols")

    // Construct super/sub queries

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


  def maybeSplitColumns(
    pageId: Int@@PageID,
    components: Seq[AtomicComponent]
  ): Unit = {
    // find char-based left-edge histogram
    val pageNum = pageIdMap(pageId)
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val rTreeIndex = pageIndex.componentIndex

    val componentLefts = SparselyBin.ing(1.0, {x: AtomicComponent => x.bounds.left.asDouble()} named "char-lefts")

    components.foreach { i =>
      componentLefts.fill(i)
    }

    val pageGeometry = docStore.getPageGeometry(pageId)
    println(s"PageGeometry: ${pageGeometry}")

    // Construct a horizontal query, looking to boost scores of "runs" of consecutive left-x-value
    val hQueries = componentLefts.bins.toList
      .sortBy { case (bin, counting) => counting.entries }
      .reverse
      .take(10)
      .map{ case (bin, counting) =>
        val bw = componentLefts.binWidth

        val hQuery = LTBounds.Doubles(
          left   = bw * bin,
          top    = 0d,
          width  = bw,
          height = pageGeometry.height.asDouble()
        )
        hQuery
      }


    // TODO Drop hQueries w/left edges == page left edge

    hQueries.foreach { query =>
      println(s"querying ${query}")
      val intersects = rTreeIndex.search(query, {cc =>
        cc.roleLabel == LB.PageAtom
      })

      println(s"  hits: ${intersects.length}")
      val leftMostComponentsInQuery =
        intersects.sortBy(_.bounds.bottom)
          .groupByPairs((c1, c2) => c1.bounds.bottom == c2.bounds.bottom)
          .map(_.sortBy(_.bounds.left).head)
          .groupByPairs((c1, c2) => c1.bounds.left == c2.bounds.left)
          .filter{ groups => groups.length > 1 }
          .map{ ccs =>
            val chars = ccs.map(_.chars).mkString
            println(s"   chars: ${chars}")
            mpageIndex.labelRegion(ccs, LBP.ColByHash)
              .foreach { case (regionCC, targetRegion) =>
                val colBounds = targetRegion.bbox
                val startingRegion = LTBounds(
                  left   = colBounds.left-0.1d,
                  top    = colBounds.top,
                  width  = 0.1.toFloatExact(),
                  height = colBounds.height
                )
                println(s"   col-by-hash: ${colBounds}, starting ws: ${startingRegion}")

                findMaxEmptyRegion(pageId, startingRegion).foreach { maxRegion =>

                }

              }
          }

      println(s"filtered groups count = ${leftMostComponentsInQuery.length}")

    }
    // componentLefts.println
  }

  def findMaxEmptyRegion(
    pageId: Int@@PageID,
    startingRegion: LTBounds
  ): Option[RegionComponent] = {

    val pageNum = pageIdMap(pageId)
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val pageBounds = pageIndex.getPageGeometry.bounds
    val rTreeIndex = pageIndex.componentIndex

    var currWhiteSpace = startingRegion

    currWhiteSpace.adjacentRegionWithin(pageBounds, CDir.W)
      .foreach { regionLeftOf =>
        println(s"querying left of ${currWhiteSpace}: ${regionLeftOf}")
        val atomsLeftOf = rTreeIndex.search(regionLeftOf, {cc =>
          cc.roleLabel == LB.PageAtom
        })

        if (atomsLeftOf.nonEmpty) {
          val rightMostCC = atomsLeftOf.maxBy(_.bounds.right)
          regionLeftOf.splitVertical(rightMostCC.bounds.right)
            .foreach { case (left, right) =>
              currWhiteSpace = currWhiteSpace union right
            }
        }
      }

    currWhiteSpace.adjacentRegionWithin(pageBounds, CDir.N)
      .foreach { regionAbove =>
        println(s"querying above ${currWhiteSpace}: ${regionAbove}")
        val atomsAbove = rTreeIndex.search(regionAbove, {cc =>
          cc.roleLabel == LB.PageAtom
        })

        if (atomsAbove.nonEmpty) {
          val bottomMostCC = atomsAbove.maxBy(_.bounds.bottom)
          regionAbove.splitHorizontal(bottomMostCC.bounds.bottom)
            .foreach { case (top, bottom) =>
              currWhiteSpace = currWhiteSpace union bottom
            }
        }
      }

    currWhiteSpace.adjacentRegionWithin(pageBounds, CDir.S)
      .foreach { regionBelow =>
        println(s"querying below ${currWhiteSpace}: ${regionBelow}")
        val atomsBelow = rTreeIndex.search(regionBelow, {cc =>
          cc.roleLabel == LB.PageAtom
        })

        if (atomsBelow.nonEmpty) {
          val topmostCC = atomsBelow.minBy(_.bounds.top)
          regionBelow.splitHorizontal(topmostCC.bounds.top)
            .foreach { case (top, bottom) =>
              currWhiteSpace = currWhiteSpace union top
            }
        }
      }


    println(s"Creating WhitespaceCol : ${currWhiteSpace}")
    val regionId = docStore.addTargetRegion(pageId, currWhiteSpace)
    val targetRegion = docStore.getTargetRegion(regionId)
    val pageRegion = PageRegion(targetRegion.page, targetRegion.bbox)
    val regionComp = mpageIndex.createRegionComponent(pageRegion, LBP.WhitespaceCol)
    Some(regionComp)
  }

  def splitLinesWithOverlaps(pageNum: Int@@PageNum): Unit = {
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val rTreeIndex = pageIndex.componentIndex
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
  }

  def splitLinesOnWhitespaceColumns(pageNum: Int@@PageNum): Unit = {
    val pageIndex = mpageIndex.getPageIndex(pageNum)
    val rTreeIndex = pageIndex.componentIndex
    for {
      colRegions <- pageIndex.labelToComponents.get(LBP.WhitespaceCol)
      colRegionId <- colRegions
      colRegion <- rTreeIndex.get(colRegionId.unwrap)
      intersectedLine <- rTreeIndex.search(colRegion.bounds, {_.roleLabel == LBP.LineByHash})
    } {

      val charsInRegion = rTreeIndex.search(intersectedLine.bounds, {cc =>
        cc.roleLabel == LB.PageAtom
      })
      val allChars = charsInRegion.sortBy(_.bounds.left)

      val (leftSplit, rightSplit) = allChars.span(_.bounds.left < colRegion.bounds.left)

      mpageIndex.labelRegion(leftSplit, LBP.LineByHash)
      mpageIndex.labelRegion(rightSplit, LBP.LineByHash)

      mpageIndex.removeComponent(intersectedLine)
    }

  }


}

object LBP {
  import watrmarks.Label

  val LineByHash = Label("LineByHash")
  val ColByHash = Label("ColByHash")
  val WhitespaceCol = Label("WhitespaceCol")

}
