package edu.umass.cs.iesl.watr
package segment

import edu.umass.cs.iesl.watr.corpora.DocumentZoningApi
import spindex._

import ammonite.{ops => fs}, fs._
import watrmarks.{StandardLabels => LB, _}

import geometry._
import geometry.syntax._

import utils.{RelativeDirection => Dir}
import TypeTags._
import utils.ExactFloats._
import textgrid._

// import org.dianahep.histogrammar.ascii._


object PageSegmenter {

  def getVisualLinesInReadingOrder(pageIndex: PageIndex): Seq[(Component, Seq[Component])] = {
    val linesPerBlock0 = for {
      block <- pageIndex.getOrdering(LB.ReadingBlocks)
    } yield {
      for {
        visualLineRootCCs <- pageIndex.getRelations(block, LB.HasVisualLines).toList
        visualLineRootCC <- visualLineRootCCs
        lineMembers   <- pageIndex.getClusterMembers(LB.ReadingBlockLines, visualLineRootCC)
      } yield {
        (block, lineMembers)
      }
    }

    val linesPerBlock = linesPerBlock0.flatten

    linesPerBlock
  }

}

trait SegmentationCommons {
  def modalValue(ccs: Seq[Component], f: Component => Int): Option[Int] = {
    ccs.groupBy{f(_)}.toSeq
      .sortBy({ case (_, atoms) => atoms.length })
      .reverse.headOption.map(_._1)
  }

}

trait PageLevelFunctions extends tracing.VisualTracer with SegmentLogging{
  def pageId: Int@@PageID
  def pageNum: Int@@PageNum
  def pageIndex: PageIndex
  def mpageIndex: MultiPageIndex

  def pageGeometry = docStore.getPageGeometry(pageId)

  def docStore: DocumentZoningApi

  def labelRegion(bbox: LTBounds, label: Label, text: Option[String] = None): RegionComponent = {
    val regionId = docStore.addTargetRegion(pageId, bbox)
    val pageRegion = docStore.getTargetRegion(regionId)
    mpageIndex.createRegionComponent(pageRegion, label, text)
  }


  def combineCandidateWhitespaceCols(): Unit = {

    implicit val log = tracer.createLog("combineCandidateWhitespaceCols")

    var candidates = pageIndex.getComponentsWithLabel(LB.WhitespaceColCandidate)

    flashComponents("Whitespace Col Candidates", candidates)

    while(candidates.nonEmpty) {
      val candidate = candidates.head

      val overlaps = pageIndex.rtreeSearch(candidate.bounds, LB.WhitespaceColCandidate)
        .filterNot { _.id.unwrap == candidate.id.unwrap }

      flashComponents("Target Cols", Seq(candidate))
      flashComponents("Overlapped Cols", overlaps)

      overlaps.headOption match {
        case Some(overlap) =>
          // "Burst" the overlapping regions into all constituent parts
          val obbox = overlap.bounds
          val cbbox = candidate.bounds

          val (maybeIntersect, burstRegions) = obbox.withinRegion(cbbox).burstAllAdjacent()

          val allRegions = maybeIntersect.map(_ +: burstRegions).getOrElse(burstRegions)

          allRegions.foreach { bbox =>
            // Discard very small columns
            if (bbox.width > 1.0 && bbox.height > 1.0) {
              // micro-shrink the bbox to prevent it from overlapping its neighbors
              val LTBounds(x, y, w, h) = bbox
              val shrunk = LTBounds(
                x+0.01.toFloatExact,
                y+0.01.toFloatExact,
                w-0.02.toFloatExact,
                h-0.02.toFloatExact
              )
              if (shrunk.area >= 0) {
                labelRegion(shrunk, LB.WhitespaceColCandidate)
              } else {
                // println(s"combineCandidateWhitespaceCols: area <= 0 for ${shrunk} was ${bbox}")
              }
            }
          }

          pageIndex.removeComponent(candidate)
          pageIndex.removeComponent(overlap)
        case None =>
          pageIndex.removeComponent(candidate)
          labelRegion(candidate.bounds(), LB.WhitespaceCol)
      }

      candidates = pageIndex.getComponentsWithLabel(LB.WhitespaceColCandidate)
    }
  }



  def growToMaxEmptySpace(startingRegion: LTBounds): Option[LTBounds] = {

    var currWhiteSpace = startingRegion
    val nudgeFactor = 0.05.toFloatExact()

    def search(q:LTBounds) = pageIndex.rtreeSearch(q, LB.PageAtom, LB.Image, LB.VLinePath, LB.HLinePath, LB.HPageDivider)

    currWhiteSpace.withinRegion(pageGeometry).adjacentRegion(Dir.Left)
      .foreach { regionLeftOf =>
        // println(s"querying left of ${currWhiteSpace}: ${regionLeftOf}")
        val atomsLeftOf = search(regionLeftOf)

        if (atomsLeftOf.nonEmpty) {
          val rightMostCC = atomsLeftOf.maxBy(_.bounds.right)
          for {
            right <- regionLeftOf.splitVertical(rightMostCC.bounds.right+nudgeFactor)._2
          } {
            currWhiteSpace = currWhiteSpace union right
          }
        }
      }

    currWhiteSpace.withinRegion(pageGeometry).adjacentRegion(Dir.Top)
      .foreach { regionAbove =>
        // println(s"querying above ${currWhiteSpace}: ${regionAbove}")
        val atomsAbove = search(regionAbove)

        if (atomsAbove.nonEmpty) {
          val bottomMostCC = atomsAbove.maxBy(_.bounds.bottom)
          for {
            bottom <- regionAbove.splitHorizontal(bottomMostCC.bounds.bottom+nudgeFactor)._2
          } { currWhiteSpace = currWhiteSpace union bottom }
        } else {
          currWhiteSpace = currWhiteSpace union regionAbove
        }
      }

    currWhiteSpace.withinRegion(pageGeometry).adjacentRegion(Dir.Bottom)
      .foreach { regionBelow =>
        // println(s"querying below ${currWhiteSpace}: ${regionBelow}")
        val atomsBelow = search(regionBelow)

        if (atomsBelow.nonEmpty) {
          val topmostCC = atomsBelow.minBy(_.bounds.top)
          for {
            top <- regionBelow.splitHorizontal(topmostCC.bounds.top-nudgeFactor)._1
          } {
            currWhiteSpace = currWhiteSpace union top
          }
        } else {
          currWhiteSpace = currWhiteSpace union regionBelow
        }
      }

    Some(currWhiteSpace)

  }

  // Try to detect and possibly rewrite text that is represented as path objects
  def rewritePathObjects(orderedTextBlocks: Seq[LTBounds]): Unit = {
    val _ = for {
      textBlock <- orderedTextBlocks
      hlineCC <- pageIndex.rtreeSearchOverlapping(textBlock, LB.HLinePath)
    } yield {
      pageIndex.removeComponent(hlineCC)

      val width = hlineCC.bounds.width

      if (width.asDouble() < 10d) {
        labelRegion(hlineCC.bounds, LB.PageAtom, Some("—"))
      } else {

        leftRightContext(hlineCC, textBlock, LB.PageAtom, LB.HLinePath) match {
          case Some((lefts, rights)) =>
            if (lefts.isEmpty && rights.isEmpty) {
              labelRegion(hlineCC.bounds, LB.HPageDivider)
            } else {
              labelRegion(hlineCC.bounds, LB.HLinePath)
            }

          case None =>
            labelRegion(hlineCC.bounds, LB.HPageDivider)
        }
      }
    }
  }

  def leftRightContext(
    cc: Component,
    queryRegion: LTBounds,
    l0: Label,
    labels: Label*
  ): Option[(Seq[Component], Seq[Component])] = {

    cc.bounds.withinRegion(queryRegion)
      .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
      .map { horizontalStripeRegion =>
        pageIndex.rtreeSearch(horizontalStripeRegion, l0, labels:_*)
          .sortBy(_.bounds.left)
          .filterNot(_.id == cc.id)
          .span(_.bounds.left < cc.bounds.left)
      }

    // currWhiteSpace.withinRegion(pageBounds).adjacentRegion(Dir.Left)

  }
}

class PageSegmenter(
  override val pageId: Int@@PageID,
  override val pageNum: Int@@PageNum,
  documentSegmenter: DocumentSegmenter
) extends SegmentationCommons with PageLevelFunctions with tracing.VisualTracer {

  val mpageIndex = documentSegmenter.mpageIndex
  val docStats = documentSegmenter.docStats

  val docStore = mpageIndex.docStore
  val stableId = mpageIndex.getStableId
  val docId = docStore.getDocument(stableId)
    .getOrElse(sys.error(s"DocumentSegmenter trying to access non-existent document ${stableId}"))
  val pageStats = docStats.addPage(pageNum)

  val pageIndex = mpageIndex.getPageIndex(pageNum)

  val segvisRootPath = pwd / s"${stableId}-segs.d"

  def runLineSegmentation(): Unit = {
    tracer.enter()

    labelImages()

    runLineDeterminationOnPage()

    buildLinePairTrapezoids()

    tracer.exit()
  }

  def runLineClassification(): Unit = {
    val lineClassifier = new LineGroupClassifier(mpageIndex, pageId, pageNum, tracer)
    lineClassifier.classifyLines()

    setPageText()
  }

  private def labelImages(): Unit = {
    mpageIndex.getImageAtoms(pageNum).foreach { imgCC =>
      mpageIndex.labelRegion(Seq(imgCC), LB.Image)
    }
  }

  private def runLineDeterminationOnPage(): Unit = {
    tracer.enter()

    val lineFinder = new LineFinder(mpageIndex, pageId, pageNum)
    lineFinder.determineLines()


    tracer.exit()
  }


  private def buildLinePairTrapezoids(): Unit = {
    tracer.enter()

    // pageIndex.reportClusters()

    for {
      (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex).toList
      linePair <- lineCCs.sliding(2)
    }  {

      // construct trapezoids: isosceles, right, rectangular
      linePair match {
        case Seq(l1, l2) =>
          val ml1Text = pageIndex.getComponentText(l1, LB.VisualLine)
          val ml2Text = pageIndex.getComponentText(l2, LB.VisualLine)


          (ml1Text, ml2Text) match {
            case (Some(l1Text), Some(l2Text)) =>

              pageIndex.getRelations(l1, LB.VisualLineModal)
              val l1VisLineModal = pageIndex.getRelations(l1, LB.VisualLineModal).head.head
              val l2VisLineModal = pageIndex.getRelations(l2, LB.VisualLineModal).head.head

              val l1Baseline = l1VisLineModal.bounds().toLine(Dir.Bottom)
              val l2Baseline = l2VisLineModal.bounds().toLine(Dir.Bottom)

              val t = Trapezoid.fromHorizontals(l1Baseline, l2Baseline)

              pageStats.trapezoidHeights.fill(t)
              pageStats.leftAcuteBaseAngles.fill(t)
              pageStats.leftObtuseBaseAngles.fill(t)
              docStats.trapezoidHeights.fill(t)
              docStats.leftAcuteBaseAngles.fill(t)
              docStats.leftObtuseBaseAngles.fill(t)

              pageIndex.setAttribute[Trapezoid](l1.id, watrmarks.Label("Trapezoid"), t)

              Option(t)

            case _ => None
          }
        case Seq(l1) => None
        case Seq() => None
      }
    }



    tracer.exit()
  }


  def setPageText(): Unit = {
    for {
      pageNum      <- mpageIndex.getPages
      pageIndex    <- List(mpageIndex.getPageIndex(pageNum))
    }  {
      val textLines = for {
        (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex)
        (line, n)    <- lineCCs.zipWithIndex
        textRow      <- pageIndex.getComponentText(line, LB.VisualLine).toList
      } yield textRow

      val pageTextGrid = TextGrid.fromRows(textLines)

      docStore.setPageText(pageId, pageTextGrid)
    }
  }





  def deleteComponentsWithLabel(l: Label): Unit = {
    pageIndex.getComponentsWithLabel(l)
      .foreach { cc =>
        pageIndex.removeComponent(cc)
      }
  }












}
