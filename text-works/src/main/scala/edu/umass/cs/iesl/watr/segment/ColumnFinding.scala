package edu.umass.cs.iesl.watr
package segment

import spindex._

// import ammonite.{ops => fs}, fs._
import watrmarks.{StandardLabels => LB, _}

import geometry._
import geometry.syntax._

import utils.{RelativeDirection => Dir}
import TypeTags._
import utils.ExactFloats._
import utils.SlicingAndDicing._
// import utils.{Debugging => Dbg}

import org.dianahep.{histogrammar => HST}
// import org.dianahep.histogrammar.ascii._


trait ColumnFinding extends PageScopeSegmenter { self =>
  lazy val columnFinder = self


  def runColumnFinder(): Unit = {
  }

  def findCandidateWhitespaceCols(): Unit = {
    implicit val log = traceLog.createLog("findCandidateWhitespaceCols")

    val components = mpageIndex.getPageAtoms(pageNum)

    val cols = findLeftAlignedCharCols(components)

    traceLog.drawPageGeometry()

    traceLog.flashComponents(s"Left-aligned Char Cols", cols)

    cols.foreach { colRegion =>
      val colBounds = colRegion.bounds
      pageIndex.removeComponent(colRegion)
      showComponentRemoval(s"Removing Left-aligned Col", Seq(colRegion))

      val startingRegion = LTBounds(
        left   = colBounds.left-0.1d,
        top    = colBounds.top,
        width  = 0.01.toFloatExact(),
        height = colBounds.height
      )

      growToMaxEmptySpace(startingRegion)
        .foreach{ emptyRegion =>
          val colIsWideEnough = emptyRegion.width > 4.0d

          traceLog.showMorph(s"Expanding ${startingRegion.prettyPrint} To Max=${emptyRegion.prettyPrint}", startingRegion, emptyRegion)


          if (colIsWideEnough) {
            val expandedRegion = labelRegion(emptyRegion, LB.WhitespaceColCandidate)

            traceLog.flashComponents("Creating Candidate", Seq(expandedRegion))

          }
        }
    }

    showLabeledComponents(s"Final WS Col Candidates", LB.WhitespaceColCandidate)


  }

  def findLeftAlignedCharCols(
    components: Seq[AtomicComponent]
  ): Seq[RegionComponent] = {
    import HST._
    val componentLefts = HST.SparselyBin.ing(1.0, {x: AtomicComponent => x.bounds.left.asDouble()} named "char-lefts")

    components.foreach { componentLefts.fill(_) }

    // Construct a horizontal query, looking to boost scores of "runs" of consecutive left-x-value
    val queryBoxes = componentLefts.bins.toList
      .sortBy { case (bin, counting) => counting.entries }
      .reverse.take(10) //  only consider the 10 tallest cols
      .map{ case (bin, counting) =>
        val bw = componentLefts.binWidth

        LTBounds.Doubles(
          left   = bw * bin,
          top    = 0d,
          width  = bw,
          height = pageGeometry.height.asDouble()
        )
      }

    val res: List[Option[RegionComponent]] =
      queryBoxes.flatMap { query =>
        val intersects = pageIndex.searchIntersecting(query, LB.PageAtom)

        val consecutiveLeftAlignedCharCols =
          intersects.sortBy(_.bounds.bottom)
            .groupByPairs((c1, c2) => c1.bounds.bottom == c2.bounds.bottom)
            .map(_.sortBy(_.bounds.left).head)
            .groupByPairs((c1, c2) => c1.bounds.left == c2.bounds.left)
            .filter{ groups => groups.length > 1 }

        consecutiveLeftAlignedCharCols
          .map{ ccs => mpageIndex.labelRegion(ccs, LB.LeftAlignedCharCol).map(_._1) }


      }

    res.flatten
  }



  def growToMaxEmptySpace(startingRegion: LTBounds): Option[LTBounds] = {

    var currWhiteSpace = startingRegion
    val nudgeFactor = 0.05.toFloatExact()

    def search(q:LTBounds) = pageIndex.searchIntersecting(q, LB.PageAtom, LB.Image, LB.VLinePath, LB.HLinePath, LB.HPageDivider)

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
      hlineCC <- pageIndex.searchOverlapping(textBlock, LB.HLinePath)
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
        pageIndex.searchIntersecting(horizontalStripeRegion, (l0 +: labels):_*)
          .sortBy(_.bounds.left)
          .filterNot(_.id == cc.id)
          .span(_.bounds.left < cc.bounds.left)
      }

    // currWhiteSpace.withinRegion(pageBounds).adjacentRegion(Dir.Left)

  }

  def combineCandidateWhitespaceCols(): Unit = {

    implicit val log = tracer.createLog("combineCandidateWhitespaceCols")

    import scala.collection.mutable

    val candidateCCs = pageIndex.getComponentsWithLabel(LB.WhitespaceColCandidate).sortBy(_.bounds.area).reverse

    // val candidateBounds = candidateCCs.map(_.bounds()).sortBy(_.area).reverse

    val candidates = mutable.ArrayBuffer[Component](candidateCCs:_*)
    // val candidates = mutable.ArrayBuffer[LTBounds](candidateBounds)

    traceLog.flashComponents("Whitespace Col Candidates", candidateCCs)

    while(candidates.nonEmpty) {
      val candidate = candidates.head
      var currColBounds = candidate.bounds()


      val overlaps = pageIndex.searchIntersecting(currColBounds, LB.WhitespaceColCandidate)
        .filterNot { _.id.unwrap == candidate.id.unwrap }

      traceLog.flashComponents("Query Column + Overlaps", candidate +: overlaps)

      overlaps.foreach { overlappedCC =>

        if (overlappedCC.bounds.isContainedBy(candidate.bounds)){
          pageIndex.removeComponent(overlappedCC)
          candidates -= overlappedCC

        } else if (overlappedCC.bounds.isContainedByVProjection(candidate.bounds)) {
          val adjacency = overlappedCC.bounds().withinRegion(candidate.bounds())
          val overlappingVertStripe = adjacency.adjacentRegions(Dir.Top, Dir.Center, Dir.Bottom)
          val vstripe = overlappingVertStripe.getOrElse { sys.error("no overlapping area") }

        }


      }

      pageIndex.removeComponent(candidate)
    }
  }


}






  // def combineCandidateWhitespaceColsV0(): Unit = {

  //   implicit val log = tracer.createLog("combineCandidateWhitespaceCols")

  //   var candidates = pageIndex.getComponentsWithLabel(LB.WhitespaceColCandidate)

  //   flashComponents("Whitespace Col Candidates", candidates)

  //   while(candidates.nonEmpty) {
  //     val candidate = candidates.head

  //     val overlaps = pageIndex.rtreeSearch(candidate.bounds, LB.WhitespaceColCandidate)
  //       .filterNot { _.id.unwrap == candidate.id.unwrap }

  //     flashComponents("Target Cols", Seq(candidate))
  //     flashComponents("Overlapped Cols", overlaps)

  //     overlaps.headOption match {
  //       case Some(overlap) =>
  //         // "Burst" the overlapping regions into all constituent parts
  //         val obbox = overlap.bounds
  //         val cbbox = candidate.bounds

  //         val (maybeIntersect, burstRegions) = obbox.withinRegion(cbbox).burstAllAdjacent()

  //         val allRegions = maybeIntersect.map(_ +: burstRegions).getOrElse(burstRegions)

  //         allRegions.foreach { bbox =>
  //           // Discard very small columns
  //           if (bbox.width > 1.0 && bbox.height > 1.0) {
  //             // micro-shrink the bbox to prevent it from overlapping its neighbors
  //             val LTBounds(x, y, w, h) = bbox
  //             val shrunk = LTBounds(
  //               x+0.01.toFloatExact,
  //               y+0.01.toFloatExact,
  //               w-0.02.toFloatExact,
  //               h-0.02.toFloatExact
  //             )
  //             if (shrunk.area >= 0) {
  //               labelRegion(shrunk, LB.WhitespaceColCandidate)
  //             } else {
  //               // println(s"combineCandidateWhitespaceCols: area <= 0 for ${shrunk} was ${bbox}")
  //             }
  //           }
  //         }

  //         pageIndex.removeComponent(candidate)
  //         pageIndex.removeComponent(overlap)
  //       case None =>
  //         pageIndex.removeComponent(candidate)
  //         labelRegion(candidate.bounds(), LB.WhitespaceCol)
  //     }

  //     candidates = pageIndex.getComponentsWithLabel(LB.WhitespaceColCandidate)
  //   }
  // }
