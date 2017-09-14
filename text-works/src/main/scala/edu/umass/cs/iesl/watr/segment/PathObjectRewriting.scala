package edu.umass.cs.iesl.watr
package segment

import spindex._

import watrmarks.{StandardLabels => LB, _}

import geometry._
import geometry.syntax._

import utils.{RelativeDirection => Dir}
import TypeTags._
import utils.ExactFloats._

trait PathObjectRewriting extends PageScopeSegmenter {

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
    labels: Label*
  ): Option[(Seq[Component], Seq[Component])] = {

    cc.bounds.withinRegion(queryRegion)
      .adjacentRegions(Dir.Left, Dir.Center, Dir.Right)
      .map { horizontalStripeRegion =>
        pageIndex.searchIntersecting(horizontalStripeRegion, labels:_*)
          .sortBy(_.bounds.left)
          .filterNot(_.id == cc.id)
          .span(_.bounds.left < cc.bounds.left)
      }

    // currWhiteSpace.withinRegion(pageBounds).adjacentRegion(Dir.Left)

  }
}
