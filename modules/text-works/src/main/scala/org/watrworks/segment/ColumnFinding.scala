package org.watrworks
package segment

import geometry._
// import geometry.syntax._
// import TypeTags._

import utils.ExactFloats._
// import utils.SlicingAndDicing._

// import org.dianahep.{histogrammar => HST}
// import utils.FunctionalHelpers._
// import watrmarks._

sealed trait ColumnEvidence

object ColumnEvidence {
  case class TwoColumn(
    pageBand: Rect,
    c1FontId: String @@ ScaledFontID,
    c1Left: Int @@ FloatRep,
    c1Right: Int @@ FloatRep,
    c2FontId: String @@ ScaledFontID,
    c2Left: Int @@ FloatRep,
    c2Right: Int @@ FloatRep
  ) extends ColumnEvidence

}

trait ColumnFinding extends BasePageSegmenter with LineSegmentation { self =>

  lazy val columnFinder = self

  def findColumnEvidence(): Unit = {}

}
