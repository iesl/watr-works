package edu.umass.cs.iesl.watr
package segment

import spindex._

import watrmarks.{StandardLabels => LB, _}
import geometry._
import geometry.syntax._

import utils.{RelativeDirection => Dir}
import TypeTags._
import utils.ExactFloats._
import utils.SlicingAndDicing._

import org.dianahep.{histogrammar => HST}


trait CharColumnFinding extends PageScopeSegmenter { self =>
  lazy val columnFinder = self

  val colLefts = HST.SparselyBin.ing(1.0, {p: Point => p.x.asDouble()})
  val colRights = HST.SparselyBin.ing(1.0, {p: Point => p.x.asDouble()})

  def markLeftRightColumns(): Unit = {
    colLefts.bins.toList
      .sortBy { case (bin, counting) => counting.entries }
      .filter{  case (bin, counting) => counting.entries > 1d }
      .map{ case (bin, counting) =>
        val bw = colLefts.binWidth

        val colGuess = LTBounds.Doubles(left=bin.toDouble, top=0d, width=bw, pageGeometry.height.asDouble())
        pageIndex.addShape(colGuess, LB.LeftAlignedCharCol)

      }
    colRights.bins.toList
      .sortBy { case (bin, counting) => counting.entries }
      .filter{  case (bin, counting) => counting.entries > 1d }
      .map{ case (bin, counting) =>
        val bw = colRights.binWidth
        val colGuess = LTBounds.Doubles(left=bin.toDouble, top=0d, width=bw, pageGeometry.height.asDouble())
        pageIndex.addShape(colGuess, LB.LeftAlignedColEnd)
      }
  }

  def boundedVerticalLine(bbox: LTBounds, atX: Int@@FloatRep): Line = {
    val LTBounds(x, y, w, h) = bbox
    Line(
      Point(atX, y),
      Point(atX, y+h)
    )
  }

  def runColumnFinder(): Unit = {
    implicit val log = createLog("initialPageComponents")

    for {
      lineStart <- pageIndex.getOrdering(LB.ExtractedLineStarts)
    } {
      val vline = boundedVerticalLine(pageGeometry, lineStart.bounds.left)
      val llPoint = lineStart.bounds.toPoint(Dir.BottomLeft)

      pageIndex.addShape(vline, LB.LineStartHint)
      pageIndex.addShape(llPoint, LB.LineStartEvidence)
      colLefts.fill(llPoint)


    }

    traceLog.drawPageShapes()

  }



}




