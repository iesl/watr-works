package org.watrworks
package segment

import geometry._
import geometry.syntax._
import textgrid._
import watrmarks._
import TypeTags._

trait ShapeFunctions extends TrapezoidFinding with LineShapeClassification { self =>
  lazy val shapeFunctions = self

}

trait TrapezoidFinding extends PageScopeSegmenter { self =>

  lazy val docStats = docScope.docStats

  def buildLinePairTrapezoids(): Unit = {

    val lineReprShapes = getLabeledRects(LB.BaselineMidriseBand)
    val shapeAndCharsAndScaledFontId = lineReprShapes
      .map(l => (l, getCharsForShape(l)))
      .sortBy { case (_, baselineChars) =>
        baselineChars.head.id
      }

    shapeAndCharsAndScaledFontId.foreach { case (lineReprShape, _) =>
      setWeightsForShape(lineReprShape, WeightedLabeling())
    }

    shapeAndCharsAndScaledFontId.sliding(2).foreach { case linePair =>
      linePair match {
        case Seq(l1, l2) =>
          val (line1ReprShape, line1Chars) = l1
          val (line2ReprShape, line2Chars) = l2

          val areVertical = line1ReprShape.shape.isStrictlyAbove(line2ReprShape.shape)
          val areOverlappingHorizontal = line1ReprShape.shape.isNeitherLeftNorRightOf(line2ReprShape.shape)

          if (areVertical && areOverlappingHorizontal) {

            val l1Baseline = line1ReprShape.shape.toLine(Dir.Bottom)
            val l2Baseline = line2ReprShape.shape.toLine(Dir.Bottom)

            val trapezoid = Trapezoid.fromHorizontals(l1Baseline, l2Baseline)

            pageStats.trapezoidHeights.fill(trapezoid)
            pageStats.leftAcuteBaseAngles.fill(trapezoid)
            pageStats.leftObtuseBaseAngles.fill(trapezoid)

            docStats.trapezoidHeights.fill(trapezoid)
            docStats.leftAcuteBaseAngles.fill(trapezoid)
            docStats.leftObtuseBaseAngles.fill(trapezoid)

            setTrapezoidForShape(line1ReprShape, trapezoid)

            traceLog.traceAll {

              val trap = initShape(trapezoid, LB.LinePairTrapezoid)
              val rel = relation("TrapezoidLinePairs")
                .field(trap)
                .field(line1ReprShape)
                .field(line2ReprShape)

              List(shape(trap), rel)
            }
          }
        case _ => None
      }
    }

  }

}
