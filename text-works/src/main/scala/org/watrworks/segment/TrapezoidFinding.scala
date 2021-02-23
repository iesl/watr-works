package org.watrworks
package segment

import geometry._
import geometry.syntax._
import watrmarks._
import TypeTags._
import org.dianahep.{histogrammar => HST}

trait ShapeFunctions extends TrapezoidFinding with LineShapeClassification { self =>
  lazy val shapeFunctions = self
}

trait TrapezoidFinding extends PageScopeSegmenter { self =>

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

    shapeAndCharsAndScaledFontId.sliding(2).foreach {
      _ match {
        case Seq(l1, l2) =>
          val (line1ReprShape, line1Chars) = l1
          val (line2ReprShape, line2Chars) = l2

          val areVertical = line1ReprShape.shape.isStrictlyAbove(line2ReprShape.shape)
          val areOverlappingHorizontal =
            line1ReprShape.shape.isNeitherLeftNorRightOf(line2ReprShape.shape)

          if (areVertical && areOverlappingHorizontal) {

            val l1Baseline = line1ReprShape.shape.toLine(Dir.Bottom)
            val l2Baseline = line2ReprShape.shape.toLine(Dir.Bottom)

            val trapezoid = Trapezoid.fromHorizontals(l1Baseline, l2Baseline)
            pageScope.accumulateShapeStats(trapezoid)

            // TODO this should be more like a relation or grouping, not an attribute of line-repr-shape
            line1ReprShape.setAttr(LB.LinePairTrapezoid, trapezoid)

            val linePairTrap = indexShape(trapezoid, LB.LinePairTrapezoid)
            // TODO these should be specified in a feature extraction method
            val isUpLeftCapCase = line1Chars.headOption
              .map(_.char.matches("^[A-Z]"))
              .getOrElse(false)

            val isLowRightCharPeriod = line2Chars.lastOption
              .map(_.char.matches("[.]"))
              .getOrElse(false)

            linePairTrap.setAttr(LB.UpLeftCharCapCase, isUpLeftCapCase)
            linePairTrap.setAttr(LB.UpLeftCharCapCase, isLowRightCharPeriod)

            traceLog.trace {
              shape(linePairTrap)
            }
          }
        case _           => None
      }
    }
  }

}

import smile.clustering.{
  kmeans
  // XMeans
}
import com.spotify.featran._
import com.spotify.featran.transformers._

case class TrapezoidFeatureRec(
  trapezoid: Trapezoid
)

object TrapezoidFeatures extends RectangularCuts {

  val spec = FeatureSpec
    .of[TrapezoidFeatureRec]
    .required(_.trapezoid.leftBaseAngle())(Identity("LBaseAngle"))

}

trait TrapezoidAnalysis extends DocumentScopeSegmenter { self =>

  // import com.spotify.featran.converters._

  import TrapezoidFeatures._

  def createFeatureVectors(): Unit = {

    // Get all Trapezoids across pages
    val allTraps = pageSegmenters.flatMap({ p =>
      p.getLabeledTraps(LB.LinePairTrapezoid)
    })

    // allTraps.foreach(t => println(t.shape.prettyPrint()))
    val records = allTraps.map(t => TrapezoidFeatureRec(t.shape)).toList

    val baseAngleHisto = HST.SparselyBin.ing(
      0.1,
      { t: Trapezoid => t.leftBaseAngle() }
    )

    allTraps.foreach(t => baseAngleHisto.fill(t.shape))

    val f1: FeatureExtractor[List, TrapezoidFeatureRec] =
      spec.extract(records)

    val featureVectors = f1.featureValues[Array[Double]].toArray
    pprint.pprintln("Ffeature vectors")
    pprint.pprintln(featureVectors)

    // val clusters: XMeans = xmeans(featureVectors, 4)
    val clusters = kmeans(featureVectors, 4)

    pprint.pprintln(clusters)

    clusters.centroids.foreach(centroid => {
      val c0 = centroid.head
      val lldeg = (c0 * 180) / math.Pi
      pprint.pprintln(s"Centroid = ${lldeg}  (${c0} rad)")
    })

    val lbAngles = baseAngleHisto // docStats.leftAcuteBaseAngles
    lbAngles.bins.toList
      .foreach { case (bin, counting) =>
        val binWidth = lbAngles.binWidth
        val binLeft = bin * binWidth
        val binLeftDeg = (binLeft * 180) / math.Pi
        val binRight = (bin + 1) * binWidth
        val entries = counting.entries
        println(s"${binLeft}(${binLeftDeg}) - ${binRight}  : ${entries}")
      }
  }
}
