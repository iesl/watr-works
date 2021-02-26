package org.watrworks
package segment

import geometry._
import geometry.syntax._
import watrmarks._
import TypeTags._
import org.dianahep.{histogrammar => HST}
import utils.ExactFloats._
import org.watrworks.transcripts.Transcript
import _root_.io.circe, circe._, circe.syntax._
import utils.Interval
import Interval._
import org.watrworks.segment.TraceLog.LabelTraceLog

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

            val tl = trapezoid.topLeft
            val tr = tl.translate(trapezoid.topWidth, 0.toFloatExact())
            val bl = trapezoid.bottomLeft
            val th = trapezoid.height()

            val br = bl.translate(trapezoid.bottomWidth, 0.toFloatExact())
            val fmt = (p: Point) => s"${p.x.pp()},${p.y.pp()}"
            val fmtx = (p: Point) => s"${p.x.pp()}x${p.y.pp()}"
            val geom = pageGeometry.toPoint(Dir.BottomRight)
            // val crop = s"${geom.x.pp()}x${th.pp()}+0+${tl.y.pp()}"
            val crop = s"${geom.x.pp()}x${(th + 14.toFloatExact()).pp()}+0+${(tl.y - 7.toFloatExact()).pp()}"

            // println(s"'${crop}' '${fmtx(geom)}' '${fmt(tl)} ${fmt(tr)} ${fmt(br)} ${fmt(bl)}'")

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
      val pageTraps = p.getLabeledTraps(LB.LinePairTrapezoid)

      val pageTrapLabels = pageTraps.map(trap => {
        // range = Range.uri.document(d).page(p).shape(s);
        val labelId = LabelID(trap.id.unwrap)
        Transcript.Label(
          "Trapezoid",
          Some(labelId),
          range = List(
            // range=doc+page+shape
            Transcript.DocumentRange(at = self.stableId),
            Transcript.PageRange(at = p.pageNum),
            Transcript.GeometryRange("shape", at = trap.shape)
          ),
          props = None,
          children = None
        )

      })

      docTraceLogs.trace(LabelTraceLog(body=pageTrapLabels))
      pageTraps
    })

    // allTraps.foreach(t => println(t.shape.prettyPrint()))
    val records = allTraps.map(t => TrapezoidFeatureRec(t.shape)).toList

    val f1: FeatureExtractor[List, TrapezoidFeatureRec] =
      spec.extract(records)

    val featureVectors = f1.featureValues[Array[Double]].toArray
    // pprint.pprintln("Feature vectors")
    // pprint.pprintln(featureVectors)

    // TODO what normalized left-base trapezoid angle gives
    //      the best approximation of a paragraph start across all documents ?
    // val clusters: XMeans = xmeans(featureVectors, 4)
    val clusters = kmeans(featureVectors, 4)
    pprint.pprintln(clusters)

    clusters.centroids.foreach(centroid => {
      val c0 = centroid.head
      val lldeg = (c0 * 180) / math.Pi

      pprint.pprintln(s"Centroid = ${lldeg}  (${c0} rad)")
    })

    val lbAngles = HST.SparselyBin.ing(
      0.1,
      { t: Trapezoid => t.leftBaseAngle() }
    )

    allTraps.foreach(t => lbAngles.fill(t.shape))
    val binClusters = lbAngles.bins.toList
      .map { case (bin, counting) =>
        val binWidth = lbAngles.binWidth
        val binLeft = bin * binWidth
        val binLeftDeg = ((binLeft * 180) / math.Pi).toFloatExact().pp()
        val binRight = (bin + 1) * binWidth
        val binRightDeg = ((binRight * 180) / math.Pi).toFloatExact().pp()
        val binDegs = s"${binLeftDeg}° - ${binRightDeg}°"
        val entries = counting.entries
        // println(s"${entries}: #${bin} = ${binDegs}")
        val binInterval = Interval.DblBeginLen(binLeft, binWidth)
        val binInstances = allTraps.filter(t =>
          binInterval.contains(
            t.shape.leftBaseAngle()
          )
        )
        val instances = binInstances.map(inst => {
          val lba = inst.shape.leftBaseAngle()
          val lbaDeg = (lba * 180) / math.Pi
          val lbaPP = lbaDeg.toFloatExact().pp()
          Transcript.Label(
            "Instance",
            Some(LabelID(inst.id.unwrap)),
            List(),
            Some(
              Json.obj(
                "leftBaseAngle" := lbaPP
              )
            ),
            None
          )

        })

        Transcript.Label(
          "Bin",
          Some(LabelID(bin.toInt)),
          List(),
          Some(
            Json.obj(
              "bin-num" := bin,
              "bin-deg" := binDegs,
              "entries" := entries
            )
          ),
          Some(instances.toList)
        )
      }

    val binLabel = Transcript.Label(
      "TrapezoidBins",
      Some(LabelID(23)),
      List(
        Transcript.DocumentRange(at = self.stableId)
      ),
      Some(
        Json.obj(
          "bin-width" := lbAngles.binWidth
        )
      ),
      Some(binClusters)
    )

    docTraceLogs.trace(LabelTraceLog(body=List(binLabel)))

  }
}
