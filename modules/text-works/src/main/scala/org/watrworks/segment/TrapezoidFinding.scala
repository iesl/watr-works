package org.watrworks
package segment

import geometry._
import geometry.syntax._
import TypeTags._
import org.dianahep.{histogrammar => HST}
import utils.Interval
import Interval._

trait ShapeFunctions extends TrapezoidFinding with LineShapeClassification { self =>
  lazy val shapeFunctions = self
}

trait TrapezoidFinding extends BasePageSegmenter { self =>

  def buildLinePairTrapezoids(): Unit = {
    val lineReprShapes = getLabeledRects(LB.BaselineMidriseBand)

    // .map(l => (l, getCharsForShape(l)))
    val shapeAndCharsAndScaledFontId = lineReprShapes
      .map(l => (l, l.getAttr(ExtractedChars).getOrElse(Seq())))
      .sortBy { case (_, baselineChars) =>
        baselineChars.head.id
      }

    // shapeAndCharsAndScaledFontId.foreach { case (lineReprShape, _) =>
    //   setWeightsForShape(lineReprShape, WeightedLabeling())
    // }

    shapeAndCharsAndScaledFontId.sliding(2).foreach {
      _ match {
        case Seq(l1, l2) =>
          val (line1ReprShape, line1Chars@_) = l1
          val (line2ReprShape, line2Chars@_) = l2

          val areVertical = line1ReprShape.shape.isStrictlyAbove(line2ReprShape.shape)
          val areOverlappingHorizontal =
            line1ReprShape.shape.isNeitherLeftNorRightOf(line2ReprShape.shape)

          if (areVertical && areOverlappingHorizontal) {

            val l1Baseline = line1ReprShape.shape.toLine(M3.Bottom)
            val l2Baseline = line2ReprShape.shape.toLine(M3.Bottom)

            val trapezoid = Trapezoid.fromHorizontals(l1Baseline, l2Baseline)
            pageScope.accumulateShapeStats(trapezoid)

            // TODO this should be more like a relation or grouping, not an attribute of line-repr-shape
            // line1ReprShape.setAttr(LB.LinePairTrapezoid, trapezoid)

            val linePairTrap = indexShape(trapezoid, LB.LinePairTrapezoid)
            // // TODO these should be specified in a feature extraction method
            // val isUpLeftCapCase = line1Chars.headOption
            //   .map(_.char.matches("^[A-Z]"))
            //   .getOrElse(false)

            // val isLowRightCharPeriod = line2Chars.lastOption
            //   .map(_.char.matches("[.]"))
            //   .getOrElse(false)

            // linePairTrap.setAttr(LB.UpLeftChar, line1Chars.head.char)
            // linePairTrap.setAttr(LB.LowRightChar, line2Chars.last.char)

            // linePairTrap.setAttr(LB.UpLeftCharCapCase, isUpLeftCapCase)
            // linePairTrap.setAttr(LB.LowRightCharPeriod, isLowRightCharPeriod)

            // val tl = trapezoid.topLeft
            // val tr = tl.translate(trapezoid.topWidth, 0.toFloatExact())
            // val bl = trapezoid.bottomLeft
            // val th = trapezoid.height()

            // val br = bl.translate(trapezoid.bottomWidth, 0.toFloatExact())
            // val fmt = (p: Point) => s"${p.x.pp()},${p.y.pp()}"
            // val fmtx = (p: Point) => s"${p.x.pp()}x${p.y.pp()}"

            // val geom = pageGeometry.toPoint(M3.BottomRight)
            // val crop = s"${geom.x.pp()}x${th.pp()}+0+${tl.y.pp()}"
            // val crop = s"${geom.x.pp()}x${(th + 14.toFloatExact()).pp()}+0+${(tl.y - 7.toFloatExact()).pp()}"

            // println(s"'${crop}' '${fmtx(geom)}' '${fmt(tl)} ${fmt(tr)} ${fmt(br)} ${fmt(bl)}'")

            traceLog.trace {
              shape(linePairTrap)
            }
          }
        case _ => None
      }
    }
  }
}

// import smile.clustering.{
//   kmeans
//   // XMeans
// }
import com.spotify.featran._
import com.spotify.featran.{transformers => ft}
// import com.spotify.featran.converters._

import TypeTags._

trait TrapezoidPagewiseAnalysis extends BasePageSegmenter { self =>

  def createFeatureSpec(): FeatureSpec[TrapShape] = {
    val spec = FeatureSpec
      .of[TrapShape]
      .required(_.shape.leftBaseAngle())(ft.Identity("LBaseAngle"))
      // .required(_.trapezoid.leftBaseAngle())(Identity("1em-Height"))
      // .required(_.trapezoid.leftBaseAngle())(Identity("1em-Height-TopLine"))
      .required(
        _.getAttr(UpLeftChar)
          .map(x => x.matches("[A-Z]"))
          .map(x => { if (true) 1.0d else 0.0d })
          .getOrElse(0.0)
      )(ft.Identity("UpLeftCharCapCase"))
    // .required(_.trapezoid.leftBaseAngle())(Identity("TopLeftNumeric"))

    spec
  }
}

trait TrapezoidAnalysis extends BaseDocumentSegmenter { self =>

  def createClusteringLabels(clustering: List[List[AnyShape]]): Unit = {}

  def createFeatureVectors(): Unit = {
    // val allTrapsFeatures = pageSegmenters.flatMap({ p =>
    //   val pageTraps                             = p.getLabeledTraps(LB.LinePairTrapezoid)
    //   val spec                                  = p.createFeatureSpec()
    //   val f: FeatureExtractor[Array, TrapShape] = spec.extract(pageTraps.toArray)
    //   f.featureValues[Array[Double]]
    // })

    // Get all Trapezoids across pages
    val allTraps = pageSegmenters.flatMap(
      _.getLabeledTraps(LB.LinePairTrapezoid)
    )

    val lbAngles = HST.SparselyBin.ing(
      0.1,
      { t: Trapezoid => t.leftBaseAngle() }
    )

    allTraps.foreach(t => lbAngles.fill(t.shape))

    val lbAngleClustering = lbAngles.bins.toList.foldLeft(
      ShapeClustering.init("LBAngleBins")
    ) {
      case (acc, (bin, counting @ _)) => {
        val binWidth    = lbAngles.binWidth
        val binLeft     = bin * binWidth
        val binInterval = Interval.DblBeginLen(binLeft, binWidth)
        // val binLeftDeg = ((binLeft * 180) / math.Pi).toFloatExact().pp()
        // val binRight = (bin + 1) * binWidth
        // val binRightDeg = ((binRight * 180) / math.Pi).toFloatExact().pp()
        // val binDegs = s"${binLeftDeg}° - ${binRightDeg}°"
        // val entries = counting.entries

        val binInstances = allTraps.filter(t =>
          binInterval.contains(
            t.shape.leftBaseAngle()
          )
        )

        val ids = binInstances.map(_.id)

        acc.addCluster(ids)
      }
    }

    val ms          = shapeIndexes.view.map(shapeIndex => shapeIndex._2.shapeMap)
    val combinedMap = ms.foldLeft(Map[Long, AnyShape]()) { case (acc, e) => acc ++ e.toMap }
    val shapeMap    = combinedMap.view.map({ case (k, v) => ShapeID(k.toInt) -> v }).toMap

    val clustering = lbAngleClustering
      .build(shapeMap)

    val clusteringLabel = ShapeClustering.toTranscriptLabel(clustering)
    docTraceLogs.trace(TraceLog(body = List(clusteringLabel)))

    // val records = allTraps.map(t => TrapezoidFeatureRec(t)).toList

    // val f1: FeatureExtractor[List, TrapezoidFeatureRec] =
    //   spec.extract(records)

    // val featureVectors = f1.featureValues[Array[Double]].toArray
    // // pprint.pprintln("Feature vectors")
    // // pprint.pprintln(featureVectors)

    // // TODO what normalized left-base trapezoid angle gives
    // //      the best approximation of a paragraph start across all documents ?
    // // val clusters: XMeans = xmeans(featureVectors, 4)
    // val clusters = kmeans(featureVectors, 4)
    // pprint.pprintln(clusters)

    // clusters.centroids.foreach(centroid => {
    //   val c0 = centroid.head
    //   val lldeg = (c0 * 180) / math.Pi

    //   pprint.pprintln(s"Centroid = ${lldeg}  (${c0} rad)")
    // })

  }
}
