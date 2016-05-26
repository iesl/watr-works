package edu.umass.cs.iesl.watr
package docseg

import watrmarks._

import scalaz.@@
import pl.edu.icm.cermine.tools.Histogram
import pl.edu.icm.cermine.tools.DisjointSets
import Bounds._
import Component._
import scala.collection.JavaConversions._
import scala.collection.mutable


case class LineDimensionBins(
  page: Int@@PageID,
  // Seq[(width, widthFrequency), Seq[lines w/width]]
  widthBin: Seq[((Double, Double), Seq[ConnectedComponents])],
  unBinned: Seq[ConnectedComponents]
)

case class PageSegAccumulator(
  pageLines: Seq[Seq[ConnectedComponents]],
  commonLineDimensions: Seq[Point] = Seq(),
  lineDimensionBins: Seq[LineDimensionBins] = Seq()
)

trait DocstrumUtils {


  def approxSortYX(charBoxes: Seq[CharBox]): Seq[CharBox] = {
    charBoxes.sortBy({ c =>
      (c.bbox.top, c.bbox.left)
    })
  }

  def squishb(charBoxes: Seq[CharBox]): String = {
    approxSortYX(charBoxes)
      .map({ cbox => cbox.char })
      .mkString
  }

  def squishc(charBoxes: Seq[CharComponent]): String = {
    squishb(charBoxes.map(_.component))
  }
}

object DocstrumSegmenter extends DocstrumUtils {
  def compareDouble(d1: Double, d2: Double, precision: Double): Int = {
    if (d1.isNaN() || d2.isNaN()) {
      d1.compareTo(d2)
    } else {
      val p:Double = if (precision == 0) 0 else 1

      val i1: Long = math.round(d1 / p);
      val i2: Long = math.round(d2 / p);
      i1.compareTo(i2)
    }
  }

  def compareDoubleWithTolerance(d1: Double, d2: Double, tolerance: Double): Int = {
    if (math.abs(d1 - d2) < tolerance) 0
    else if (d1 < d2) -1
    else 1
  }

  def filterAngle(direction: Double, tolerance: Double): (Double) => Boolean = {
    val filter = angleFilter(direction: Double, tolerance: Double)
      (angle: Double) => filter.matches(angle)
  }

  def angleFilter(direction: Double, tolerance: Double): AngleFilter = {
    val t2 = tolerance / 2
    AngleFilter(direction - t2, direction + t2)
  }

  def histogram(min: Double, max: Double, resolution: Double): Histogram = {
    new Histogram(min, max, resolution)
  }

  def histogram(values: Seq[Double], resolution: Double): Histogram = {
    Histogram.fromValues(values.toList.map(new java.lang.Double(_)), resolution)
  }

  def getMostFrequentValues(in: Seq[Double], resolution: Double): Seq[(Double, Double)] = {
    val hist = histogram(in, resolution)
    hist.iterator.toSeq
      .sortBy(_.getFrequency)
      .reverse
      .takeWhile(_.getFrequency > 0)
      .map{b=>(b.getValue, b.getFrequency)}
  }


  def init(pagedefs: List[(PageChars, PageGeometry)]): (DocstrumSegmenter, PageSegAccumulator) = {
    val zoneIndex = ZoneIndexer.loadSpatialIndices(pagedefs)

    val docstrum = new DocstrumSegmenter(zoneIndex)

    val allPageLines = for {
      pageId <- docstrum.pages.getPages
    } yield {
      docstrum.determineLines(pageId, docstrum.pages.getComponents(pageId))
    }

    val accum = PageSegAccumulator(allPageLines, Seq())
    // get document-wide stats
    val accum2 = docstrum.getDocumentWideStats(accum)
    (docstrum, accum2)
  }

  import TB._
  def segmentPages(pagedefs: List[(PageChars, PageGeometry)]): String = { // Seq[ConnectedComponents]
    val (docstrum, accum) = init(pagedefs)

    val pageZones = for {
      pageId <- docstrum.pages.getPages
    } yield {
      println(s"segmenting page ${pageId}")
      docstrum.determineZones(pageId, accum)
    }

    implicit val initState = Option(CCRenderState(
      numOfPages = pagedefs.length,
      startingPage = PageID(0)
    ))

    val pageBoxes = pageZones.zipWithIndex.map{ case (zones, pagenum) =>
      val pageZones = zones.toList.map({ zone =>
        vjoinTrailSep(sep=",")(renderConnectedComponents(zone):_*)
      })

      initState.foreach(_.advancePage())

      (s"""{"page": ${pagenum},""" %
        """ "lines": [""" %
        indent()(vsep(pageZones)) %
        """]}""")
    }

    val tokenDict = initState.map { state =>
      val tokLines = state.tokens
        .map({case (pg, tok, bb) => s"[${tok},[${pg}, ${bb.compactPrint}]]".box })
        .grouped(10)
        .map(group => hjoin(sep=",")(group:_*))
        .toList

      indent()(vjoinTrailSep(left, ",")(tokLines:_*))
    } getOrElse nullBox


    val op = (
        """{ "pages": [""" %
                indent()(
                  vjoinTrailSep(left, ",")(
                    vjoinTrailSep(left, ",")(pageBoxes:_*)
                  )) %
             "], " %
        """ "ids": [""" %
                 indent()(tokenDict) %
             "]" %
        """}""" %|
        ""
    )

    op.toString()
  }
}

class DocstrumSegmenter(
  val pages: ZoneIndexer
) {
  import DocstrumSegmenter._

  val LB = StandardLabels

  val MAX_ZONES_PER_PAGE = 300;
  val PAGE_MARGIN = 2;
  val ORIENTATION_MARGIN = 0.2d;
  val LINES_PER_PAGE_MARGIN = 100;

  var docOrientation: Double = 0d

  private def findNeighbors(pageId: Int@@PageID, qbox: CharBox): Seq[CharBox] = {
    pages.nearestNChars(pageId, qbox, 12, 15.0f)
  }

  def computeInitialOrientation(): Double = {
    // println(s"computeInitialOrientation")
    val histPeaks = for {
      pageId <- pages.getPages
    } yield {
      val histogram = new Histogram(-Math.PI/2, Math.PI/2, angleHistogramResolution);
      println(s"hist range = ${-Math.PI/2} - ${Math.PI/2}")
      for {
        component <- pages.getComponents(pageId)
      } yield {

        // println(s"neighbors of ${component.char}")
        val neighbors = findNeighbors(pageId, component)
        neighbors
          .foreach({c =>
            val angle = c.bbox.toCenterPoint.angleTo(component.bbox.toCenterPoint)
            val dist = c.bbox.centerDistanceTo(component.bbox)
            println(s"    ${c.char}: ${c.bbox.prettyPrint} -> ${component.bbox.prettyPrint}")
            println(s"        angle=${angle} dist=${dist}")
            histogram.add(angle)
          })
      }
      // Rectangular smoothing window has been replaced with gaussian smoothing window
      histogram.circularGaussianSmooth(angleHistogramSmoothingWindowLength, angleHistogramSmoothingWindowStdDeviation)

      histogram.getPeakValue()
    }

    println(s"""computeInitialOrientation: ${histPeaks.mkString(", ")}""")

    histPeaks.headOption.getOrElse(0.0)
  }


  val withinAngle = filterAngle(docOrientation, math.Pi / 3)


  /** Groups components into text lines. */
  def determineLines(
    pageId: Int@@PageID,
    components: Seq[CharBox]
  ): Seq[ConnectedComponents] = {
    val maxHorizontalDistance: Double = 2.5d
    val maxVerticalDistance: Double = 12.0

    val sets = new DisjointSets[CharBox](components);


    for { component <- components.sortBy(_.bbox.left) } {
      val searchLog = mutable.ArrayBuffer[TB.Box]()
      findNeighbors(pageId, component)
        .foreach({neighbor =>
          val angle = component.bbox.toCenterPoint.angleTo(neighbor.bbox.toCenterPoint)

          val maxWidth = math.max(neighbor.bbox.width, component.bbox.width)
          val dx = neighbor.bbox.toCenterPoint.hdist(component.bbox.toCenterPoint)
          val dy = neighbor.bbox.toCenterPoint.vdist(component.bbox.toCenterPoint)
          val dist = neighbor.bbox.toCenterPoint.dist(component.bbox.toCenterPoint)

          val eastWestDist = component.bbox.toEasternPoint.dist(
            neighbor.bbox.toWesternPoint
          )

          var joinWith = false
          // val maxWidthMult = 2.7
          val maxHWidthMult = 2.7
          val maxAngleWidthMult = 1.0

          if (angle.eqFuzzy(0.01)(0.0) && eastWestDist < maxWidth*maxHWidthMult) {
            sets.union(component, neighbor);
            joinWith = true
          } else if (withinAngle(angle) && dist < maxWidth*maxAngleWidthMult) {
            sets.union(component, neighbor);
            joinWith = true
          }

          { import TB._
            val topsNotEq = component.bbox.top.pp != neighbor.bbox.top.pp
            val angleNotZero = angle.pp != "0.00"
            searchLog.append(
              s"   ${neighbor.char} #${neighbor.id} ${neighbor.bbox.prettyPrint}".box %
              s"""       ${ if (joinWith && topsNotEq) "!join" else if (joinWith) "join" else "" }""" %
              s"       angle:${angle.pp} dx:${dx.pp} dy:${dy.pp}" %
              s"       dist:${dist.pp} e/wi-dist:${eastWestDist.pp}" %
              s"       maxwidth= ${maxWidth} withinAngle=${withinAngle(angle)}"
            )
          }
        })

      // { import TB._
      //   println(
      //     s"'${component.char} #${component.id} ${component.bbox.prettyPrint}".box %
      //     vcat(top)(searchLog.toList)
      //   )
      // }

    }

    val lines = sets.iterator().toSeq.map{
      _.toSeq.sortBy(c => (c.bbox.left, c.bbox.top)).map(new CharComponent(_, docOrientation))
    }


    lines.map{ Component(_, LB.Line) }
  }


  def computeOrientation(lines: Seq[ConnectedComponents]): Double = {
    // Compute weighted mean of line angles
    var valueSum = 0.0;
    var weightSum = 0.0;
    for (line <- lines) {
      valueSum += line.getAngle() * line.getLength();
      weightSum += line.getLength();
    }
    return valueSum / weightSum;
  }

  def charBasedPageBounds(
    pageId: Int@@PageID
  ): LTBounds = {
    val allBboxes = pages.getComponents(pageId).map(_.bbox)

    val minX = allBboxes.map(_.left).min
    val minY = allBboxes.map(_.top).min
    val maxX = allBboxes.map(_.right).max
    val maxY = allBboxes.map(_.bottom).max

    LTBounds(
      minX, minY,
      maxX-minX,
      maxY-minY
    )
  }


  def lineWidthMatches(line: ConnectedComponents, width: Double): Boolean  = {
    line.determineNormalTextBounds.width.eqFuzzy(0.5d)(width)
  }
  def lineHeightMatches(line: ConnectedComponents, height: Double): Boolean  = {
    line.determineNormalTextBounds.height.eqFuzzy(0.5d)(height)
  }

  def lineDimensionsMatch(line: ConnectedComponents, hw: Point): Boolean = {
    lineWidthMatches(line, hw.x) && lineHeightMatches(line, hw.y)
  }

  def debugFormatLine(cc: ConnectedComponents): String = {
    import TB._
    val line = renderConnectedComponents(cc.tokenizeLine())
    s"${cc.bounds.prettyPrint}, r:${cc.bounds.right.pp} b:${cc.bounds.bottom} ${cc.bounds.prettyPrint} > ${hsep(line)}"
  }

  def printPageLineBins(bin: LineDimensionBins, indent: Int=0): Unit = {
    bin.widthBin.foreach{ case (width, lines) =>
      println(" "*indent + s"Lines within width ${width}")
      lines.sortBy(_.bounds.top).foreach{ line =>
        println("  "*indent + s"w:${line.bounds.width.pp}, h:${line.bounds.height.pp} ${line.bounds.prettyPrint} > ${line.tokenizeLine().toText}")
      }
    }
  }

  def printCommonLineBins(lineBins: Seq[LineDimensionBins]): Unit = {
    lineBins.zipWithIndex.toList.foreach{ case (bin, pnum) =>
      println(s"Page $pnum")
      printPageLineBins(bin, 2)
    }
  }


  def findMostFrequentLineDimensions(psegAccum: PageSegAccumulator):  PageSegAccumulator = {
    val allDocumentWidths = for {
      p <- psegAccum.pageLines;
      l <- p
    } yield l.bounds.width

    val topWidths = getMostFrequentValues(allDocumentWidths, 0.2d).toList
    val topNWidths = topWidths.take(7)
    // Common width meaning from largest->smallest:
    //    left/right justified line width
    //    paragraph-starting indented line width
    //    reference width(s), for hanging-indent first line, remaining lines
    //    other l/r justified blocks (abstract, e.g)

    println(s"""common widths = ${topNWidths.mkString(", ")}""")

    // bin each page by line widths
    val commonLineBins = for {
      (plines, pagenum) <- psegAccum.pageLines.zipWithIndex
    } yield {

      val remainingLines = mutable.ListBuffer[ConnectedComponents](plines:_*)
      val widthBins = mutable.ArrayBuffer[((Double, Double), Seq[ConnectedComponents])]()

      topNWidths.foreach{ case (width, wfreq) =>
        val mws = remainingLines.filter(lineWidthMatches(_, width))
        widthBins.append(((width -> wfreq), mws))
        remainingLines --= mws
      }

      LineDimensionBins(PageID(pagenum), widthBins, remainingLines)
    }
    // printCommonLineBins(commonLineBins)

    psegAccum.copy(
      lineDimensionBins = commonLineBins
    )
  }

  def getDocumentWideStats(psegAccum: PageSegAccumulator): PageSegAccumulator = {
    findMostFrequentLineDimensions(psegAccum)
  }

  def determineZones(
    pageId: Int@@PageID,
    psegAccum: PageSegAccumulator
  ): Seq[ConnectedComponents] = {
    val pageLines: Seq[ConnectedComponents] = psegAccum.pageLines(PageID.unwrap(pageId))

    val pageBounds = charBasedPageBounds(pageId)
    val pageCenter = pageBounds.toCenterPoint

    // println(s"page center ${pageCenter.prettyPrint}")

    val lineBins = psegAccum.lineDimensionBins.find(_.page == pageId).get

    val ((mostFrequentWidthDocwide, wfreq), linesWFreq)  = lineBins.widthBin.sortBy(_._1._2).reverse.head

    // divide page-specific most frequent lines into likely columns:
    val colCenters = getMostFrequentValues(linesWFreq.map(_.bounds.toCenterPoint.x) , resolution=0.2d)

    val commonLinesInCols = for {
      (colX, cfreq) <- colCenters
    } yield {
      (colX, linesWFreq.filter({ line => line.bounds.toCenterPoint.x.eqFuzzy(0.4)(colX) }))
    }

    val sortedCommonLines = commonLinesInCols
      .sortBy(_._1)
      .map({ case (colX, colLines) =>
        // look up/down within confines of this column:

        val ySortedLines = colLines.sortBy(_.bounds.top)
        val topLine = ySortedLines.head
        val bottomLine = ySortedLines.last

        // val candidateLines = lineBins.unBinned.filter({cc =>
        val possibleCand = pageLines.diff(ySortedLines)

        val candidateLines = possibleCand.filter({cc =>
          val tlx = topLine.bounds.xProjection
          val ccx = cc.bounds.xProjection
          val topLineOverlaps = tlx.p1.x-0.1 <= ccx.p1.x && ccx.p2.x <= tlx.p2.x+0.1

          // val fmt=debugFormatLine(cc)
          // println(s"""| checking for candidates
          //             |  ${fmt}
          //             |   top: ${topLine.bounds.prettyPrint} / r: ${topLine.bounds.right}
          //             |   bottom: ${bottomLine.bounds.prettyPrint} / r: ${bottomLine.bounds.right}
          //             |
          //             |""".stripMargin)

          // if (fmt.endsWith("electrode assembly.")) {
          //   val tlfmt=debugFormatLine(topLine)
          //   println(s"""| comparing candidates
          //               |    top: ${tlfmt}
          //               |        ${topLine.bounds.prettyPrint} / r: ${topLine.bounds.right}
          //               |    ${fmt}
          //               |       top: ${cc.bounds.prettyPrint} / r: ${cc.bounds.right}
          //               |       bottom: ${cc.bounds.prettyPrint} / r: ${cc.bounds.right}
          //               |""".stripMargin)

          //   debugLineComponentStats(topLine)
          //   println(s"candidate line ccs")
          //   debugLineComponentStats(cc)
          // }

          topLineOverlaps
        }).sortBy(_.bounds.top)


        val candidateLinesAbove = candidateLines.filter({cc =>
          cc.bounds.top < topLine.bounds.top
        })

        val candidateLinesBelow = candidateLines.filter({cc =>
          cc.bounds.top > bottomLine.bounds.top
        })

        val candidateLinesWithin = candidateLines.filter({cc =>
          // val fmt=debugFormatLine(cc)
          // println(s"""| checking for within ${fmt}
          //             | top: ${topLine.bounds.prettyPrint} / r: ${topLine.bounds.right}
          //             | bottom: ${bottomLine.bounds.prettyPrint} / r: ${bottomLine.bounds.right}
          //             |""".stripMargin)
          topLine.bounds.top < cc.bounds.top && cc.bounds.top < bottomLine.bounds.top
        })

        val debugAboveLines = candidateLinesAbove.map({ cc=> renderConnectedComponents(cc) }).mkString("\n")
        val debugWithin = candidateLinesWithin.map({ cc=> renderConnectedComponents(cc) }).mkString("\n")
        val debugMiddle = ySortedLines.map({ cc=> renderConnectedComponents(cc) }).mkString("\n")
        val debugBelowLines = candidateLinesBelow.map({ cc=> renderConnectedComponents(cc) }).mkString("\n")

        println(s"Candidates Above\n${debugAboveLines}\n")
        // println(s"Candidates Within\n${debugWithin}\n")
        // println(s"\n\n${debugMiddle}\n\n")
        // println(s"Candidates below\n${debugBelowLines}\n")


        // find common v-dist within block, grab lines from above/below within that distance

        val vDists = ySortedLines.sliding(2).map({
          case Seq(c1, c2) => c1.bounds.toCenterPoint.vdist(c2.bounds.toCenterPoint)
          case Seq(c1) => 0d
        })

        val topVDists = getMostFrequentValues(vDists.toList, 0.5d).toList
        println(s"""top v-dists ${topVDists.mkString(", ")}""")

        // pad out the distance slightly
        val topVDist = topVDists.head._1 + 0.1d

        val hitsAbove = candidateLinesAbove.filter({cabove =>
          val vdist = topLine.bounds.toCenterPoint.vdist(cabove.bounds.toCenterPoint)
          println(s"checking vdist=${vdist.pp} for ${cabove.toText}")
          vdist <= topVDist
        })

        val hitsBelow = candidateLinesBelow.filter({cbelow =>
          val vdist = bottomLine.bounds.toCenterPoint.vdist(cbelow.bounds.toCenterPoint)
          vdist <= topVDist
        })

        val totalLine =  hitsAbove ++ ySortedLines ++ candidateLinesWithin ++ hitsBelow
        val totalLineSorted = totalLine.sortBy(_.bounds.top)

        Component(totalLineSorted, LB.Block)
      })

    /// return:
    sortedCommonLines



    // column detection
    //   - search paper-wide for most common: line-width,
    //     for most common line widths, find most common ctr-x, most common v-dist to next line
    //   foreach page:
    //     label centered text lines
    //       look up/down foreach centered line to form centered block
    //     foreach line that matches common-width w/common ctr,
    //       look up/down and join w/ lines within reach
    //     foreach un-labeled line on page
    //        try l/r justified block detection (justified l/r)
    //        try center-column detection (justified l/r)
    //        try left-justified block detection



    //     justified l/r cols
    //       map lines => x.(left, ctr, right)
    //         group into matching sets
    //         foreach matching set, look up/down for straggling line



    // lines.foreach{ line =>
    //   val lineCenter = line.bounds.toCenterPoint
    //   println(s"${lineCenter.prettyPrint} cb:${cdist2} pb:${cdist}  ${line.tokenizeLine().toText}")
    // }
    // println(
    // lines.map{ line =>
    //   line.bounds.toCenterPoint.x
    // }.sorted.mkString("\n")
    // )




  }


  // def pageZonesToSortedZones(zones: Seq[Seq[ConnectedComponents]]): Seq[ConnectedComponents] = {
  //   // BxPage page = new BxPage();
  //   // List[BxPage] pages = Lists.newArrayList(origPage.getParent());
  //   // int pageIndex = pages.indexOf(origPage);
  //   // boolean groupped = false;
  //   // if (zones.size() > MAX_ZONES_PER_PAGE && pageIndex >= PAGE_MARGIN
  //   //         && pageIndex < pages.size() - PAGE_MARGIN) {
  //   //     val oneZone:List[ConnectedComponents]  = List[ConnectedComponents]();
  //   //     for (List[ConnectedComponents] zone : zones) {
  //   //         oneZone.addAll(zone);
  //   //     }
  //   //     zones = new ArrayList[List[ConnectedComponents]]();
  //   //     zones.add(oneZone);
  //   //     groupped = true;
  //   // }
  //   val page = for (lines <- zones) yield {
  //     val zLines = mutable.ArrayBuffer[ConnectedComponents]()
  //     // if (groupped) {
  //     //     zone.setLabel(BxZoneLabel.GEN_OTHER);
  //     // }
  //     for (line <- lines) {
  //       zLines.append(line.convertToBxLine());
  //     }

  //     val zSorted = zLines.sortWith({ case (cc1, cc2) =>
  //       cc1.bounds.top < cc2.bounds.top
  //     })


  //     // zone.setLines(zLines);
  //     // BxBoundsBuilder.setBounds(zone);
  //     // page.addZone(zone);
  //     zSorted.toSeq
  //   }

  //   val pccs = page.map({case ccs => Component(ccs, 0d, LB.Zone) })

  //   val sortedZones = sortZonesYX(pccs)

  //   sortedZones.foreach { cc => println(s"zone ${cc.toText}") }
  //   sortedZones
  //   // BxBoundsBuilder.setBounds(page);
  //   // return page;
  // }


  def sortZonesYX(zones: Seq[ConnectedComponents]): Seq[ConnectedComponents]= {

    zones.sortWith({case (cc1, cc2) =>
      val ycmp = compareDouble(cc1.bounds.top, cc2.bounds.top, 0.01)

      val cmp = if (ycmp == 0) {
        compareDouble(cc1.bounds.left, cc2.bounds.left, 0.01)
      } else {
        ycmp
      }

      cmp < 0
    })
  }

  val DEFAULT_SPACE_HIST_RES: Double = 0.5
  val DISTANCE_STEP: Double = 16.0;

  val DEFAULT_ANGLE_HIST_RES : Double = Math.toRadians(0.5);
  val DEFAULT_ANGLE_HIST_SMOOTH_LEN : Double = 0.25 * Math.PI;
  val DEFAULT_ANGLE_HIST_SMOOTH_STDDEV : Double = 0.0625 * Math.PI;
  val DEFAULT_SPACE_HIST_SMOOTH_LEN : Double = 2.5;
  val DEFAULT_SPACE_HIST_SMOOTH_STDDEV : Double = 0.5;
  val DEFAULT_MAX_VERT_COMP_DIST : Double = 0.67;
  val DEFAULT_MIN_LINE_SIZE_SCALE : Double = 0.9;
  val DEFAULT_MAX_LINE_SIZE_SCALE : Double = 2.5;
  val DEFAULT_MIN_HORIZONTAL_DIST : Double = -0.5;
  val DEFAULT_MIN_VERTICAL_DIST : Double = 0.0;
  val DEFAULT_MAX_VERTICAL_DIST : Double = 1.2;
  val DEFAULT_COMP_DIST_CHAR : Double = 3.5;
  val DEFAULT_WORD_DIST : Double = 0.2;
  val DEFAULT_MIN_HORIZONTAL_MERGE_DIST : Double = -3.0;
  val DEFAULT_MAX_VERTICAL_MERGE_DIST : Double = 0.5;
  val DEFAULT_NEIGHBOR_COUNT : Double = 5;

  val DEFAULT_ANGLE_TOLERANCE: Double = Math.PI / 6;



  //     /**
  //      * Angle histogram resolution in radians per bin.
  //      */
  val angleHistogramResolution = DEFAULT_ANGLE_HIST_RES

  //     /**
  //      * Angle histogram smoothing window length in radians.
  //      * Length of angle histogram is equal to pi.
  //      */
      val angleHistogramSmoothingWindowLength: Double = DEFAULT_ANGLE_HIST_SMOOTH_LEN;

  //     /**
  //      * Angle histogram gaussian smoothing window standard deviation in radians.
  //      */
      val angleHistogramSmoothingWindowStdDeviation:Double = DEFAULT_ANGLE_HIST_SMOOTH_STDDEV;

  //     /**
  //      * Spacing histogram resolution per bin.
  //      */
      val spacingHistogramResolution: Double = DEFAULT_SPACE_HIST_RES;

  //     /**
  //      * Spacing histogram smoothing window length.
  //      */
      val spacingHistogramSmoothingWindowLength:Double = DEFAULT_SPACE_HIST_SMOOTH_LEN;

  //     /**
  //      * Spacing histogram gaussian smoothing window standard deviation.
  //      */
      val spacingHistogramSmoothingWindowStdDeviation:Double = DEFAULT_SPACE_HIST_SMOOTH_STDDEV;

  //     /**
  //      * Maximum vertical component distance multiplier used during line
  //      * determination.
  //      *
  //      * Maximum vertical distance between components (characters) that belong
  //      * to the same line is equal to the product of this value and estimated
  //      * between-line spacing.
  //      */
      val maxVerticalComponentDistanceMultiplier:Double = DEFAULT_MAX_VERT_COMP_DIST;

  //     /**
  //      * Minimum line size scale value.
  //      *
  //      * During zone determination (merging lines into zones) line height is
  //      * taken into account. To achieve this, line size scale is estimated and
  //      * limited to range [minLineSizeScale, maxLineSizeScale].
  //      */
      val minLineSizeScale:Double = DEFAULT_MIN_LINE_SIZE_SCALE;

  //     /**
  //      * Maximum line size scale value.
  //      *
  //      * See minLineSizeScale for more information.
  //      */
      val maxLineSizeScale:Double = DEFAULT_MAX_LINE_SIZE_SCALE;

  //     /**
  //      * Minimum horizontal line distance multiplier.
  //      *
  //      * Minimum horizontal distance between lines that belong to the same zone
  //      * is equal to the product of this value and estimated within-line spacing.
  //      */
      val minHorizontalDistanceMultiplier:Double = DEFAULT_MIN_HORIZONTAL_DIST;

  //     /**
  //      * Minimum vertical line distance multiplier.
  //      *
  //      * Minimum vertical distance between lines that belong to the same zone
  //      * is equal to the product of this value and estimated between-line spacing.
  //      */
      val minVerticalDistanceMultiplier:Double = DEFAULT_MIN_VERTICAL_DIST;

  //     /**
  //      * Maximum vertical line distance multiplier.
  //      *
  //      * Maximum vertical distance between lines that belong to the same zone
  //      * is equal to the product of this value and estimated between-line spacing.
  //      */
      val maxVerticalDistanceMultiplier:Double = DEFAULT_MAX_VERTICAL_DIST;

  //     /**
  //      * Component distance character spacing multiplier.
  //      *
  //      * Maximum distance between components that belong to the same line is
  //      * equal to (lineSpacing * componentDistanceLineMultiplier +
  //      * characterSpacing * componentDistanceCharacterMultiplier), where
  //      * lineSpacing and characterSpacing are estimated between-line and
  //      * within-line spacing, respectively.
  //      */
      val componentDistanceCharacterMultiplier:Double = DEFAULT_COMP_DIST_CHAR;

  //     /**
  //      * Word distance multiplier.
  //      *
  //      * Maximum distance between components that belong to the same word is
  //      * equal to the product of this value and estimated within-line spacing.
  //      */
      val wordDistanceMultiplier:Double = DEFAULT_WORD_DIST;

  //     /**
  //      * Minimum horizontal line merge distance multiplier.
  //      *
  //      * Minimum horizontal distance between lines that should be merged is equal
  //      * to the product of this value and estimated within-line spacing.
  //      *
  //      * Because split lines do not overlap this value should be negative.
  //      */

      val minHorizontalMergeDistanceMultiplier:Double = DEFAULT_MIN_HORIZONTAL_MERGE_DIST;

  //     /**
  //      * Maximum vertical line merge distance multiplier.
  //      *
  //      * Maximum vertical distance between lines that should be merged is equal
  //      * to the product of this value and estimated between-line spacing.
  //      */

      val maxVerticalMergeDistanceMultiplier:Double = DEFAULT_MAX_VERTICAL_MERGE_DIST;

  //     /**
  //      * Angle tolerance for comparisons of angles between components and angles
  //      * between lines.
  //      */
      val angleTolerance: Double = DEFAULT_ANGLE_TOLERANCE;

  //     /**
  //      * Number of nearest-neighbors found per component.
  //      */
  //     private int neighborCount = DEFAULT_NEIGHBOR_COUNT;


  //     public void setSpacingHistogramResolution(double value) {
  //         spacingHistogramResolution = value;
  //     }

  //     public void setSpacingHistogramSmoothingWindowLength(double value) {
  //         spacingHistogramSmoothingWindowLength = value;
  //     }

  //     public void setSpacingHistogramSmoothingWindowStdDeviation(double value) {
  //         spacingHistogramSmoothingWindowStdDeviation = value;
  //     }

  //     public void setMaxLineSizeScale(double value) {
  //         maxLineSizeScale = value;
  //     }

  //     public void setMaxVerticalDistanceMultiplier(double value) {
  //         maxVerticalDistanceMultiplier = value;
  //     }

  //     public void setMinHorizontalDistanceMultiplier(double value) {
  //         minHorizontalDistanceMultiplier = value;
  //     }

  //     public void setComponentDistanceCharacterMultiplier(double value) {
  //         componentDistanceCharacterMultiplier = value;
  //     }

  //     public void setWordDistanceMultiplier(double value) {
  //         wordDistanceMultiplier = value;
  //     }

  //     public void setMaxVerticalMergeDistanceMultiplier(double value) {
  //         maxVerticalMergeDistanceMultiplier = value;
  //     }

  //     public void setAngleTolerance(double value) {
  //         angleTolerance = value;
  //     }

  // }

}




      // LineDimensionBins(
      //   PageID(pagenum),
      //   topNWidths.map{ case (width, wfreq) =>
      //     //
      //     // plines.filterNot(lineWidthMatches(_, width))
      //     //   .sortBy(_.bounds.top)
      //     //   .foreach({ line =>
      //     //     if (pagenum < 2) {
      //     //       // line.determineNormalTextBounds.width.eqFuzzy(0.5d)(width)
      //     //       val ntbs = line.determineNormalTextBounds
      //     //       val nwidth = ntbs.width
      //     //       val dist = math.abs(nwidth - width)
      //     //       val withinRangs = dist < 0.5
      //     //       println(s"""| ${pagenum} > ${line.tokenizeLine().toText}
      //     //                   |     bounds: ${line.bounds.prettyPrint} normal-bounds: ${ntbs.prettyPrint}
      //     //                   |     bin-width: ${width}  norm-width: ${ntbs.width} dist: ${dist}
      //     //                   |""".stripMargin)
      //     //       if (line.toText.startsWith("Proton")) {
      //     //         line.components.foreach { c =>
      //     //           println(s"        ${c.toText} ${c.bounds.prettyPrint}")
      //     //         }
      //     //         val mfHeights = getMostFrequentValues(line.components.map(_.bounds.height), 0.1d)
      //     //         println(s"""        most-freq heights: ${mfHeights.mkString("\n          ")}""")

      //     //         // val mfHeight= mfHeights.headOption.map(_._1).getOrElse(0d)

      //     //         // components
      //     //         //   .filter(_.bounds.height.eqFuzzy(0.01d)(mfHeight))
      //     //         //   .map(_.bounds)
      //     //         //   .foldLeft(components.head.bounds)( { case (b1, b2) =>
      //     //         //     b1 union b2
      //     //         //   })

      //     //       }
      //     //     }
      //     //   })
      //     ((width, wfreq), plines.filter(lineWidthMatches(_, width)))
