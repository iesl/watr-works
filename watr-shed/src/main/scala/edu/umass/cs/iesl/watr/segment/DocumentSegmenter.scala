package edu.umass.cs.iesl.watr
package segment

import java.io.InputStream
import watrmarks._
import spindex._
// import utils.CompassDirection

import spindex._
import scalaz.@@
// import Bounds._
// import Component._
import scala.collection.JavaConversions._
import scala.collection.mutable
import utils._
import TypeTags._
import textboxing.{TextBoxing => TB}
import IndexShapeOperations._
import ComponentTypeEnrichments._
import ComponentOperations._
import ComponentRendering._
import SlicingAndDicing._
import utils.{CompassDirection => CDir}

import utils.{Histogram, AngleFilter, DisjointSets}
import Histogram._

case class LineDimensionBins(
  page: Int@@PageID,
  // Seq[(width, widthFrequency), Seq[lines w/width]]
  widthBin: Seq[((Double, Double), Seq[Component])],
  unBinned: Seq[Component]
)

case class PageSegAccumulator(
  commonLineDimensions: Seq[Point] = Seq(),
  lineDimensionBins: Seq[LineDimensionBins] = Seq(),
  commonFocalJumps: Map[String, Seq[String]] = Map()
)

trait DocumentUtils {


  def approxSortYX(charBoxes: Seq[PageRegion]): Seq[PageRegion] = {
    charBoxes.sortBy({ c =>
      (c.region.bbox.top, c.region.bbox.left)
    })
  }

  def squishb(charBoxes: Seq[PageRegion]): String = {
    approxSortYX(charBoxes)
      .map({ cbox => cbox.prettyPrint })
      .mkString
  }

  def squishc(charBoxes: Seq[PageComponent]): String = {
    squishb(charBoxes.map(_.component))
  }
}

object DocumentSegmenter extends DocumentUtils {

  def pairwiseSpaces(cs: Seq[CharRegion]): Seq[Double] = {
    val cpairs = cs.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2)  => c2.region.bbox.left - c1.region.bbox.right
      case _  => 0d
    })

    dists :+ 0d
  }

  def approximateLineBins(charBoxes: Seq[CharRegion]): Seq[(LTBounds, Seq[CharRegion])] = {
    val sortedYPage = charBoxes
      .groupBy(_.region.bbox.bottom.pp)
      .toSeq
      .sortBy(_._1.toDouble)

    val sortedXY = sortedYPage
      .map({case (topY, charBoxes) =>
        val sortedXLine = charBoxes
          .sortBy(_.region.bbox.left)
        (charBoxesBounds(sortedXLine), sortedXLine)
      })
    sortedXY
  }

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


  def createSegmenter(pdfins: InputStream): DocumentSegmenter = {
    val chars = extract.DocumentExtractor.extractChars(pdfins)
    createSegmenter(chars.map(c => (c._1.regions, c._2)))
  }

  def createSegmenter(pagedefs: Seq[(Seq[PageRegion], PageGeometry)]): DocumentSegmenter = {
    val zoneIndex = ZoneIndexer.loadSpatialIndices2(pagedefs)
    new DocumentSegmenter(zoneIndex)
  }


  def debugFormatLine(cc: Component): String = {
    import TB._
    val line = renderConnectedComponents(cc.tokenizeLine())
    s"${cc.bounds.prettyPrint}, r:${cc.bounds.right.pp} b:${cc.bounds.bottom} ctr:${cc.bounds.toCenterPoint.prettyPrint} > ${hsep(line)}"
  }


  def candidateCrossesLineBounds(cand: Component, line: Component): Boolean = {
    val slopFactor = 0.31d

    val linex0 = line.bounds.toWesternPoint.x-slopFactor
    val linex1 = line.bounds.toEasternPoint.x+slopFactor
    val candx0 = cand.bounds.toWesternPoint.x
    val candx1 = cand.bounds.toEasternPoint.x
    val candRightInside = linex0 <= candx1 && candx1 <= linex1
    val candLeftOutside = candx0 < linex0
    val candLeftInside = linex0 <= candx0 && candx0 <= linex1
    val candRightOutside = linex1 < candx1

    val crossesLeft = candRightInside && candLeftOutside
    val crossesRight = candLeftInside && candRightOutside


    crossesLeft || crossesRight
  }

  def isOverlappedVertically(line1: Component, line2: Component): Boolean = {
    !(isStrictlyAbove(line1, line2) || isStrictlyBelow(line1, line2))
  }

  def isStrictlyAbove(line1: Component, line2: Component): Boolean = {
    val y1 = line1.bounds.toPoint(CompassDirection.S).y
    val y2 = line2.bounds.toPoint(CompassDirection.N).y
    y1 < y2
  }
  def isStrictlyBelow(line1: Component, line2: Component): Boolean = {
    val y1 = line1.bounds.toPoint(CompassDirection.N).y
    val y2 = line2.bounds.toPoint(CompassDirection.S).y
    y1 > y2
  }

  def isStrictlyLeftToRight(cand: Component, line: Component): Boolean = {
    val linex0 = line.bounds.toWesternPoint.x
    val candx1 = cand.bounds.toEasternPoint.x
    candx1 < linex0
  }

  def isStrictlyRightToLeft(cand: Component, line: Component): Boolean = {
    val linex1 = line.bounds.toEasternPoint.x
    val candx0 = cand.bounds.toWesternPoint.x
    candx0 > linex1
  }
  def candidateIsOutsideLineBounds(cand: Component, line: Component): Boolean = {
    isStrictlyLeftToRight(cand, line) ||
      isStrictlyRightToLeft(cand, line)
  }

}



class DocumentSegmenter(
  val pages: ZoneIndexer
) {
  import scala.math.Ordering.Implicits._
  implicit def RegionIDOrdering: Ordering[Int@@RegionID] = Ordering.by(_.unwrap)

  import DocumentSegmenter._

  val LB = StandardLabels

  var docOrientation: Double = 0d

  var pageSegAccum: PageSegAccumulator = PageSegAccumulator(Seq())



  def runLineDetermination(): Component = {
    val allPageLines = for {
      pageId <- pages.getPages
    } yield {
      determineLines(pageId, pages.getComponents(pageId))
    }
    pages.concatComponents(allPageLines, LB.Pages)
  }



  def groupLeftAlignedBlocks(): Unit = {
    // lines ordered as per cc analysis
    val leftBinHistResolution = 1.0d


    val alignedBlocksPerPage = for {
      page <- visualLineOnPageComponents
    } yield {
      println("Processing page")

      val lefts = page.zipWithIndex
        .map({case (l, i) => (l.bounds.left, i)})
        .sortBy(_._1)

      // val freqLefts = getMostFrequentValues(lefts.map(_._1), leftBinHistResolution)
      val hist = histogram(lefts.map(_._1), leftBinHistResolution)
      hist.smooth(leftBinHistResolution)

      val freqLefts = hist
        .getFrequencies
        .sortBy(_.frequency)
        .reverse
        .takeWhile(_.frequency > 0)
        .map{b=>(b.value, b.frequency)}

      val fmt = freqLefts.map({case (v, f) => s"${v.pp} ($f)" }).mkString("freq\n   ", "\n   ", "\n/freq")
      println(fmt)


      def valueIsWithinHistBin(bin: Double, res: Double)(value: Double): Boolean = {
        bin-res <= value && value <= bin+res
      }

      // val clusters = freqLefts.map({ case (leftBin, freq) =>
      //   lefts.partition { case (leftX, i) =>
      //     valueIsWithinHistBin(leftBin, 2.0)(leftX)
      //   }
      // })



      // val clustered = lefts.clusterBy({case ((l1, i1), (l2, i2)) =>
      //   math.abs(l1 - l2) < 2.0d
      // })


      val groupedBlocks = page.zipWithIndex
        .splitOnPairs({ case ((l1, l1i), (l2, l2i)) =>
          // val linesAreClustered = clustered.exists({ grp =>
          //   grp.exists(_._2==l1i) && grp.exists(_._2==l2i)
          // })

          val linesAreClustered = freqLefts.exists({ case (leftBin, freq) =>
            val l1InBin = valueIsWithinHistBin(leftBin, leftBinHistResolution)(l1.bounds.left)
            val l2InBin = valueIsWithinHistBin(leftBin, leftBinHistResolution)(l2.bounds.left)
            l1InBin && l2InBin
          })

          val h1 = l1.determineNormalTextBounds.height
          val h2 = l2.determineNormalTextBounds.height
          val heightsDiffer = h1 != h2

          heightsDiffer || (!linesAreClustered)
        })

      groupedBlocks
    }

    val allVDists = for {
      groupedBlocks <- alignedBlocksPerPage
      block <- groupedBlocks
    } yield {
      block
        .sliding(2).toSeq
        .map({
          case Seq((a1, i1), (a2, i2)) =>
            val a1Left = a1.bounds.toPoint(CDir.W).y
            val a2Left = a2.bounds.toPoint(CDir.W).y
            val vdist = math.abs(a1Left - a2Left)

            math.abs(vdist)

          case Seq((a1, i1)) => -1
          case Seq() => -1
        }).filter(_ > 0d)
    }

    // TODO use a histogram here for clustering
    val clusterVDists = allVDists.flatten.clusterBy { (d1, d2) =>
      math.abs(d1-d2) < 0.18
    }

    // println(s"clusters")
    // val asdf = clusterVDists.map { vclust =>
    //   val clustpp = vclust.sorted.map(_.pp).toSet.toSeq.sorted.mkString(", ")
    //   println(s"   ${vclust.length}:  ${clustpp}")
    //   (vclust.length, vclust.max)
    // }
    // println(s"clusters")
    val modalVDist = clusterVDists
      .map { vclust => (vclust.length, vclust.max) }
      .sortBy(_._1)
      .reverse.headOption
      .map(_._2)
      .getOrElse(12.0d)


    // One last pass through block to split over-large vertical line jumps
    val finalSplit = for {
      groupedBlocks <- alignedBlocksPerPage
      block <- groupedBlocks
    } yield {
      block.splitOnPairs ({ case ((a1, i1), (a2, i2)) =>
        val a1Left = a1.bounds.toPoint(CDir.W).y
        val a2Left = a2.bounds.toPoint(CDir.W).y
        val vdist = math.abs(a1Left - a2Left)
        val maxVDist = modalVDist * 1.15d

        // if (vdist > maxVDist) {
        //   println(s"splitting lines on vdist=${vdist}, maxd=${maxVDist}  modald=${modalVDist}")
        //   println(s"   ${a1.tokenizeLine().toText}")
        //   println(s"   ${a2.tokenizeLine().toText}")
        // }

        vdist > maxVDist
      })
    }

    // Now create a BIO labeling linking visual lines into blocks
    val textBlockSpine = pages.bioSpine("TextBlockSpine")

    val spine = finalSplit
      .flatten
      .map({ block =>
        // pages.concatComponents(block.map(_._1), LB.TextBlock)
        val unlabeled = block.map(_._1).map(BioNode(_))
        unlabeled.map({node =>
          node

          node
        })
      })

    // finalSplit.flatten
    // .foreach { block =>
    //   println(s"left-aligned group")
    //   block.foreach{ case (line, i) =>
    //     println(s"   ${line.tokenizeLine().toText}")
    //   }
    // }

  }

  def labelPageLines(): Unit = {

    val lineSpine = pages.bioSpine("VisualLines")

    lineSpine ++= (for {
      pagesComp <- pages.getLabeledComponents(LB.Pages)
      pageComp <- pagesComp.labeledChildren(LB.Page)
      line <- pageComp.labeledChildren(LB.VisualLine)
    } yield {
      BioNode(line)
    })
  }



  def runPageSegmentation(): String = { // Seq[ConnectedComponents]
    // import TB._

    // Bottom-up connected-component line-finding
    runLineDetermination()
    groupLeftAlignedBlocks()

    // document-wide stats on cc discovered lines
    // findMostFrequentLineDimensions()

    // findMostFrequentFocalJumps()

    // val orderedLinesPerPage = for {
    //   pageId <- pages.getPages
    // } yield {
    //   println(s"segmenting page ${pageId}")
    //   groupPageTextBlocks(pageId)
    // }
    // labelPageLines()

    // // label text lines (added label on page lines)
    // // label text blocks BIO labeling on text lines
    // // label figure/captions

    labelSectionHeadings()


    ""

  }


  private def findNeighbors(pageId: Int@@PageID, qbox: CharRegion): Seq[CharRegion] = {
    pages.nearestNChars(pageId, qbox, 12, 15.0f)
      .filterNot(_.isWonky)
  }


  val withinAngle = filterAngle(docOrientation, math.Pi / 3)

  def fillInMissingChars(pageId: Int@@PageID, charBoxes: Seq[CharRegion]): Seq[CharRegion] = {
    if (charBoxes.isEmpty) Seq()
    else {
      val ids = charBoxes.map(_.region.id.unwrap)
      val minId = ids.min
      val maxId = ids.max

      val missingIds = (ids.min to ids.max) diff ids
      val missingChars = missingIds.map(id => pages.getComponent(pageId, RegionID(id)))

      // TODO check missing chars for overlap w/lineChars
      val completeLine = (charBoxes ++ missingChars).sortBy(_.region.bbox.left)

      // // check for split in line
      // println(s"""${charBoxes.map(_.bestGuessChar).mkString}""")
      // println(s"""  +: ${missingChars.map(_.bestGuessChar).mkString}""")
      // println(s"""  =: ${completeLine.map(_.bestGuessChar).mkString}""")
      completeLine
    }
  }

  def determineLines(
    pageId: Int@@PageID,
    components: Seq[CharRegion]
  ): Component = {

    val lineSets = new DisjointSets[CharRegion](components)

    // line-bin coarse segmentation
    val lineBins = approximateLineBins(components)

    /// Split run-on lines (crossing columns, e.g.,), by looking for jumps in the char.id
    val splitAndFilledLines = for {
      (lineBounds, lineChars) <- lineBins
      if !lineChars.isEmpty
    } {

      def spanloop(chars: Seq[CharRegion]): Seq[Int] = {
        if (chars.isEmpty) Seq()
        else {
          val (line1, line2) = chars
            .sliding(2).span({
              case Seq(ch1, ch2) => ch2.region.id.unwrap - ch1.region.id.unwrap < 20
              case Seq(_)     => true
            })
          val len = line1.length
          len +: spanloop(chars.drop(len+1))
        }
      }

      var totalIndex: Int = 0

      val splitLines = spanloop(lineChars)
        .foreach({ index =>
          val m = fillInMissingChars(pageId, lineChars.drop(totalIndex).take(index+1))

          m.tail.foreach({char =>
            lineSets.union(char, m.head)
            // if (!char.isWonky) {
            //   lineSets.union(char, m.head)
            // }
          })
          totalIndex += index+1;
        })
    }


    def regionIds(cc: Component): Seq[Int@@RegionID] = {
      cc.component.targetRegions.map(_.id)
    }

    def minRegionId(ccs: Seq[Component]): Int@@RegionID = {
      ccs.flatMap(regionIds(_)).min
    }

    // consider each line pair-wise and decide if they should be re-joined:
    val prejoined = lineSets.iterator().toSeq
      .map({ line =>
        line.toSeq
          .sortBy(c => (c.region.bbox.left, c.region.bbox.top))
          .map(c => pages.toComponent(c))
      })
      .sortBy(line => minRegionId(line))



    val first = prejoined.headOption.toSeq

    val maybeJoined = prejoined.drop(1).toSeq
      .foldLeft(first)({ case (acc, l2) =>
        val l1 = acc.last

        val l1max = regionIds(l1.last).max.unwrap
        val l2min = regionIds(l2.head).min.unwrap
        val idgap = l2min - l1max

        // val idgap = l2.head.component.region.id.unwrap - l1.last.component.region.id.unwrap
        val shouldJoin = (
          isStrictlyLeftToRight(l1.last, l2.head)
            && isOverlappedVertically(l1.last, l2.head)
            && idgap < 5
        )

        // def isStrictlyLeftToRight(cand: Component, line: Component): Boolean = {
        // val acclast = l1.last.bounds.prettyPrint
        // val next = l2.head.bounds.prettyPrint
        // val l1EastX = l1.last.bounds.toEasternPoint.x
        // val l2WestX = l2.head.bounds.toWesternPoint.x
        // val totalAcc = l1.map(_.toText).mkString
        // val considering = l2.map(_.toText).mkString
        // println(s"${totalAcc}  ?:: ${considering}")
        // println(s"  idgap = ${idgap}, shouldJoin = ${shouldJoin} l1-east=${l1EastX.pp}, l2-west=${l2WestX.pp}")
        // println(s"  ${acclast}  -- ${next}")

        if (shouldJoin) acc.dropRight(1) :+ (l1 ++ l2)
        else acc :+ l2
      })

    // val maybeJoined = prejoined.sliding(2).toSeq.map({
    //   case Seq(l1, l2) =>
    //     val idgap = l2.head.component.id.unwrap - l1.last.component.id.unwrap
    //     val shouldJoin = isStrictlyLeftToRight(l1.last, l2.head) && idgap < 5
    //     if (shouldJoin) Seq(l1 ++ l2)
    //     else Seq(l1, l2)
    //   case Seq(x) => Seq(x)
    //   case _ => Seq(Seq())
    // })

    val linesJoined = maybeJoined
      .sortBy(b => b.map(regionIds(_)).min)
      .map(l => pages.concatComponents(l, LB.VisualLine))

    pages.concatComponents(linesJoined, LB.Page)
  }



  def charBasedPageBounds(
    pageId: Int@@PageID
  ): LTBounds = {
    val allBboxes = pages.getComponents(pageId).map(_.region.bbox)

    if (allBboxes.isEmpty) LTBounds(0, 0, 0, 0) else {
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
  }


  def lineWidthMatches(line: Component, width: Double): Boolean  = {
    line.determineNormalTextBounds.width.eqFuzzy(0.5d)(width)
  }
  def lineHeightMatches(line: Component, height: Double): Boolean  = {
    line.determineNormalTextBounds.height.eqFuzzy(0.5d)(height)
  }

  def lineDimensionsMatch(line: Component, hw: Point): Boolean = {
    lineWidthMatches(line, hw.x) && lineHeightMatches(line, hw.y)
  }


  def printPageLineBins(bin: LineDimensionBins, indent: Int=0): Unit = {
    bin
      .widthBin
      .sortBy(_._1)
      .filter({ case (width, lines) => !lines.isEmpty })
      .foreach({ case (width, lines) =>
        println(" "*indent + s"Lines within width ${width}")
        lines.sortBy(_.bounds.top).foreach{ line =>
          println("  "*indent + s"w:${line.bounds.width.pp}, h:${line.bounds.height.pp} ${line.bounds.prettyPrint} > ${line.tokenizeLine().toText}")
        }
    })
  }

  def printCommonLineBins(lineBins: Seq[LineDimensionBins]): Unit = {
    lineBins.zipWithIndex.toList.foreach{ case (bin, pnum) =>
      println(s"Page $pnum")
      printPageLineBins(bin, 2)
    }
  }

  def visualLineOnPageComponents: Seq[Seq[Component]] = for {
    ps <- pages.getLabeledComponents(LB.Pages)
    p <- ps.labeledChildren(LB.Page)
  } yield p.labeledChildren(LB.VisualLine)



  def focalJump(c1: Component, c2: Component): Double = {
    val c1East = c1.bounds.toPoint(CompassDirection.E)
    val c2West = c2.bounds.toPoint(CompassDirection.W)
    c1East.dist(c2West)
  }

  def findMostFrequentFocalJumps():  Unit = {
    val allWidthsAndDists = for {
      pageBin <- pageSegAccum.lineDimensionBins
      ((width, widthFreq), linesWithFreq)  <- pageBin.widthBin

    } yield {

      val sameCenterGroups = linesWithFreq.clusterBy((a, b) => a.hasSameLeftEdge()(b))
      /// print out a list of components
      // val grouped = sameCenterGroups.map({ group =>
      //   group.map({comp =>
      //     s"""comp:> ${comp.toText}"""
      //   }).mkString("\n   ", "\n   ", "\n")
      // }).mkString("---------")
      // println(s"width=${width} freq=${widthFreq}, groups page ${pageBin.page}")
      // println(grouped)

      val widthsAndDists = sameCenterGroups.map{ group =>
        val sorted = group
          .sortBy(_.bounds.top)

        val dists = sorted
          .sliding(2).toSeq
          .map({
            case Seq(c1, c2)  => focalJump(c1, c2)
            case _            => 0d
          })

        dists :+ 0d

        sorted.map(_.bounds.width).zip(dists)
      }
      widthsAndDists.flatten
    }

    val focalJumps = allWidthsAndDists
      .flatten
      .groupBy(_._1)
      .map({case (k, v) => (k.pp, v.map(_._2.pp))})

    val jstr = focalJumps.map({case (k, v) => s"""w:${k} = [${v.mkString(", ")}]"""})
    val jcol = jstr.mkString("\n   ", "\n   ",  "\n")

    println(s""" focalJumps: ${jcol} """)

    pageSegAccum = pageSegAccum.copy(
      commonFocalJumps = focalJumps
    )
  }



  def findMostFrequentLineDimensions():  Unit = {

    val allPageLines = for {
      p <- visualLineOnPageComponents; l <- p
    } yield l

    val allDocumentWidths = allPageLines.map(_.bounds.width)

    val topWidths = getMostFrequentValues(allDocumentWidths, 0.2d).toList
    // val topNWidths = topWidths // .take(7)
    val topNWidths = topWidths.takeWhile(_._2 > 1.0)
    // Common width meaning from largest->smallest:
    //    left/right justified line width
    //    paragraph-starting indented line width
    //    reference width(s), for hanging-indent first line, remaining lines
    //    other l/r justified blocks (abstract, e.g)

    // println(s"""common widths = ${topNWidths.mkString(", ")}""")

    // bin each page by line widths
    val commonLineBins = for {
      (plines, pagenum) <- visualLineOnPageComponents.zipWithIndex
    } yield {

      val remainingLines = mutable.ListBuffer[Component](plines:_*)
      val widthBins = mutable.ArrayBuffer[((Double, Double), Seq[Component])]()

      topNWidths.foreach{ case (width, wfreq) =>
        val mws = remainingLines.filter(lineWidthMatches(_, width))
        widthBins.append(((width -> wfreq), mws))
        remainingLines --= mws
      }

      LineDimensionBins(PageID(pagenum), widthBins, remainingLines)
    }
    printCommonLineBins(commonLineBins)
    pageSegAccum = pageSegAccum.copy(
      lineDimensionBins = commonLineBins
    )
  }

  def labelSectionHeadings(): Unit = {

    val vlines = pages.bioSpine("VisualLines")
    println(s"len lineSpine = ${vlines.length}")
    for {
      linec <- vlines
      line = linec.component
      numbering = line.chars.takeWhile(c => c.isDigit || c=='.')
      nexts = line.chars.drop(numbering.length) //.takeWhile(c => c.isLetter)
      isTOCLine = """\.+""".r.findAllIn(nexts).toSeq.sortBy(_.length).lastOption.exists(_.length > 4)

      if !numbering.isEmpty && !nexts.isEmpty()
      // _ = println(s"""${line.chars}""")
      if numbering.matches("^[1-9]+\\.([1-9]\\.)*")
      if !isTOCLine
      // _ = println(s"""    >${numbering}""")
    } {
      val ns = numbering.split("\\.").toList.map(_.toInt)
      // line.addLabel(LB.SectionHeadingLine)
      // TODO line.addLabel(LB.SectionHeading.withValue(...)) || line.addLabel(LB.SectionNumber.withValue(...))
      val p = BioPins(
        id = 0,
        pin = LB.SectionHeadingLine.U
      )
      println(s"adding ${p}")
      linec.pins.add(p)

      // linec.pins += p

      (line, ns)

    }

    // val numberedSectionLines = for {
    //   page <- visualLineOnPageComponents


    // println("sorted sections ")
    // sortedSections.foreach { case (line, ns) =>
    //   val rline = renderConnectedComponents(line)
    //   val nss = ns.mkString(".")
    //   println(s"${nss}   ${rline}")
    // }

    // pages.addLabels(zl: ZoneAndLabel)
    // sortedSections.foreach({ case (linecc, ns) =>
    //   val asdf =linecc.withLabel(LB.Heading)
    // })

    // What are the predominant fonts per 'level'?

    // Distinguish between TOC and in-situ section IDs

    // println(s"""section: ${headerLines.map(_.toText).mkString(" ")}""")

    // look for rectangular blocks of text (plus leading/trailing lines)
    // look for for left-aligned (column-wise) single or double numbered text lines w/
    //   large gaps

    // filter out figure/caption/footnotes based on embedded images and page locations

  }



  ///
  def groupVisualTextBlocks(
    colX: Double, textRectCandidates: Seq[Component], remainingPageLines: Seq[Component]
  ): Seq[Seq[Component]] = {
    val unusedLines = mutable.ArrayBuffer[Component](remainingPageLines:_*)
    val usedLines = mutable.ArrayBuffer[Component]()

    val ySortedLines = textRectCandidates.sortBy(_.bounds.top)
    val topLine = ySortedLines.head
    // val bottomLine = ySortedLines.last


    val possibleCand = remainingPageLines
      .diff(ySortedLines)
      .filterNot(candidateIsOutsideLineBounds(_, topLine))

    val commonJumps = pageSegAccum.commonFocalJumps



    val totalLinesSorted =  (possibleCand ++ ySortedLines).sortBy(_.bounds.top)

    totalLinesSorted.
      sliding(2).toSeq
      .map({
        case Seq(upper, lower) =>
          val jump = focalJump(upper, lower)
          val ujumps = commonJumps(upper.bounds.width.pp)
          val ljumps = commonJumps(lower.bounds.width.pp)
          val jumps = ujumps ++ ljumps

          if (jumps contains jump.pp) {
            println("common jump found")
          }

        case Seq(lone) => 0d
        case Seq() => 0d
      })

    // Walk down column lines pair-wise, and take while diagonal distances match


    // pages.concatComponents(totalLineSorted, LB.Block)
    // totalLineSorted

    ???
  }




  // def groupPageTextBlocks(
  //   pageId: Int@@PageID
  // ): Unit = {

  //   val pageLines: Seq[Component] = visualLineOnPageComponents(pageId.unwrap)

  //   val pageBounds = charBasedPageBounds(pageId)
  //   val pageCenter = pageBounds.toCenterPoint

  //   val lineBins = pageSegAccum.lineDimensionBins.find(_.page == pageId).get

  //   val unusedPageLines = mutable.ArrayBuffer[Component](pageLines:_*)
  //   val usedPageLines = mutable.ArrayBuffer[Component]()

  //   // starting w/most common width, down to least common..
  //   val allBlocks = lineBins.widthBin.sortBy(_._1._2).reverse.map {
  //     case ((mostFrequentWidthDocwide, wfreq), linesWithFreq) =>

  //       val remainingLinesWithFreq = linesWithFreq.diff(usedPageLines)

  //       if (remainingLinesWithFreq.isEmpty) Seq() else {

  //         // divide page-specific most frequent lines into likely columns:
  //         val colCenters = getMostFrequentValues(remainingLinesWithFreq.map(_.bounds.toCenterPoint.x) , resolution=0.2d)

  //         val commonLinesInCols = for {
  //           (colX, cfreq) <- colCenters
  //         } yield {
  //           val candidateLines = remainingLinesWithFreq.filter({ line => line.bounds.toCenterPoint.x.eqFuzzy(0.4)(colX) })
  //           val visBlocks = groupVisualTextBlocks(colX, candidateLines, unusedPageLines)
  //           visBlocks.foreach { vblock =>
  //             pages.concatComponents(vblock, LB.TextBlock)
  //             unusedPageLines --= vblock
  //           }
  //         }
  //       }

  //     case _ => Seq()
  //   }

  //   if (unusedPageLines.length >0 ) {
  //     println(s"""Error: Unused page lines in text line grouping""")
  //   }
  // }


  // def sortZonesYX(zones: Seq[Component]): Seq[Component]= {

  //   zones.sortWith({case (cc1, cc2) =>
  //     val ycmp = compareDouble(cc1.bounds.top, cc2.bounds.top, 0.01)

  //     val cmp = if (ycmp == 0) {
  //       compareDouble(cc1.bounds.left, cc2.bounds.left, 0.01)
  //     } else {
  //       ycmp
  //     }

  //     cmp < 0
  //   })
  // }

}








// FIXME: delete
  // def groupVisualTextRects(colX: Double, textRectCandidates: Seq[Component], remainingPageLines: Seq[Component]): Unit = {
  //   val unusedLines = mutable.ArrayBuffer[Component](remainingPageLines:_*)
  //   val usedLines = mutable.ArrayBuffer[Component]()

  //   val ySortedLines = textRectCandidates.sortBy(_.bounds.top)
  //   val topLine = ySortedLines.head
  //   val bottomLine = ySortedLines.last


  //   def candidateIsBelowBottom(cand: Component) = cand.bounds.top > bottomLine.bounds.top
  //   def candidateIsBelowTop(cand: Component) = cand.bounds.top > topLine.bounds.top
  //   def candidateIsAboveBottom(cand: Component) = cand.bounds.top < bottomLine.bounds.top
  //   def candidateIsAboveTop(cand: Component) = cand.bounds.top < topLine.bounds.top


  //   val possibleCand = unusedLines
  //     .diff(ySortedLines)
  //     .sortBy(_.bounds.top)

  //   val candidateLinesAbove = possibleCand
  //     .reverse
  //     .filter(candidateIsAboveTop(_))
  //     .filterNot(candidateIsOutsideLineBounds(_, topLine))
  //     .takeWhile({cc =>
  //       val colBreak = candidateCrossesLineBounds(cc, topLine)

  //       !colBreak
  //     })


  //   val candidateLinesBelow = possibleCand
  //     .filter(candidateIsBelowBottom(_))
  //     .filterNot(candidateIsOutsideLineBounds(_, topLine))
  //     .takeWhile({cc =>
  //       val colBreak = candidateCrossesLineBounds(cc, topLine)
  //         !colBreak
  //     })


  //   val candidateLinesWithin = possibleCand
  //     .filter(c =>candidateIsAboveBottom(c) && candidateIsBelowTop(c))
  //     .filterNot(candidateIsOutsideLineBounds(_, topLine))
  //     .filterNot(candidateCrossesLineBounds(_, topLine))


  //   val totalLines =  candidateLinesAbove ++ ySortedLines ++ candidateLinesWithin ++ candidateLinesBelow
  //   val totalLineSorted = totalLines.sortBy(_.bounds.top)

  //   unusedLines --= totalLineSorted
  //   usedLines ++= totalLineSorted

  //   pages.concatComponents(totalLineSorted, LB.Block)
  // }
