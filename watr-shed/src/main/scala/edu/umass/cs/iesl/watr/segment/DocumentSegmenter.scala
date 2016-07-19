package edu.umass.cs.iesl.watr
package segment

import java.io.InputStream
import watrmarks._
import spindex._

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


  def approxSortYX(charBoxes: Seq[PageAtom]): Seq[PageAtom] = {
    charBoxes.sortBy({ c =>
      (c.region.bbox.top, c.region.bbox.left)
    })
  }

  def squishb(charBoxes: Seq[PageAtom]): String = {
    approxSortYX(charBoxes)
      .map({ cbox => cbox.prettyPrint })
      .mkString
  }

  def squishc(charBoxes: Seq[PageComponent]): String = {
    squishb(charBoxes.map(_.component))
  }
}

object DocumentSegmenter extends DocumentUtils {

  def pairwiseSpaces(cs: Seq[CharAtom]): Seq[Double] = {
    val cpairs = cs.sliding(2).toList

    val dists = cpairs.map({
      case Seq(c1, c2)  => c2.region.bbox.left - c1.region.bbox.right
      case _  => 0d
    })

    dists :+ 0d
  }

  def approximateLineBins(charBoxes: Seq[CharAtom]): Seq[(LTBounds, Seq[CharAtom])] = {
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
    val chars = format.DocumentIO.extractChars(pdfins)
    createSegmenter(chars.map(c => (c._1.regions, c._2)))
  }

  def createSegmenter(pagedefs: Seq[(Seq[PageAtom], PageGeometry)]): DocumentSegmenter = {
    val zoneIndex = ZoneIndexer.loadSpatialIndices2(pagedefs)
    new DocumentSegmenter(zoneIndex)
  }


  def debugFormatLine(cc: Component): String = {
    import TB._
    val line = renderConnectedComponents(cc)
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
  val zoneIndexer: ZoneIndexer
) {

  import scala.math.Ordering.Implicits._
  implicit def RegionIDOrdering: Ordering[Int@@RegionID] = Ordering.by(_.unwrap)

  import DocumentSegmenter._

  val LB = StandardLabels

  var docOrientation: Double = 0d

  var pageSegAccum: PageSegAccumulator = PageSegAccumulator(Seq())



  def runLineDetermination(): Unit = {
    val allPageLines = for {
      pageId <- zoneIndexer.getPages
    } yield {
      val charAtoms = zoneIndexer.getPageInfo(pageId).charAtomIndex.getItems

      determineLines(pageId, charAtoms)
    }

    // allPageLines.foreach { c =>
    //   println()
    //   println(c.chars)
    // }
  }


  def docWideModalParaFocalJump(
    // alignedBlocksPerPage: Seq[Seq[Seq[(Component, Int)]]],
    alignedBlocksPerPage: Seq[Seq[BioNode]],
    modalVDist: Double
  ): Double = {
    val allVDists = for {
      groupedBlocks <- alignedBlocksPerPage
      block <- groupedBlocks
    } yield {
      // block
      //   .sliding(2).toSeq
      //   .map({
      //     case Seq((a1, i1), (a2, i2)) =>
      //       val a1Left = a1.bounds.toPoint(CDir.W).y
      //       val a2Left = a2.bounds.toPoint(CDir.W).y
      //       val vdist = math.abs(a1Left - a2Left)

      //       math.abs(vdist)

      //     case Seq((a1, i1)) => -1
      //     case Seq() => -1
      //   }).filter(_ > 0d)
    }

    // getMostFrequentValues(allVDists.flatten, leftBinHistResolution)
    //   .headOption.map(_._1)
    //   .getOrElse(12.0)


    15.0d
  }


  def docWideModalLineVSpacing(alignedBlocksPerPage: Seq[Seq[Seq[(Component, Int)]]]): Double = {
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

    val modalVDist = getMostFrequentValues(allVDists.flatten, leftBinHistResolution)
      .headOption.map(_._1)
      .getOrElse(12.0)


    modalVDist

  }

  val leftBinHistResolution = 1.0d

  import utils.TraceLog
  import utils.VisualTrace._

  def findLeftAlignedBlocksPerPage(): Seq[Seq[Seq[(Component, Int)]]] = {
    val alignedBlocksPerPage = for {
      page <- visualLineOnPageComponents
    } yield {
      println("Processing page")

      TraceLog.trace(
        Message("findLeftAlignedBlocksPerPage")
      )

      val lefts = page.zipWithIndex
        .map({case (l, i) => (l.bounds.left, i)})

      val freqLefts = getMostFrequentValues(lefts.map(_._1), leftBinHistResolution)

      TraceLog.trace(
        All(freqLefts.map({ case (bin, freq) => VRuler(bin) }):_*),
        Message("most frequent lefts")
      )

      def valueIsWithinHistBin(bin: Double, res: Double)(value: Double): Boolean = {
        bin-res <= value && value <= bin+res
      }


      val groupedBlocks = page.zipWithIndex
        .splitOnPairs({ case ((l1, l1i), (l2, l2i)) =>

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
    alignedBlocksPerPage

  }

  def splitBlocksWithLargeVGaps(
    alignedBlocksPerPage: Seq[Seq[Seq[(Component, Int)]]],
    modalVDist: Double
  ): Seq[Seq[Seq[(Component, Int)]]] = {
    // One last pass through block to split over-large vertical line jumps
    for {
      blocksOnPage <- alignedBlocksPerPage
      block <- blocksOnPage
    } yield {
      block.splitOnPairs ({ case ((a1, i1), (a2, i2)) =>
        val vdist = a1.vdist(a2)
        val maxVDist = modalVDist * 1.15d
        // if (vdist > maxVDist) {
        //   println(s"splitting lines on vdist=${vdist}, maxd=${maxVDist}  modald=${modalVDist}")
        //   println(s"   ${a1.tokenizeLine().toText}")
        //   println(s"   ${a2.tokenizeLine().toText}")
        // }

        vdist > maxVDist
      })
    }

  }

  import BioLabeling._

  def groupLeftAlignedBlocks(): Unit = {
    // lines ordered as per cc analysis
    val alignedBlocksPerPage = findLeftAlignedBlocksPerPage()

    val modalVDist = docWideModalLineVSpacing(alignedBlocksPerPage)

    val finalSplit = splitBlocksWithLargeVGaps(alignedBlocksPerPage, modalVDist)

    // Now create a BIO labeling linking visual lines into blocks
    val spine = finalSplit
      .flatten
      .map({ block =>
        val bios = block.map(_._1).map(BioNode(_))
        zoneIndexer.addBioLabels(LB.TextBlock, bios)
        bios
      })

    val textBlockSpine = zoneIndexer.bioSpine("TextBlockSpine")
    textBlockSpine ++= spine.flatten

    val blocks = selectBioLabelings(LB.TextBlock, textBlockSpine)


    val modalParaFocalJump = docWideModalParaFocalJump(blocks, modalVDist)

    blocks.splitOnPairs({
      case (aNodes: Seq[BioNode], bNodes: Seq[BioNode]) =>

        // if, for consecutive blocks a, b features include
        //   - a is length 1
        //   - a is indented (determine std indents)
        //   - a's width falls strictly within b's width
        //   - dist a -> b is near doc-wide standard line distance
        //   - a is at end of column, b is higher on page, or next page

        val onSamePage = true
        val aIsSingeLine = aNodes.length == 1
        val aIsAboveB = isStrictlyAbove(aNodes.last.component, bNodes.head.component)
        val aComp = aNodes.last.component
        val bComp = bNodes.head.component
        val aWidth = aComp.bounds.width
        val bWidth = bComp.bounds.width

        val vdist = aComp.vdist(bComp)
        val withinCommonVDist = vdist.eqFuzzy(1.0)(modalVDist)
        val aWithinBsColumn = bComp.columnContains(aComp)

        val aIsIndented = aWidth < bWidth - 4.0

        // println("comparing for para begin: ")
        // println(s"  a> ${aComp.chars}")
        // println(s"  b> ${bComp.chars}")

        // println(s"     a.bounds=${aComp.bounds.prettyPrint} b.bounds=${bComp.bounds.prettyPrint}")
        // println(s"     a.right=${aComp.bounds.right.pp} b.right=${bComp.bounds.right.pp}")
        // println(s"     a is indented = ${aIsIndented}")
        // println(s"     a strictly above = ${aIsAboveB}")
        // println(s"     b columnContains a = ${aWithinBsColumn}")
        // println(s"     a/b within modal v-dist = ${withinCommonVDist} = ${modalVDist}")

        if (onSamePage &&
          aIsSingeLine &&
          aIsAboveB &&
          aIsIndented &&
          withinCommonVDist &&
          aWithinBsColumn
        ) {
          zoneIndexer.addBioLabels(LB.ParaBegin, aNodes)
        }

        true
    })

  }


  def runPageSegmentation(): Unit = {
    // Bottom-up connected-component line-finding
    runLineDetermination()
    groupLeftAlignedBlocks()

    // document-wide stats on cc discovered lines
    // findMostFrequentLineDimensions()
    // findMostFrequentFocalJumps()

    // val orderedLinesPerPage = for { pageId <- zoneIndexer.getPages }
    //     yield { groupPageTextBlocks(pageId) }

    labelAbstract()
    labelSectionHeadings()

  }

  private def findNeighbors(pageId: Int@@PageID, qbox: CharAtom): Seq[CharAtom] = {
    val atomIndex = zoneIndexer.getPageInfo(pageId).charAtomIndex
    atomIndex.nearestNItems(qbox, 12, 15.0f)
      .filterNot(_.isWonky)
  }


  val withinAngle = filterAngle(docOrientation, math.Pi / 3)

  def fillInMissingChars(pageId: Int@@PageID, charBoxes: Seq[CharAtom]): Seq[CharAtom] = {
    if (charBoxes.isEmpty) Seq()
    else {
      val ids = charBoxes.map(_.region.id.unwrap)
      val minId = ids.min
      val maxId = ids.max

      val missingIds = (ids.min to ids.max) diff ids

      val missingChars = missingIds.map(id =>
        // zoneIndexer.getAtom(pageId, RegionID(id))
        zoneIndexer.getPageInfo(pageId).charAtomIndex.getItem(id)
      )

      // val missingChars = missingIds.map(id => zoneIndexer.getAtom(pageId, RegionID(id)))

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
    components: Seq[CharAtom]
  ): Unit = {

    val lineSets = new DisjointSets[CharAtom](components)

    // line-bin coarse segmentation
    val lineBins = approximateLineBins(components)

    /// Split run-on lines (crossing columns, e.g.,), by looking for jumps in the char.id
    val splitAndFilledLines = for {
      (lineBounds, lineChars) <- lineBins
      if !lineChars.isEmpty
    } {

      def spanloop(chars: Seq[CharAtom]): Seq[Int] = {
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
          .map(c => zoneIndexer.toComponent(c))
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
      .map(l => zoneIndexer.concatComponents(l, LB.VisualLine))

    if (!linesJoined.isEmpty) {
      zoneIndexer.concatComponents(linesJoined, LB.Page)
    }
  }



  def charBasedPageBounds(
    pageId: Int@@PageID
  ): LTBounds = {
    val allBboxes = zoneIndexer.getPageInfo(pageId).charAtomIndex.getItems.map(_.region.bbox)

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
          println("  "*indent + s"w:${line.bounds.width.pp}, h:${line.bounds.height.pp} ${line.bounds.prettyPrint} > ${line.toText}")
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
    pageId <- zoneIndexer.getPages
  } yield {
    val page = zoneIndexer.getPageInfo(pageId)
    val lls = page.getComponentsWithLabel(LB.VisualLine)

    lls
  }


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

  def labelAbstract(): Unit = {
    val vlines = zoneIndexer.bioSpine("TextBlockSpine")
    for {
      lineBioNode <- vlines.take(100) //  magic # ~= page 1
      lineComp = lineBioNode.component
      lineText = lineComp.chars

      isAbstractHeader = ! """(?i:(abstract|a b s t r a c t))""".r.findAllIn(lineText).isEmpty

      if isAbstractHeader

    } {
      println(s"found abstract header: ${lineText}")
      // zoneIndexer.addBioLabels(LB.SectionHeadingLine, lineBioNode)
      zoneIndexer.addBioLabels(LB.AbstractHeading, lineBioNode)

    }
  }

  def labelSectionHeadings(): Unit = {
    val vlines = zoneIndexer.bioSpine("TextBlockSpine")
    for {
      lineBioNode <- vlines
      lineComp = lineBioNode.component
      numbering = lineComp.chars.takeWhile(c => c.isDigit || c=='.')
      nexts = lineComp.chars.drop(numbering.length) //.takeWhile(c => c.isLetter)
      isTOCLine = """\.+""".r.findAllIn(nexts).toSeq.sortBy(_.length).lastOption.exists(_.length > 4)

      if !numbering.isEmpty && !nexts.isEmpty()
      if numbering.matches("^[1-9]+\\.([1-9]\\.)*")
      if !isTOCLine
    } {
      val ns = numbering.split("\\.").toList.map(_.toInt)
      zoneIndexer.addBioLabels(LB.SectionHeadingLine, lineBioNode)


      (lineComp, ns)
    }

    // val numberedSectionLines = for {
    //   page <- visualLineOnPageComponents


    // println("sorted sections ")
    // sortedSections.foreach { case (line, ns) =>
    //   val rline = renderConnectedComponents(line)
    //   val nss = ns.mkString(".")
    //   println(s"${nss}   ${rline}")
    // }

    // zoneIndexer.addLabels(zl: ZoneAndLabel)
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



  // def groupVisualTextBlocks(
  //   colX: Double, textRectCandidates: Seq[Component], remainingPageLines: Seq[Component]
  // ): Seq[Seq[Component]] = {
  //   val unusedLines = mutable.ArrayBuffer[Component](remainingPageLines:_*)
  //   val usedLines = mutable.ArrayBuffer[Component]()

  //   val ySortedLines = textRectCandidates.sortBy(_.bounds.top)
  //   val topLine = ySortedLines.head
  //   // val bottomLine = ySortedLines.last


  //   val possibleCand = remainingPageLines
  //     .diff(ySortedLines)
  //     .filterNot(candidateIsOutsideLineBounds(_, topLine))

  //   val commonJumps = pageSegAccum.commonFocalJumps


  //   val totalLinesSorted =  (possibleCand ++ ySortedLines).sortBy(_.bounds.top)

  //   totalLinesSorted.
  //     sliding(2).toSeq
  //     .map({
  //       case Seq(upper, lower) =>
  //         val jump = focalJump(upper, lower)
  //         val ujumps = commonJumps(upper.bounds.width.pp)
  //         val ljumps = commonJumps(lower.bounds.width.pp)
  //         val jumps = ujumps ++ ljumps

  //         if (jumps contains jump.pp) {
  //           println("common jump found")
  //         }

  //       case Seq(lone) => 0d
  //       case Seq() => 0d
  //     })

  //   // Walk down column lines pair-wise, and take while diagonal distances match


  //   // zoneIndexer.concatComponents(totalLineSorted, LB.Block)
  //   // totalLineSorted

  //   ???
  // }

}
