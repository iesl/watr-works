package edu.umass.cs.iesl.watr
package segment

import java.io.InputStream
import java.net.URI
import watrmarks._
import spindex._
import GeometricFigure._

import scalaz.@@
import TypeTags._
import scala.collection.JavaConversions._
import textboxing.{TextBoxing => TB}, TB._
import EnrichGeometricFigures._
import ComponentOperations._
import ComponentRendering._

import utils._
import utils.{CompassDirection => CDir}
import utils.VisualTracer._
import utils.EnrichNumerics._
import SlicingAndDicing._

import scala.collection.mutable

import predsynth._

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

  def approxSortYX(charBoxes: Seq[Component]): Seq[Component] = {
    charBoxes.sortBy({ c =>
      (c.bounds.top, c.bounds.left)
    })
  }


  def squishb(charBoxes: Seq[Component]): String = {
    approxSortYX(charBoxes)
      .map({ cbox => cbox.chars })
      .mkString
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

  // (charBoxesBounds(sortedXLine), sortedXLine)
  def approximateLineBins(charBoxes: Seq[AtomicComponent]): Seq[Seq[AtomicComponent]] = {
    val sortedYPage = charBoxes
      .groupBy(_.bounds.bottom.pp)
      .toSeq
      .sortBy(_._1.toDouble)

    sortedYPage
      .map({case (bottomY, charBoxes) =>
        charBoxes.sortBy(_.bounds.left)
      })

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



  import extract.fonts.SplineFont

  def createSegmenter(srcUri: URI, pdfins: InputStream, glyphDefs: Seq[SplineFont.Dir]): DocumentSegmenter = {
    val chars = formats.DocumentIO.extractChars(pdfins, Set(), glyphDefs)
    createSegmenter(srcUri, chars.map(c => (c._1.regions, c._2)))
  }


  def createSegmenter(srcUri: URI, pagedefs: Seq[(Seq[PageAtom], PageGeometry)]): DocumentSegmenter = {
    val zoneIndex = ZoneIndexer.loadSpatialIndices(srcUri, pagedefs)
    new DocumentSegmenter(zoneIndex)
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
  def vtrace = zoneIndexer.vtrace

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
      val charAtoms = zoneIndexer.getPageIndex(pageId).getPageAtoms
      vtrace.trace(message(s"runLineDetermination() on page ${pageId} w/ ${charAtoms.length} char atoms"))

      determineLines(pageId, charAtoms)
    }

  }


  // def docWideModalParaFocalJump(
  //   alignedBlocksPerPage: Seq[Seq[BioNode]],
  //   modalVDist: Double
  // ): Double = {
  //   val allVDists = for {
  //     groupedBlocks <- alignedBlocksPerPage
  //     block <- groupedBlocks
  //   } yield {
  //     // block
  //     //   .sliding(2).toSeq
  //     //   .map({
  //     //     case Seq((a1, i1), (a2, i2)) =>
  //     //       val a1Left = a1.bounds.toPoint(CDir.W).y
  //     //       val a2Left = a2.bounds.toPoint(CDir.W).y
  //     //       val vdist = math.abs(a1Left - a2Left)
  //     //       math.abs(vdist)
  //     //     case Seq((a1, i1)) => -1
  //     //     case Seq() => -1
  //     //   }).filter(_ > 0d)
  //   }
  //   // getMostFrequentValues(allVDists.flatten, leftBinHistResolution)
  //   //   .headOption.map(_._1)
  //   //   .getOrElse(12.0)
  //   15.0d
  // }


  def docWideModalLineVSpacing(alignedBlocksPerPage: Seq[Seq[Seq[Component]]]): Double = {
    val allVDists = for {
      groupedBlocks <- alignedBlocksPerPage
      block <- groupedBlocks
    } yield {
      block
        .sliding(2).toSeq
        .map({
          case Seq(a1, a2) =>
            val a1Left = a1.bounds.toPoint(CDir.SW).y
            val a2Left = a2.bounds.toPoint(CDir.SW).y
            val vdist = math.abs(a1Left - a2Left)
            vdist

          case Seq(a1) => -1
          case Seq() => -1
        }).filter(_ > 1.0d)
    }

    vtrace.trace(message("Compute docWideModalLineVSpacing"))
    val modalVDist = getMostFrequentValues(vtrace)(allVDists.flatten, leftBinHistResolution)
      .headOption
      .getOrElse(12.0)


    modalVDist

  }

  val leftBinHistResolution = 1.0d


  def findLeftAlignedBlocksPerPage(): Seq[Seq[Seq[Component]]] = {
    vtrace.trace(begin("findLeftAlignedBlocksPerPage"))

    val alignedBlocksPerPage = for {
      page <- visualLineOnPageComponents
    } yield {

      val lefts = page.zipWithIndex
        .map({case (l, i) => (l.bounds.left, i)})

      vtrace.trace(message(s"Most frequent left-edge text alignment"))
      val leftsAndFreqs = getMostFrequentValuesAndFreqs(vtrace)(lefts.map(_._1), leftBinHistResolution)

      val commonLeftEdges = leftsAndFreqs.takeWhile(_._2 > 1.0)


      def valueIsWithinHistBin(bin: Double, res: Double)(value: Double): Boolean = {
        bin-res <= value && value <= bin+res
      }


      def minAtomId(c: Component): Int = {
        c.atoms.map(_.id.unwrap).min
      }

      val groupedBlocks = page
        .sortBy(minAtomId(_))
        .groupByPairs({ case (line1, line2) =>


          val linesAreClustered = commonLeftEdges.exists({ case (leftBin, _) =>
            val line1InBin = valueIsWithinHistBin(leftBin, leftBinHistResolution)(line1.left)
            val line2InBin = valueIsWithinHistBin(leftBin, leftBinHistResolution)(line2.left)

            val areClustered = line1InBin && line2InBin
            areClustered
          })

          val h1 = line1.height
          val h2 = line2.height
          // val heightsDiffer = h1 != h2
          // val heightsDiffer = ! h1.eqFuzzy(0.2)(h2)
          val areGrouped = linesAreClustered //  && !heightsDiffer

          vtrace.trace(message(
            s"""| ${areGrouped} Group ${VisualLine.render(line1)} +??+ ${VisualLine.render(line2)}
                |     l1.left: ${line1.left}        .height: ${h1}
                |     l2.left: ${line2.left}        .height: ${h2}
                |     linesAreClustered=${linesAreClustered}
                |""".stripMargin
          ))

          areGrouped
        })

      vtrace.trace({
        val blocks = groupedBlocks.map{ block =>
          block.map(VisualLine.render(_))
            .mkString("Block\n    ", "\n    ", "\n")
        }
        val allBlocks = blocks.mkString("\n  ", "\n  ", "\n")

        message(allBlocks)
      })
      groupedBlocks
    }

    vtrace.trace(end("findLeftAlignedBlocksPerPage"))

    alignedBlocksPerPage

  }

  def splitBlocksWithLargeVGaps(
    alignedBlocksPerPage: Seq[Seq[Seq[Component]]],
    modalVDist: Double
  ): Seq[Seq[Seq[Component]]] = {
    // One last pass through block to split over-large vertical line jumps
    for {
      blocksOnPage <- alignedBlocksPerPage
      block <- blocksOnPage
    } yield {
      block.splitOnPairs ({ case (a1, a2) =>
        val vdist = a1.vdist(a2)
        val maxVDist = modalVDist * 1.4d
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

  def determineVisualLineOrdering(): Unit = {
    vtrace.trace(begin("determineVisualLineOrdering"))
    val alignedBlocksPerPage = findLeftAlignedBlocksPerPage()

    val modalVDist = docWideModalLineVSpacing(alignedBlocksPerPage)

    val finalSplit = splitBlocksWithLargeVGaps(alignedBlocksPerPage, modalVDist)

    // Now create a BIO labeling linking visual lines into blocks
    val bioLabels = finalSplit
      .flatten
      .map({ block =>
        val bios = block.map(BioNode(_))
        zoneIndexer.addBioLabels(LB.TextBlock, bios)

        // each block is a list of line components that have been grouped into a text block
        // vtrace.trace(
        //   vtrace.link(
        //     vtrace.all(
        //       bios.map(b => vtrace.showComponent(b.component))
        //     ),
        //     vtrace.showLabel(LB.TextBlock)
        //   )
        // )

        bios
      })

    val lineBioLabels = zoneIndexer.bioLabeling("LineBioLabels")
    lineBioLabels ++= bioLabels.flatten

    val blocks = selectBioLabelings(LB.TextBlock, lineBioLabels)

    // val modalParaFocalJump = docWideModalParaFocalJump(blocks, modalVDist)

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
    vtrace.trace({
      val blockStrs = blocks.map{ block =>
        block.map(b => VisualLine.render(b.component))
          .mkString("Block\n    ", "\n    ", "\n")
      }
      val allBlocks = blockStrs.mkString("\n  ", "\n  ", "\n")

      "Final Block Structure" withTrace message(allBlocks)
    })
    vtrace.trace(end("determineVisualLineOrdering"))

  }


  def runPageSegmentation(): Unit = {
    vtrace.trace(
      begin("SetPageGeometries"),
      setPageGeometries(zoneIndexer.pageInfos.map(_._2.geometry).toSeq),
      end("SetPageGeometries")
    )


    // Bottom-up connected-component line-finding
    vtrace.trace(begin("runLineDetermination"))
    runLineDetermination()
    vtrace.trace(end("runLineDetermination"))

    tokenizeLines()

    vtrace.trace(begin("determineVisualLineOrdering"))
    determineVisualLineOrdering()
    vtrace.trace(end("determineVisualLineOrdering"))

    labelTitle()
    labelAuthors()
    labelAbstract()
    labelSectionHeadings()

    joinLines()

  }

  // force tokenization of all visual lines
  def tokenizeLines(): Unit = for {
    page <- visualLineOnPageComponents
    (line, linenum) <- page.zipWithIndex
  } {
    line.tokenizeLine
  }

  def show(c: Component): TB.Box = {
    VisualLine.render(c).map(t => t.text.box).getOrElse("<could not render>".box)
  }



  import textflow._

  def alignPredSynthPaper(paper: Paper): Unit = {
    println("aligning predsynth paper ")

    val lineBioLabels = zoneIndexer.bioLabeling("LineBioLabels")

    val lineTextAndUnits = for {
      linec <- lineBioLabels
      line   <- VisualLine.renderWithoutFormatting(linec.component).toSeq
    } yield {

      val ltext = line.text

      val lunits = line.flow.flatMap(funit =>
        (0 until funit.length).map(_ => "*").mkString)

      val lineUnits = line.flow.flatMap(funit =>
        (0 until funit.length).map(_ => funit))

      if(ltext.length != lunits.length) {
        val lflow = line.flow.mkString("\n    ", "\n    ", "\n")

        println(s"""flow/text lengths are unequal""")
        println(s"""${ltext}""")
        println(s"""${lflow}""")
      }

      (ltext, lineUnits)
    }

    val plaintext = lineTextAndUnits.map(_._1).mkString
    val lineUnits = lineTextAndUnits.flatMap(_._2)

    println(s"text len = ${plaintext.length()} lineunits len = ${lineUnits.length}")

    // left=error, right=(begin, end) str offsets
    // val contexts: Seq[Either[(RawTextContext, String), (RawTextContext, (Int, Int))]]
    val contexts: Seq[AlignedGroup]
      = PredsynthLoad.alignContexts(paper, plaintext)

    val mongoIdToClusterId = mutable.HashMap[String, Int@@ClusterID]()
    val rawTextMentionsById = mutable.HashMap[Int@@MentionID, RawTextContext]()

    val relations = mutable.ArrayBuffer[Relation.Record]()
    val props = mutable.ArrayBuffer[Relation.PropRec]()

    import utils.IdGenerator

    val relationIds = IdGenerator[RelationID]()
    // val mentionIds = IdGenerator[MentionID]()
    val clusterIds = IdGenerator[ClusterID]()



    // idGen.nextId
    contexts.foreach({ alignedGroup: AlignedGroup =>
      val groupNumber = alignedGroup.textMentionGroup.groupNumber
      val id = alignedGroup.textMentionGroup.id
      val groupClusterID = clusterIds.nextId
      id.foreach { mongoId =>
        mongoIdToClusterId.put(mongoId, groupClusterID)
      }


      val groupCluster = Relation.Elem.Cluster(groupClusterID)

      props += Relation.PropRec(
        groupCluster,
        Relation.PropKV(
          "role",
          Relation.Prop.Str("recipe/"+alignedGroup.groupType)
        )
      )

      props += Relation.PropRec(
        groupCluster,
        Relation.PropKV(
          "mongoId",
          Relation.Prop.Str(id.getOrElse("null"))
        )
      )

      props ++= alignedGroup.textMentionGroup.props.map(Relation.PropRec(groupCluster, _))

      alignedGroup.alignedContexts.foreach {
        case AlignSuccess(rtc, (begin, end)) =>

          // val rawTextMentionId = mentionIds.nextId
          // rawTextMentionsById.put(rawTextMentionId, rtc)

          val slice = lineUnits.slice(begin, end)
          val foundText = slice.map({ funit =>
            TextFlow.toText(funit)
          }).mkString


          val targetRegions = slice.collect({
            case u: FlowUnit.Atom =>
              val cc = u.atomicComponent
              cc.pageAtom.region

            case u: FlowUnit.Rewrite =>
              val cc = u.atom.atomicComponent
              cc.pageAtom.region
          })


          val intersectedVisualLines  = targetRegions.map{ targetRegion =>
            val pageIndex = zoneIndexer.getPageIndex(targetRegion.target)

            pageIndex.componentIndex
              .queryForIntersects(targetRegion.bbox)
              .filter(_.hasLabel(LB.VisualLine))
              .headOption
              .getOrElse { sys.error(s"no visual line found intersecting ${targetRegion}") }

          }

          val uniqVisualLines = intersectedVisualLines
            .groupByPairs ({ case (c1, c2) => c1.id == c2.id })
            .map(_.head)


          val ann = LB.Annotation(rtc.textType)

          val annotationRegions = uniqVisualLines.map{visualLine =>
            val pageForLine = visualLine.pageId
            val pageRegions = targetRegions.filter(_.target == visualLine.pageId)
            // Select the span for each line that corresponds to labeled region
            val intersectingLineAtoms = visualLine.queryAtoms()
              .dropWhile({ lineAtom: AtomicComponent =>
                val haveIntersection = pageRegions.exists { _.bbox.intersects(lineAtom.bounds) }
                  !haveIntersection
              }).reverse
              .dropWhile({ lineAtom: AtomicComponent =>
                val haveIntersection = pageRegions.exists { _.bbox.intersects(lineAtom.bounds) }
                  !haveIntersection
              }).reverse


            zoneIndexer.labelRegion(intersectingLineAtoms, ann)
          }

          val annRegions = annotationRegions.flatten.map{_.targetRegion}
          val newZone = Zone(ZoneID(0), annRegions,ann)
          val zAdded = zoneIndexer.addZone(newZone)
          val mentionId = MentionID(zAdded.id.unwrap)

          rawTextMentionsById.put(mentionId, rtc)

          relations += Relation.Record(RelationID(zAdded.id.unwrap),
            groupCluster,
            "hasMember",
            Relation.Elem.Mention(mentionId)
          )

          if (rtc.rawText.raw_text == foundText) {
            println(s"   > g:${groupNumber} ${id} >> ${rtc.toString()}")
          } else {
            println(s"***> g:${groupNumber} ${id} >> ${rtc.toString()}  ===>  ${foundText}")
          }

        case AlignFailure(rawTextContext, message) =>
          println(s"!!!> ${rawTextContext.toString()} ${message}")

      }
    })

    paper.connections
      .flatten.foreach({ connection =>
        (connection.id1, connection.id2) match {
          case (Some(id1), Some(id2)) =>
            val group1 = mongoIdToClusterId(id1)
            val group2 = mongoIdToClusterId(id2)

            relations += Relation.Record(relationIds.nextId,
              Relation.Elem.Cluster(group1),
              "connectsTo",
              Relation.Elem.Cluster(group2)
            )

          case (None, Some(id2)) =>
            val group2 = mongoIdToClusterId(id2)
            println(
              s"""errata: (?? `connectsTo` group:${group2})""")
          case (Some(id1), None) =>
            val group1 = mongoIdToClusterId(id1)
            println(
              s"""(group:${group1} `connectsTo` ??)""")
          case _ =>
        }
      })

    zoneIndexer.addRelations(relations)
    zoneIndexer.addProps(props)

  }

  def joinLines(): Unit = {
    vtrace.trace(begin("JoinLines"))

    val lineBioLabels = zoneIndexer.bioLabeling("LineBioLabels")
    val textBlocks = selectBioLabelings(LB.TextBlock, lineBioLabels)

    textBlocks.flatten.foreachPair({(line1Node, line2Node) =>
      val line1 = line1Node.component
      val line2 = line2Node.component
      val wordBreaks = "-"
      val lineMinLenReq = line1.chars.length > 10 // magic # for minimum line length
      val hasNonLetterPrefix = line1.chars.reverse.drop(1).take(2).exists { !_.isLetter  }
      val endsWithDash = line1.chars.lastOption.exists { wordBreaks contains _ }
      val isBrokenWord = endsWithDash && lineMinLenReq && !hasNonLetterPrefix

      if (isBrokenWord) {

        val wordHalfFirst = line1.getChildren(LB.TextSpan).lastOption
        val wordHalfSecond = line2.getChildren(LB.TextSpan).headOption

        vtrace.trace("Broken word found" withTrace all(Seq(
          showComponent(line1), // endToken.map(showComponent(_)),
          showComponent(line2), // startToken.map(showComponent(_)),
          message(show(line1)), message(show(line2)),
          showComponents(wordHalfFirst.toSeq++wordHalfSecond.toSeq)
        )))

        (wordHalfFirst, wordHalfSecond) match {
          case (Some(firstHalf), Some(secondHalf)) =>
            val w1 = firstHalf.chars.dropRight(1)
            val w2 = secondHalf.chars.reverse.dropWhile(!_.isLetter).reverse
            val w2Extra = secondHalf.chars.reverse.takeWhile(!_.isLetter).reverse

            val word = w1 + w2

            val dict = utils.EnglishDictionary.global
            if (dict.contains(word)) {
              vtrace.trace("Broken word joined:" withInfo(word))
              // join word
              val joinedWord = LB.LineBreakToken(word)
              firstHalf.addLabel(joinedWord)
              secondHalf.addLabel(LB.Invisible)
              // if (w2Extra.isEmpty()) {
              // } else {
              //   // TODO add back the extra final punctuation w2Extra
              // }
            } else if (dict.contains(w1) && dict.contains(w2)) {
              // join word, but retain hyphen
              val wjoin = s"${w1}-${w2}"
              vtrace.trace("Broken word hyphenated:" withInfo(wjoin))
              val joinedWord = LB.LineBreakToken(wjoin)
              firstHalf.addLabel(joinedWord)
              // if (w2Extra.isEmpty()) {
              // } else {
              //   // TODO add back the extra final punctuation w2Extra
              // }
              secondHalf.addLabel(LB.Invisible)
            }

          case _ =>
            vtrace.trace(message(s"couldn't find broken word continuation"))

        }
      }
    })

    vtrace.trace(end("JoinLines"))

  }


  val withinAngle = filterAngle(docOrientation, math.Pi / 3)

  def fillInMissingChars(pageId: Int@@PageID, lineBinChars: Seq[AtomicComponent]): Seq[Component] = {
    // val consecutiveCharGroups = lineBinChars.groupByPairs({ (ch1, ch2) =>
    //   ch2.id.unwrap - ch1.id.unwrap == 1
    // })
    val ids = lineBinChars.map(_.id.unwrap)
    val minId = ids.min
    val maxId = ids.max

    val missingIds = (ids.min to ids.max) diff ids

    val missingChars = missingIds.map(id =>
      zoneIndexer.getComponent(ComponentID(id), pageId)
    )

    vtrace.trace("inserting missing chars" withTrace
      all(missingChars.map(showComponent(_))))

    (lineBinChars ++ missingChars).sortBy(_.bounds.left)
  }


  def splitRunOnLines(lineBins: Seq[Seq[AtomicComponent]]): Seq[Seq[AtomicComponent]] = {
    for {
      lineBin <- lineBins
      shortLine <- lineBin.splitOnPairs({ (ch1, ch2) =>
        val largeIdGap = ch2.id.unwrap - ch1.id.unwrap > 10
        if (largeIdGap) {
          vtrace.trace("splitting chars" withTrace link(showComponent(ch1), showComponent(ch2)))
        }
        largeIdGap
      })
    } yield shortLine
  }

  def gutterDetection(
    pageId: Int@@PageID,
    components: Seq[AtomicComponent]
  ): Unit = {
    vtrace.trace(begin("GutterDetection"))
    vtrace.trace(message(s"page ${pageId}"))
    // Find most common left Xs
    // starting w/most frequent left-x (lx0), try to construct the largest whitespace boxes
    // with right-x=lx0-epsilon, left=?
    // line-bin coarse segmentation
    val lefts = components.map(_.left)
    val hist = histogram(lefts, 0.1d)
    val freqLefts = hist.getFrequencies
      .sortBy(_.frequency)
      .reverse
      .takeWhile(_.frequency > 2.0)

    vtrace.trace(vtraceHistogram(hist))

    val epsilon = 0.01d
    freqLefts.foreach({leftXBin =>
      val leftX = leftXBin.value


      // LTBounds(
      //   left=leftX-epsilon,
      //   top=pageMin,
      //   width=
      // )
    })

    vtrace.trace(end("GutterDetection"))
  }

  def determineLines(
    pageId: Int@@PageID,
    components: Seq[AtomicComponent]
  ): Unit = {


    gutterDetection(pageId, components)

    def regionIds(cc: Component): Seq[Int@@RegionID] = cc.targetRegions.map(_.id)
    def minRegionId(ccs: Seq[Component]): Int@@RegionID =  ccs.flatMap(regionIds(_)).min

    val lineSets = new DisjointSets[Component](components)

    // line-bin coarse segmentation
    val lineBinsx = approximateLineBins(components)


    vtrace.trace("Visual lines, first approximation" withInfo
      vcat(lineBinsx.map(ccs =>
        ccs.map(_.chars).mkString.box
      )))

    val shortLines = splitRunOnLines(lineBinsx)

    for {
      line <- shortLines.map(fillInMissingChars(pageId, _))
      if !line.isEmpty
      char <- line
    } { lineSets.union(char, line.head) }


    val shortLinesFilledIn = lineSets.iterator().toSeq
      .map(_.toSeq.sortBy(c => (c.bounds.left, c.bounds.top)))
      .sortBy(line => minRegionId(line))

    vtrace.trace("Vis. Lines, second approx." withInfo
      vcat(shortLinesFilledIn.map(ccs =>
        ccs.map(_.chars).mkString.box
      )))


    val longLines = shortLinesFilledIn
      .groupByPairs({ (linePart1, linePart2) =>
        val l1 = linePart1.last
        val l2 = linePart2.head
        val l1max = l1.targetRegion.id.unwrap
        val l2min = l2.targetRegion.id.unwrap
        val idgap = l2min - l1max
        val leftToRight = isStrictlyLeftToRight(l1, l2)
        val overlapped = isOverlappedVertically(l1, l2)
        val smallIdGap = idgap < 5
        vtrace.trace("maybe join line parts" withInfo
          s"idgap:${idgap}, left2right:${leftToRight}, overlapped:${overlapped}")

        val shouldJoin = leftToRight && overlapped && smallIdGap

        shouldJoin
      })

    val pageLines = longLines.map({ lineGroups =>
      // println("Line Groups-----")
      // lineGroups.foreach { lgrp =>
      //   println("---Line Group-----")
      //   lgrp.foreach { l =>
      //     println("----line")
      //     println(l)
      //   }
      // }


      val line = lineGroups.reduce(_ ++ _)

      val uio = line.map(_.chars).mkString(", ")

      vtrace.trace("debug starting line atoms" withInfo
        s""" ${uio} """)

      // Glue together page atoms into a VisualLine/TextSpan
      zoneIndexer.labelRegion(line, LB.VisualLine)
        .map ({ visualLine =>
          visualLine.setChildren(LB.PageAtom, line)
          visualLine.connectChildren(LB.PageAtom, Some(_.bounds.left))

          val asdf = visualLine.atoms.map(_.chars).mkString(", ")
          vtrace.trace("debug vline atoms" withInfo
            s""" ${asdf} """)

          val textSpan = visualLine.cloneAndNest(LB.TextSpan)
          val qewr = textSpan.atoms.map(_.chars).mkString(", ")
          vtrace.trace("debug textSpan atoms" withInfo
            s""" ${qewr} """)

          vtrace.trace("Ending Tree" withInfo VisualLine.renderRoleTree(visualLine))
          visualLine
        })
    })

    zoneIndexer
      .labelRegion(pageLines.flatten, LB.Page)
      .map(_.connectChildren(LB.VisualLine, None))
  }


  def lineWidthMatches(line: Component, width: Double): Boolean  = {
    // line.determineNormalTextBounds.width.eqFuzzy(0.5d)(width)
    line.width.eqFuzzy(0.5d)(width)
  }
  def lineHeightMatches(line: Component, height: Double): Boolean  = {
    // line.determineNormalTextBounds.height.eqFuzzy(0.5d)(height)
    line.height.eqFuzzy(0.5d)(height)
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
          println("  "*indent + s"w:${line.bounds.width.pp}, h:${line.bounds.height.pp} ${line.bounds.prettyPrint} > ${line.chars}")
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
    val page = zoneIndexer.getPageIndex(pageId)
    val lls = page.getComponentsWithLabel(LB.VisualLine)

    lls.sortBy { _.bounds.top }
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

    // println(s""" focalJumps: ${jcol} """)

    pageSegAccum = pageSegAccum.copy(
      commonFocalJumps = focalJumps
    )
  }



  def findMostFrequentLineDimensions():  Unit = {

    val allPageLines = for {
      p <- visualLineOnPageComponents; l <- p
    } yield l

    val allDocumentWidths = allPageLines.map(_.bounds.width)

    val topWidths = getMostFrequentValuesAndFreqs(vtrace)(allDocumentWidths, 0.2d).toList
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
    vtrace.trace(begin("LabelAbstract"))
    // find the word "abstract" in some form, then,
    // if the text block containing "abstract" is a single line,
    //    take subsequent text blocks until we take a multiline
    // else if the text block is multi-line, take that block to be the entire abstract
    val lineBioLabels = zoneIndexer.bioLabeling("LineBioLabels")
    val blocks = selectBioLabelings(LB.TextBlock, lineBioLabels)

    vtrace.trace(message(s"TextBlock count: ${blocks.length}"))

    val maybeLookingAtAbstract = blocks.dropWhile { tblines =>
      tblines.headOption.exists { l1 =>
        val lineComp = l1.component
        val lineText = lineComp.chars
        val isAbstractHeader =  """^(?i:(abstract|a b s t r a c t))""".r.findAllIn(lineText).length > 0
          !isAbstractHeader
      }
    }


    if (maybeLookingAtAbstract.length > 0) {
      val firstBlockIsMultiline = maybeLookingAtAbstract.headOption.exists(_.length > 1)
      if (firstBlockIsMultiline) {
        // label this as the abstract
        maybeLookingAtAbstract.headOption.foreach { abs =>
          zoneIndexer.addBioLabels(LB.Abstract, abs)

          vtrace.trace("Found Abstract in multi-line block" withTrace
            all(abs.map(b => showComponent(b.component))))
        }
      } else {
        val singleLines = maybeLookingAtAbstract.takeWhile { tblines =>
          tblines.length == 1
        }
        val absBlock = maybeLookingAtAbstract.drop(singleLines.length).headOption.getOrElse(Seq())
        val totalABs = singleLines :+ absBlock

        zoneIndexer.addBioLabels(LB.Abstract, totalABs.flatten)

        vtrace.trace("Found Abstract in single-line block" withTrace all(
          totalABs.flatMap(_.map(b => showComponent(b.component)))
        ))


      }
    } else {
      // println("abstract is unlabled")
      // abstract is unlabled, look for the first multi-line text block before the intro?
      var found = false
      val allBlocksBeforeIntro = blocks.takeWhile { tblines =>
        tblines.headOption.exists { l1 =>
          // l1 is a BioNode
          val lineComp = l1.component
          val lineText = lineComp.chars
          //todo: might accidentally mistake title for introduction - throwing out multi-work lines to fix this
          val isIntroHeader =
          {"""introduction""".r.findAllIn(lineText.toLowerCase).length > 0 && lineText.split(" ").length == 1}
          if(isIntroHeader){
            //println("found intro header")
            found = true
          }
          !isIntroHeader
        }
      }
      // todo: fix so that it checks for other labels
      // todo: handle case where the intro isn't labeled either (should we attempt to label things in this case?)
      // find first multiline block before intro and label it as abstract, if it has no other labelings
      if (found && allBlocksBeforeIntro.length > 0) {
        //todo: this is for testing, remove eventually
        allBlocksBeforeIntro.foreach(block => {
          //block.foreach(node => println(node.component.toText))
          //println
        })

        val lastMultilineBeforeIntro = allBlocksBeforeIntro.lastIndexWhere(_.length > 3)
        if (lastMultilineBeforeIntro != -1) {
          // label this as the abstract
          allBlocksBeforeIntro.get(lastMultilineBeforeIntro).foreach { abs =>
            zoneIndexer.addBioLabels(LB.Abstract, abs)
            //println("labeling as part of abstract: " + abs.component.toText)
            // vtrace.trace(
            //   vtrace.link(
            //     vtrace.all(
            //       abs.map(b => vtrace.showComponent(b.component))
            //     ),
            //     vtrace.showLabel(LB.Abstract)
            //   )
            // )


          }
        }
      }
    }
    vtrace.trace(end("LabelAbstract"))
  }

  def labelSectionHeadings(): Unit = {
    vtrace.trace(begin("LabelSectionHeadings"))
    val lineBioLabels = zoneIndexer.bioLabeling("LineBioLabels")
    for {
      lineBioNode <- lineBioLabels
      lineComp = lineBioNode.component
      numbering = lineComp.chars.takeWhile(c => c.isDigit || c=='.')
      nexts = lineComp.chars.drop(numbering.length) //.takeWhile(c => c.isLetter)
      isTOCLine = """\.+""".r.findAllIn(nexts).toSeq.sortBy(_.length).lastOption.exists(_.length > 4)

      if !numbering.isEmpty && !nexts.isEmpty()
      if numbering.matches("^[1-9]+\\.([1-9]\\.)*")
      if !isTOCLine
    } {

      vtrace.trace("Labeled section heading" withInfo
        show(lineBioNode.component))

      zoneIndexer.addBioLabels(LB.SectionHeadingLine, lineBioNode)
    }

    vtrace.trace(end("LabelSectionHeadings"))

    // What are the predominant fonts per 'level'?
    // Distinguish between TOC and in-situ section IDs
    // look for rectangular blocks of text (plus leading/trailing lines)
    // look for for left-aligned (column-wise) single or double numbered text lines w/ large gaps
    // filter out figure/caption/footnotes based on embedded images and page locations
  }

  def labelTitle(): Unit = {
    val lineBioLabels = zoneIndexer.bioLabeling("LineBioLabels")
    // look for lines with biggest font within first [x] lines of paper
    // get rid of lines with length less than some arbitrary length (to weed out some weird cases)
    val biggestLineAtBeginning = lineBioLabels.take(50)
      .filter(_.component.chars.length() > 5)
      .sortWith(_.component.height > _.component.height)

    // for debugging, print out all the lines sorted in order from largest to smallest
//    biggestLineAtBeginning.foreach(node => println(node.component.chars))
//    println

    if(biggestLineAtBeginning.headOption.isDefined) {
      println("Title candidate: " + biggestLineAtBeginning.headOption.get.component.chars)
      zoneIndexer.addBioLabels(LB.Title, biggestLineAtBeginning.headOption.get)
      println
    } else {
      println("there isn't a biggest line?")
    }
  }


  def labelAuthors(): Unit = {
    val fnStream: InputStream = getClass().getClassLoader.getResourceAsStream("first_names.txt")
    val firstNames = scala.io.Source.fromInputStream(fnStream).getLines()
    val lnStream: InputStream = getClass.getClassLoader.getResourceAsStream("last_names.txt")
    val lastNames = scala.io.Source.fromInputStream(lnStream).getLines()

    val firstNameSet = firstNames.toSet
    val lastNameSet = lastNames.toSet

    val lineBioLabels = zoneIndexer.bioLabeling("LineBioLabels")

    val firstLines = lineBioLabels.filter(_.component.chars.length() > 5).take(8)

    // FIXME: integrate w/textflow and reinstate this block
    // for(lineNode <- firstLines) {
    //   val lineText = ComponentRendering.VisualLine.render(lineNode.component)
    //   if(!lineText.isEmpty) {
    //     val lines = lineText.get.lines.mkString(" ")
    //     val words = lines.split(" ")
    //     //words.foreach(println)
    //     //TODO: remove weird punctuation and super/subscripts for matching purpose (but keep them for later pattern matching?)
    //     val lowerLines = lines.toLowerCase()
    //     if(!lowerLines.contains("university") && !lowerLines.contains("department")
    //       && !lowerLines.contains("school") && !lowerLines.contains("college") && !lowerLines.contains("center")) {
    //       // first check for word in set of known names
    //       for (word <- words) {
    //         if ((firstNameSet.contains(word) || lastNameSet.contains(word))&& word.length > 1) {
    //           println("found " + word + " in the names corpus")
    //           zoneIndexer.addBioLabels(LB.Author, lineNode)
    //           println("labeling " + lines + " as author ")
    //         }
    //       }
    //     }
    //     // todo: use pattern matching to look for things like initials, etc. (figure how why this isn't working)
    //     val initial = """[A-Z]+\.+\s""".r
    //     if((initial findAllIn lines).length > 0) {
    //       println("found a possible initial in line " + lines)
    //       zoneIndexer.addBioLabels(LB.Author, lineNode)
    //       println("labeling " + lines + " as author ")
    //     }
    //   }
    // }
    // println()
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


  //   // zoneIndexer.connectComponents(totalLineSorted, LB.Block)
  //   // totalLineSorted

  //   ???
  // }

}
