package edu.umass.cs.iesl.watr
package table

import TypeTags._
import watrmarks._
import corpora._

import geometry._
import geometry.syntax._
import textgrid._
import utils.{RelativeDirection => Dir}
import utils.DoOrDieHandlers._
import utils._

import textboxing.{TextBoxing => TB}, TB._
import segment._
import spindex._

object AnnotationDiffs {
  import spindex._


  val R = RelationModel
  import TextGridFunctions._
  import scalaz.{@@ => _, _} //, Scalaz._
  import \&/._
  import scalaz.syntax.align._
  import scalaz.std.list._
  import scalaz.syntax.equal._

  val Other = Label.auto
  val AnyLabel = Label.auto
  val GridCellLabel = Label.auto
  val GridCellIndex = Label.auto
  val TextGridsAtPoint = Label.auto
  val LabeledCell = Label.auto
  val AnnotRecAttr = Label.auto
  val LabelIndicatorPoint = Label.auto
  val LabeledSpanAttr = Label.auto


  case class LabeledSpan(
    annotId: Int@@AnnotationID,
    label: Label,
    cells: Seq[TextGrid.GridCell],
    isGold: Boolean
  )

  case class IndicatorPointAttr(
    labeledSpan: LabeledSpan,
    cellIndex: Int,
    pinRepBox: TB.Box,
    spanRecIndex: Int
  )

  case class CellBioPinDiff(
    diffs: Seq[(BioPin, BioPin)]
  )

  sealed trait GridCellComparison {
    def i1: Int
    def i2: Int
  }

  object GridCellComparison {

    case class Diff(
      i1: Int,
      i2: Int,
      span1: LabeledSpan,
      span2: LabeledSpan,
      bioPinDiff: CellBioPinDiff
    ) extends GridCellComparison

    case class Same(
      i1: Int,
      i2: Int
    ) extends GridCellComparison
  }

  case class LabelSpanComparison(
    cellComparisons: Seq[GridCellComparison] = Seq(),
    alignedCells: List[TextGrid.GridCell \&/ TextGrid.GridCell] = List()
  )


  case class DocumentDiffRecords(
    stableId: String@@DocumentID,
    labelSpans: Seq[LabeledSpan],
    goldToNonGoldTable: TabularData[Int, Int, LabelSpanComparison]
  )



  private def initPageIndexes(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): Map[Int@@PageNum, PageIndex] = {
    val docStore = corpusAccessApi.docStore
    val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")
    docStore.getPages(docId).map { pageId =>
      val pageDef = docStore.getPageDef(pageId).orDie(s"no page def for ${pageId}")
      val pageGeometry = PageGeometry(
        pageDef.pagenum,
        pageDef.bounds
      )
      docStore.getPageGeometry(pageId)
      val pageIndex = new PageIndex(pageGeometry, Array(), 0, 0)
      (pageDef.pagenum, pageIndex)
    }.toMap
  }

  def bioPinsToBlockRep(pins: Seq[BioPin]): TB.Box = {
    vjoins(top, pins.map{ p =>
      val labelChar = p.label.fqn.head.toString()
      val pinRep = if (p.isUnit || p.isBegin) {
        labelChar.toUpperCase()
      } else {
        labelChar.toLowerCase()
      }
      pinRep.box
    })
  }

  def indexedCellsToBioBlock(cells: Seq[(TextGrid.GridCell, Int, Int)]): TB.Box = {
    hjoins(top, cells.map(c => bioPinsToBlockRep(c._1.pins)))
  }

  /**
    * All fine-grained labels for given document
    */
  def buildFineGrainedLabelTable(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): Seq[LabeledSpan] = {
    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore
    val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")

    val annots = annotApi.listDocumentAnnotations(docId)

    annots.flatMap { annotId =>
      val annotRec = annotApi.getAnnotationRecord(annotId)
      val isGold = annotRec.annotPath.exists { corpusPath =>
        corpusPath.unwrap.endsWith("UmaCziTeam")
      }
      annotRec.body.toList.flatMap { body =>
        body match {
          case AnnotationBody.TextGrid(textGridDef) =>
            val textGrid = TextGrid.fromJsonStr(textGridDef)
            val labelTree = textGridToLabelTree(textGrid)
            val labelSpanTree = labelTreeToSpanTree(labelTree)
            val gridCells = textGrid.indexedCells().map(_._1)

            labelSpanTree.flatten.flatMap { case (maybeLabel, start, len) =>
              maybeLabel.map { spanLabel =>
                val cells = gridCells.slice(start, start+len)
                LabeledSpan(annotId, spanLabel, cells, isGold)
              }
            }

          case _ => None
        }
      }
    }
  }

  val XNoLabel = Label.auto

  def compareIndicatedGridCell(i1: IndicatorPointAttr, i2: IndicatorPointAttr): Option[CellBioPinDiff] = {
    val cell1 = i1.labeledSpan.cells(i1.cellIndex)
    val cell2 = i2.labeledSpan.cells(i2.cellIndex)
    compareGridCells(cell1, cell2)
  }

  def compareGridCells(cell1: TextGrid.GridCell, cell2: TextGrid.GridCell, onlyCompare: Option[Label] = None): Option[CellBioPinDiff] = {

    val zippedPins = cell1.pins.zipAll(cell2.pins, XNoLabel.B, XNoLabel.B)


    val differingPins = zippedPins
      .filter{ case (p1, p2) =>
        onlyCompare.map { l =>
          p1.label == l && p1.toString() != p2.toString()
        } getOrElse {
          p1.toString() != p2.toString()
        }
      }

    // val debugCompare = zippedPins
    //   .map{ case (p1, p2) =>
    //     if (p1.toString() != p2.toString()) {
    //       s"✗ ${p1.toString()} !==  ${p2.toString()}".box
    //     } else {
    //       s"  ${p1.toString()} ===  ${p2.toString()}".box
    //     }
    //   }


    if (differingPins.nonEmpty) {
      // println(
      //   "Comparing BioPins (Different)".box.hangIndent(vjoin(
      //     vspace(1),
      //     vjoins(left, debugCompare),
      //     vspace(1),
      //   ))
      // )
      Option(CellBioPinDiff(
        differingPins
      ))
    } else None
  }

  def addLabelIndicatorPoints(labelSpans: Seq[LabeledSpan], pageIndexMap: Map[Int@@PageNum, PageIndex]): Unit = {
    for {
      (labelSpan, spanNum) <- labelSpans.zipWithIndex
    } for {
      (gridCell, cellNum) <- labelSpan.cells.zipWithIndex
      if gridCell.isGlyphCell
    } {
      val cellRegion = gridCell.pageRegion
      val pageNum = cellRegion.page.pageNum
      val pageIndex = pageIndexMap(pageNum)
      val cellBounds = gridCell.pageRegion.bbox
      val cellCenter = cellBounds.toPoint(Dir.Center)
      val pinRep = bioPinsToBlockRep(gridCell.pins)

      val queryBox = minBoundingRect(cellCenter)

      val hits = queryForIndicatorPoints(queryBox, pageIndex)

      if (hits.isEmpty) {
        val indicatorPt = pageIndex.shapes.indexShape(cellCenter, LabelIndicatorPoint)
        val attr = List( IndicatorPointAttr(labelSpan, cellNum, pinRep, spanNum) )
        pageIndex.shapes.setShapeAttribute(indicatorPt.id, LabeledSpanAttr, attr)

      } else {
        // assume(hits.length==1) TODO this should hold true


        // DEBUGGING CODE:
        if (hits.length > 1) {
          val allHitChars = hits.map{ hitShape =>
            val hitAttrs = getIndicatorPointAttrs(hitShape, pageIndex)
            // val hitAttrs = pageIndex.shapes.getShapeAttribute[List[IndicatorPointAttr]](hitShape.id, LabeledSpanAttr).get
            val hitChars = hitAttrs.map{ attr =>
              val cell = attr.labeledSpan.cells(attr.cellIndex)
              cell.char
            }
            val allGlyphs = hitAttrs.forall{ attr =>
              val cell = attr.labeledSpan.cells(attr.cellIndex)
              cell.isGlyphCell
            }
            if (allGlyphs) {
              hitChars.mkString("[", ",", "]")
            } else {
              hitChars.mkString("[!!:", ",", "]")
            }
          }
          val s = allHitChars.mkString(", ")
          println(s"Warning: Multiple Indictor Points found, n=${hits.length}, {  ${s} }")
        } // END DEBUGGING CODE:

        hits.foreach { indicatorPt =>
          val attr = IndicatorPointAttr(labelSpan, cellNum, pinRep, spanNum)
          appendIndicatorPointAttrs(indicatorPt, attr, pageIndex)
        }
      }
    }
  }

  implicit class RicherLabeledShape[A <: GeometricFigure](val theShape: LabeledShape[A]) {
    def asLineShape: LineShape = theShape.asInstanceOf[LineShape]
    def asPointShape: PointShape = theShape.asInstanceOf[PointShape]
    def asRectShape: RectShape = theShape.asInstanceOf[RectShape]
  }

  def getIndicatorPointAttrs(indicatorPoint: PointShape, pageIndex: PageIndex): List[IndicatorPointAttr] = {
    pageIndex.shapes.getShapeAttribute[List[IndicatorPointAttr]](indicatorPoint.id, LabeledSpanAttr).get
  }

  def appendIndicatorPointAttrs(indicatorPoint: PointShape, attr:IndicatorPointAttr, pageIndex: PageIndex): Unit = {
    val attrs = pageIndex.shapes.getShapeAttribute[List[IndicatorPointAttr]](indicatorPoint.id, LabeledSpanAttr).get
    val updated = attrs :+ attr
    pageIndex.shapes.setShapeAttribute(indicatorPoint.id, LabeledSpanAttr, updated)
  }

  protected def getLabeledPoints(l: Label, pageIndex: PageIndex): Seq[PointShape] = {
    pageIndex.shapes.getShapesWithLabel(l)
      .map(_.asPointShape)
  }

  protected def queryForIndicatorPoints(query: LTBounds, pageIndex: PageIndex): Seq[PointShape] = {
    pageIndex.shapes
      .searchShapes(query, LabelIndicatorPoint)
      .map(_.asPointShape)
  }

  def prettyPrintAlignmentKey(cells: List[TextGrid.GridCell \&/ TextGrid.GridCell]) : TB.Box = {
    val rows:Seq[Box] = cells.zipWithIndex.flatMap{ case (these, index) => these match {
      case This(a) => None
      case That(b) => None
      case Both(a, b) =>
        val cellComparison = compareGridCells(a, b)

        cellComparison match {
          case Some(diff) =>
            Some(
              vjoins(top,
                diff.diffs.map{ case (pin1, pin2) =>
                  s"${pin1} != ${pin2}".box
                }
              )
            )

          case None =>
            None

        }
    }}

    vjoins(left, rows)
  }

  def prettyPrintAlignedGridCells(cells: List[TextGrid.GridCell \&/ TextGrid.GridCell]) : TB.Box = {
    val alignedCols = cells.map{ _ match {
      case This(a) =>
        val pinCol = bioPinsToBlockRep(a.pins)
        vjoin(
          a.char.toString,
          "╌",
          "─",
          pinCol
        )
      case That(b) =>
        val pinCol = bioPinsToBlockRep(b.pins)
        vjoin(
          "╌".box,
          b.char.toString,
          "─",
          pinCol,
        )
      case Both(a, b) =>
        val pinColA = bioPinsToBlockRep(a.pins)
        val pinColB = bioPinsToBlockRep(b.pins)
        val cellComparison = compareGridCells(a, b)

        cellComparison match {
          case Some(diff) =>
            vjoin(
              a.char.toString,
              b.char.toString,
              "✗",
              pinColA,
              "─",
              pinColB,
            )

          case None =>
            vjoin(
              a.char.toString,
              b.char.toString,
              " ",
              pinColA,
            )

        }
    }}

    hjoins(top, alignedCols)
  }

  def alignGridCells(cells1: List[TextGrid.GridCell], cells2: List[TextGrid.GridCell]): List[TextGrid.GridCell \&/ TextGrid.GridCell] = {

    var cells2CommonIndex = -1
    val cells1CommonIndex = cells1.indexWhere { case cell1 =>
      cells2CommonIndex = cells2.indexWhere { case cell2 =>
        val bothGlyphs = cell1.isGlyphCell() && cell2.isGlyphCell()
        val eq = cell1.pageRegion.bbox === cell2.pageRegion.bbox
        bothGlyphs && eq
      }
      cells2CommonIndex >= 0
    }

    val (cells1Pre, cells1Common) = cells1.splitAt(cells1CommonIndex)
    val (cells2Pre, cells2Common) = cells2.splitAt(cells2CommonIndex)

    val preAligned = cells1Pre.reverse.align(cells2Pre.reverse).reverse
    val commonAligned = cells1Common.align(cells2Common)
    val aligned = preAligned ++ commonAligned


    // Double check:
    val thisLen = aligned.collect{ case This(_) => 0 }.length
    val thatLen = aligned.collect{ case That(_) => 0 }.length
    val bothLen = aligned.collect{ case Both(_, _) => 0 }.length

    assume(thisLen + bothLen == cells1.length)
    assume(thatLen + bothLen == cells2.length)

    aligned
  }



  def diffDocumentAnnots(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): DocumentDiffRecords = {

    val pageIndexMap = initPageIndexes(stableId)

    val allFineLabelsIndexed: Seq[(LabeledSpan, Int)] = buildFineGrainedLabelTable(stableId).zipWithIndex
    val allFineLabels: Seq[LabeledSpan] = allFineLabelsIndexed.map(_._1)

    val goldLabels = allFineLabelsIndexed.filter{ _._1.isGold }
    val nonGoldLabels = allFineLabelsIndexed.filterNot{ _._1.isGold}

    val goldToNonGoldTable = GuavaHelpers.initTable[Int, Int, LabelSpanComparison]()

    for {
      (gold, goldIndex) <- goldLabels
      (nonGold, nonGoldIndex) <- nonGoldLabels
    } { goldToNonGoldTable.set(goldIndex, nonGoldIndex, LabelSpanComparison()) }

    addLabelIndicatorPoints(allFineLabels, pageIndexMap)


    // Generate char-level diffs between overlapping labeled spans...
    // Record all label pairs that have diffs
    pageIndexMap.toList.sortBy(_._1).foreach { case (pageNum, pageIndex) =>

      getLabeledPoints(LabelIndicatorPoint, pageIndex).foreach { indicatorPoint =>

        val attrs = getIndicatorPointAttrs(indicatorPoint, pageIndex)
        val goldAttrs = attrs.filter(_.labeledSpan.isGold)
        val nonGoldAttrs = attrs.filterNot(_.labeledSpan.isGold)

        for {
          goldAttr <- goldAttrs
          nonGoldAttr <- nonGoldAttrs
        } {

          compareIndicatedGridCell(goldAttr, nonGoldAttr)  match {

            case Some(diff) =>

              val comparison = GridCellComparison.Diff(
                goldAttr.spanRecIndex, nonGoldAttr.spanRecIndex,
                allFineLabels(goldAttr.spanRecIndex), allFineLabels(nonGoldAttr.spanRecIndex),
                diff
              )

              goldToNonGoldTable.modify(goldAttr.spanRecIndex, nonGoldAttr.spanRecIndex, { cellDiffs =>
                cellDiffs.copy(cellComparisons = cellDiffs.cellComparisons :+ comparison)
              })

            case None =>

              val comparison = GridCellComparison.Same(goldAttr.spanRecIndex, nonGoldAttr.spanRecIndex)

              goldToNonGoldTable.modify(goldAttr.spanRecIndex, nonGoldAttr.spanRecIndex, { cellDiffs =>
                cellDiffs.copy(cellComparisons = cellDiffs.cellComparisons :+ comparison)
              })
          }
        }
      }
    }

    // Align label cells
    goldToNonGoldTable.modEach { spanComparison =>
      if (spanComparison.cellComparisons.nonEmpty) {
        val span1 = allFineLabels(spanComparison.cellComparisons.head.i1)
        val span2 = allFineLabels(spanComparison.cellComparisons.head.i2)

        val span1Cells = span1.cells.toList
        val span2Cells = span2.cells.toList
        val aligned = alignGridCells(span1Cells, span2Cells)

        spanComparison.copy(alignedCells = aligned)
      } else {
        spanComparison
      }

    }

    DocumentDiffRecords(
      stableId,
      allFineLabels,
      goldToNonGoldTable
    )
  }



  def summarizeDocument(diffRec: DocumentDiffRecords): Unit = {
    val stableId = diffRec.stableId

    val goldLabels = diffRec.labelSpans.filter(_.isGold)

    println(s"${diffRec.stableId} Total fine-grained label count = ${diffRec.labelSpans.length}, gold: ${goldLabels.length}")

    val goldUnmatched = diffRec.goldToNonGoldTable
      .mapRows(true)({ case (acc, e) => acc && e.cellComparisons.isEmpty })
      .filter(_._2)

    val goldMatched = diffRec.goldToNonGoldTable.mapRows(List[LabelSpanComparison]())({ case (acc, e) =>
      val hasOverlaps = e.cellComparisons.nonEmpty
      if (hasOverlaps)  acc :+ e
      else acc
    })

    val (goldMatchedCorrectly, goldMatchedWithErrs) = goldMatched.partition {
      case (rowk, comps: List[LabelSpanComparison]) =>
        comps.exists { spanComparison =>

          val allCellsSame = spanComparison.cellComparisons.forall(_.isInstanceOf[GridCellComparison.Same])
          val exactOverlap = spanComparison.alignedCells.forall(_.isBoth)

          allCellsSame && exactOverlap
        }
    }

    val goldSummary = s"Gold Matched Correctly: ${goldMatchedCorrectly.length}  Matched With Errors: ${goldMatchedWithErrs.length}  Unmatched: ${goldUnmatched.length}"

    // Report the error rate for this document without considering trivial differences
    val nonGoldUnmatched = diffRec.goldToNonGoldTable
      .mapColumns(true)({ case (acc, e) => acc && e.cellComparisons.isEmpty })
      .filter(_._2)

    val nonGoldSummary = s"Non-Gold Unmatched: ${nonGoldUnmatched.length}"

    val denominator = goldLabels.length + nonGoldUnmatched.length

    val errCount =  nonGoldUnmatched.length + goldUnmatched.length  + goldMatchedWithErrs.length
    val okCount = goldMatchedCorrectly.length

    val errFraction = "%3.2f".format(  (errCount.toDouble / denominator) * 100.0 )
    val okFraction = "%3.2f".format(  (okCount.toDouble / denominator) * 100.0 )

    val total = errFraction + okFraction

    val docSummary = s"Correct: ${okFraction} / Incorrect:${errFraction} = ${total}, denom: ${denominator} = (golds)${goldLabels.length} + (non-gold unmatch)${nonGoldUnmatched.length}"

    println(
      s"Table gold v. non-gold".box.hangIndent(vjoin(
        "Gold Summary".hangIndent(
          goldSummary,
        ),
        "Non-Gold Summary".hangIndent(
          nonGoldSummary,
        ),
        "Doc Summary".hangIndent(
          docSummary
        ),
      ))
    )

    type InvalidT = (Int, TB.Box)
    type ValidT  = (Int, LabelSpanComparison, TB.Box)
    // type ValidatedT  = Either[InvalidT, ValidT]

    // divide matched errs into correct/incorrect
    val divided: Seq[Either[(Int, TB.Box), (Int, LabelSpanComparison, TB.Box)]] = goldMatchedWithErrs
      .map { case (rowk: Int, spanComparisons: List[LabelSpanComparison]) =>
        val span1 = diffRec.labelSpans(rowk)
        val span1Index = rowk

        val matchingLabelComparisons = spanComparisons.map { spanComparison =>
          assume(spanComparison.cellComparisons.nonEmpty)
          val span1Index = spanComparison.cellComparisons.head.i1
          val span2Index = spanComparison.cellComparisons.head.i2
          assume(span1Index == rowk)
          val span2 = diffRec.labelSpans(span2Index)
          val l1 = span1.label.fqn
          val l2 = span2.label.fqn
          if (l1 == l2) Right(spanComparison) else Left(spanComparison)
        }

        val sameLabeled = matchingLabelComparisons.count { _.isRight }

        // val allComparisons: Ei = if (sameLabeled == 1) {
        // Final decision as to whether this is correctly labeled...

        val allComparisons: Either[(Int, TB.Box), (Int, LabelSpanComparison, TB.Box)] = if (sameLabeled == 1) {
          val spanComparison = matchingLabelComparisons.filter(_.isRight).head.right.get
          val span2Index = spanComparison.cellComparisons.head.i2
          // val span2 = diffRec.labelSpans(span2Index)
          // Compare these wrt. label
          // val label = span2.label.fqn
          val label = span1.label
          val aligned = spanComparison.alignedCells

          val labelsMatch = compareAlignedCellLabels(spanComparison.alignedCells, label)
          if (labelsMatch) {
            Right[InvalidT, ValidT]( (rowk, spanComparison,
              s"Exact Match: ${span1Index} / ${span2Index} Label = ${label}".box
            ) )
          } else {
            val alignmentBox = prettyPrintAlignedGridCells(aligned)
            val keyBox = prettyPrintAlignmentKey(aligned)

            Left((  rowk,
              s"Sole Inexact Matching Label: ${span1Index} / ${span2Index} Label = ${label} ${stableId}".box.hangIndent(vjoin(
                vspace(1),
                alignmentBox,
                vspace(1),
                keyBox
              )))
            )
          }

        } else if (sameLabeled > 1) {
          val validComparisons = matchingLabelComparisons.filter(_.isRight).map(_.right.get)

          val boxes = validComparisons.map{ spanComparison =>
            val span1Index = spanComparison.cellComparisons.head.i1
            val span2Index = spanComparison.cellComparisons.head.i2
            val span2 = diffRec.labelSpans(span2Index)
            val aligned = spanComparison.alignedCells

            val alignmentBox = prettyPrintAlignedGridCells(aligned)
            val keyBox = prettyPrintAlignmentKey(aligned)
            val l1 = span1.label.fqn
            val l2 = span2.label.fqn

            s"Duplicate Inexact Matching Label ${span1Index} / ${span2Index} Label = ${l1} / ${l2} ${stableId} ".box.hangIndent(vjoin(
              vspace(1),
              alignmentBox,
              vspace(1),
              keyBox
            ))
          }

          Left((rowk, vjoins(boxes)))
        } else {
          val validComparisons = matchingLabelComparisons.filter(_.isRight).map(_.left.get)

          val boxes = validComparisons.map{ spanComparison =>
            val span1Index = spanComparison.cellComparisons.head.i1
            val span2Index = spanComparison.cellComparisons.head.i2
            val span2 = diffRec.labelSpans(span2Index)
            val aligned = spanComparison.alignedCells

            val alignmentBox = prettyPrintAlignedGridCells(aligned)
            val keyBox = prettyPrintAlignmentKey(aligned)
            val l1 = span1.label.fqn
            val l2 = span2.label.fqn

            s"Non-Matching Labels Spans ${span1Index} / ${span2Index} Label = ${l1} / ${l2} ".box.hangIndent(vjoin(
              vspace(1),
              alignmentBox,
              vspace(1),
              // keyBox
            ))
          }
          Left((rowk, vjoins(boxes)))
        }

        val log = allComparisons.fold({ l =>
          l._2
        },  {r =>
          // s"Span ${rowk}, ${span1.label} vs..".box.hangIndent(vjoin(
          //   vspace(1),
          //   r._3,
          //   vspace(1),
          // ))
          nullBox
        })

        println(log)
        allComparisons

      }


    val fixedMatches = divided.count(_.isRight)
    // val adjustedErrCount =  errCount
    val adjustedOkCount = okCount + fixedMatches

    val okFraction0 = "%3.2f".format( (adjustedOkCount.toDouble / denominator) * 100.0 )

    val docSummary0 = s"Correct: ${okFraction0}; was: ${okFraction}"

    println(
      s"Adjusted Summary".box.hangIndent(vjoin(
        docSummary0
      ))
    )

    // val goldLabelCountsByLabel = diffRec.labelSpans
    //   .filter(_.isGold)
    //   .map{ span => (span.label, span) }
    //   .groupBy(_._1)


    // val totalDocumentLabelCounts = diffRec.labelSpans.map{ span => (span.label, span) }
    //   .groupBy(_._1).toList
    //   .map{ case (label, spans) =>

    //     val goldCount = goldLabelCountsByLabel.get(label)
    //       .map{ _.length }
    //       .getOrElse(0)

    //     val totalLabelCount = spans.length
    //     val goldVsOtherCount = totalLabelCount - (goldCount*2)

    //     s"${label}: ${totalLabelCount} (${goldCount})  = ${goldVsOtherCount}".box
    //   }

    // println(
    //   "Document Label Counts".box.hangIndent(vjoins(
    //     totalDocumentLabelCounts
    //   ))
    // )

  }

  def compareAlignedCellLabels(cells: List[TextGrid.GridCell \&/ TextGrid.GridCell], compareLabel: Label): Boolean = {

    def isSkippable(c: Char): Boolean = { " ,".contains(c) }

    def skipFilter(cell: TextGrid.GridCell \&/ TextGrid.GridCell): Boolean = cell match {
      case This(a) =>
        val isOtherOrUnlabeled =
          a.pins.isEmpty || a.pins.top.label == Other

        isSkippable(a.char) || isOtherOrUnlabeled
      case That(b) =>
        val isOtherOrUnlabeled =
          b.pins.isEmpty || b.pins.top.label == Other

        isSkippable(b.char) || isOtherOrUnlabeled

      case Both(a, b) =>
        val aIsOtherOrUnlabeled =
          a.pins.isEmpty || a.pins.top.label == Other
        val bIsOtherOrUnlabeled =
          b.pins.isEmpty || b.pins.top.label == Other

        val bothOther = aIsOtherOrUnlabeled && bIsOtherOrUnlabeled

        isSkippable(a.char) && isSkippable(b.char) |  bothOther
    }

    val trimmedCells = cells.dropWhile { skipFilter(_) }
      .reverse
      .dropWhile { skipFilter(_) }
      .reverse

    trimmedCells.forall{ _ match {
      case This(a) => false
      case That(b) => false

      case Both(a, b) =>
        val zippedPins = a.pins.zip(b.pins)
        val similarPins = zippedPins.filter{ case (p1, p2) =>
          p1.label == compareLabel && p1.label == p2.label
        }
        similarPins.nonEmpty
    }}

  }

  def summarizeCorpus(diffRecs: Seq[DocumentDiffRecords]): Unit = {

  }

  def diffAllDocuments(n: Int, onlyGolds: Boolean)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val allDiffs = if (onlyGolds) {
      corpusAccessApi.docStore.getDocuments()
        .filter { stableId => GoldLabelDataSet.UmaCziGold50.contains(stableId)  }
        .take(n)
        .map { stableId => diffDocumentAnnots(stableId) }

    } else {
      corpusAccessApi.docStore.getDocuments(n).map { stableId =>
        diffDocumentAnnots(stableId)
      }
    }

    println("Document Stats.....................")

    allDiffs.foreach{ diffRec =>
      summarizeDocument(diffRec)
    }
    summarizeCorpus(allDiffs)

    val corpusLabelCounts = allDiffs.flatMap{ diff =>

      val docLabelCounts = diff.labelSpans.map{ span => (span.label, span) }
        .groupBy(_._1).toList
        .map{ case (label, spans) =>
          (label, spans.length)
        }
      docLabelCounts

    }

    val corpusLabelCountStats = corpusLabelCounts
      .groupBy(_._1).toList
      .map{ case (label, spans) =>
        s"${label}: ${spans.map(_._2).sum}".box
      }

    println(
      "Corpus Label Counts".box.hangIndent(vjoins(
        corpusLabelCountStats
      ))
    )




  }

}
