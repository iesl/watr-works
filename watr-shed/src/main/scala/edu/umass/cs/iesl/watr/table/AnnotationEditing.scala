package edu.umass.cs.iesl.watr
package table

import TypeTags._
import watrmarks._
import corpora._
import utils.DoOrDieHandlers._

import geometry._
import geometry.syntax._
import textgrid._
import utils.{RelativeDirection => Dir}

import textboxing.{TextBoxing => TB}, TB._
import scala.collection.mutable

object AnnotationDiffs {
  import spindex._


  val R = RelationModel
  import TextGridFunctions._
  import scalaz.{@@ => _, _} //, Scalaz._
  import \&/._
  import scalaz.syntax.align._
  import scalaz.std.list._
  import scalaz.syntax.equal._

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
    cells: Seq[(TextGrid.GridCell, Int, Int)]
  )

  case class IndicatorPointAttr(
    labeledSpan: LabeledSpan,
    cellIndex: Int,
    pinRepBox: TB.Box,
    spanRecIndex: Int
  )

  case class DiffRecord(
    i1: Int,
    i2: Int,
    // l1: Label,
    // l2: Label
  )

  case class DocumentDiffRecords(
    stableId: String@@DocumentID,
    labelCount: Int,

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


  def buildFineGrainedLabelTable(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): Seq[LabeledSpan] = {

    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore
    val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")

    val annots = annotApi.listDocumentAnnotations(docId)

    annots.flatMap { annotId =>
      val annotRec = annotApi.getAnnotationRecord(annotId)
      annotRec.body.toList.flatMap { body =>
        body match {
          case AnnotationBody.TextGrid(textGridDef) =>
            val textGrid = TextGrid.fromJsonStr(textGridDef)
            val labelTree = textGridToLabelTree(textGrid)
            val labelSpanTree = labelTreeToSpanTree(labelTree)
            val gridCells = textGrid.indexedCells()

            labelSpanTree.flatten.flatMap { case (maybeLabel, start, len) =>
              maybeLabel.map { spanLabel =>
                val cells = gridCells.slice(start, start+len)
                LabeledSpan(annotId, spanLabel, cells)
              }
            }

          case _ => None
        }
      }
    }
  }


  def diffDocumentAnnots3(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): Unit = {

    val pageIndexMap = initPageIndexes(stableId)
    val allFineLabels: Seq[LabeledSpan] = buildFineGrainedLabelTable(stableId)

    println(s"${stableId} Fine-grained label count = ${allFineLabels.length}")

    def addLabelIndicatorPoints(labelSpans: Seq[LabeledSpan]): Unit = {
      for {
        (labelSpan, spanNum) <- labelSpans.zipWithIndex
      } for {
        ((gridCell, _, _), cellNum) <- labelSpan.cells.zipWithIndex
        if gridCell.isGlyphCell
      } {
        val cellRegion = gridCell.pageRegion
        val pageNum = cellRegion.page.pageNum
        val pageIndex = pageIndexMap(pageNum)
        val cellBounds = gridCell.pageRegion.bbox
        val cellCenter = cellBounds.toPoint(Dir.Center)
        val pinRep = bioPinsToBlockRep(gridCell.pins)

        val queryBox = minBoundingRect(cellCenter)
        // val queryBox = cellBounds

        val hits = pageIndex.shapes.searchShapes(queryBox, LabelIndicatorPoint)

        if (hits.isEmpty) {
          val indicatorPt = pageIndex.shapes.indexShape(cellCenter, LabelIndicatorPoint)
          val attr = List( IndicatorPointAttr(labelSpan, cellNum, pinRep, spanNum) )
          pageIndex.shapes.setShapeAttribute(indicatorPt.id, LabeledSpanAttr, attr)

        } else {
          // assume(hits.length==1)

          if (hits.length > 1) {
            val allHitChars = hits.map{ hitShape =>
              val hitAttrs = pageIndex.shapes.getShapeAttribute[List[IndicatorPointAttr]](hitShape.id, LabeledSpanAttr).get
              val hitChars = hitAttrs.map{ attr =>
                val cell = attr.labeledSpan.cells(attr.cellIndex)
                cell._1.char
              }
              val allGlyphs = hitAttrs.forall{ attr =>
                val cell = attr.labeledSpan.cells(attr.cellIndex)
                cell._1.isGlyphCell
              }
              if (allGlyphs) {
                hitChars.mkString("[", ",", "]")
              } else {
                hitChars.mkString("[!!:", ",", "]")
              }
            }
            val s = allHitChars.mkString(", ")
            println(s"Warning: Multiple Indictor Points found, n=${hits.length}, {  ${s} }")
          }
          hits.foreach { indicatorPt =>
            // val priorSpans = pageIndex.shapes.getShapeAttribute[List[(LabeledSpan, Int, TB.Box)]](indicatorPt.id, LabeledSpanAttr).get
            val priorSpans = pageIndex.shapes.getShapeAttribute[List[IndicatorPointAttr]](indicatorPt.id, LabeledSpanAttr).get
            val pinReps = priorSpans.map(_.pinRepBox)

            val pinRepStrs = pinReps.map(_.toString)
            val updated = if (pinRepStrs.contains(pinRep.toString)) {
              priorSpans
            } else {
              IndicatorPointAttr(labelSpan, cellNum, pinRep, spanNum) :: priorSpans
            }
            // println(s"updating indicatorPt: ${updated.map(_._1.label)}")
            pageIndex.shapes.setShapeAttribute(indicatorPt.id, LabeledSpanAttr, updated)
          }
        }
      }
    } /////////////////////////////////

    addLabelIndicatorPoints(allFineLabels)

    val diffRecords = mutable.ArrayBuffer[DiffRecord]()



    pageIndexMap.toList.sortBy(_._1)
      .foreach { case (pageNum, pageIndex) =>
        val indicatorPoints = pageIndex.shapes.getShapesWithLabel(LabelIndicatorPoint)
        if (indicatorPoints.length > 0) {
          println(s"   pg.${pageNum}: Indicator Point count = ${indicatorPoints.length}")
          indicatorPoints.foreach { indicatorPoint =>
            // val spansAtPoint = pageIndex.shapes.getShapeAttribute[List[(LabeledSpan, Int, TB.Box)]](indicatorPoint.id, LabeledSpanAttr).get
            val spansAtPoint = pageIndex.shapes.getShapeAttribute[List[IndicatorPointAttr]](indicatorPoint.id, LabeledSpanAttr).get
            if (spansAtPoint.length > 1) {

              spansAtPoint.combinations(2).foreach {
                case List(span1, span2) =>
                  val differs = span1.pinRepBox.toString() != span2.pinRepBox.toString()

                  if (differs) {
                    diffRecords.append(
                      DiffRecord(span1.spanRecIndex, span2.spanRecIndex)
                    )
                  }
              }
            }
          }
        }
      }

    val uniqDiffsPerSpan = diffRecords
      .groupBy { rec => (rec.i1, rec.i2) }

    val numOfDiffs = uniqDiffsPerSpan.keySet.size

    println(s"Number of Fine-grained label differences: ${numOfDiffs}")

    uniqDiffsPerSpan.toSeq.foreach { case ((spanIndex1, spanIndex2), diffs) =>
      println(s"Comparing Unique Diffs ${spanIndex1} to ${spanIndex2}")
      val labelSpan1 = allFineLabels(spanIndex1)
      val labelSpan2 = allFineLabels(spanIndex2)
      // val pinReps = spansAtPoint.map(_.pinRepBox)
      // val pinRepStrs = pinReps.map(_.toString)
      // val uniquePinReps = pinRepStrs.toSet

      // val bioBlock1 = indexedCellsToBioBlock(labelSpan1.cells)
      // val bioBlock2 = indexedCellsToBioBlock(labelSpan2.cells)

      val span1Cells = labelSpan1.cells.map(_._1).toList
      val span2Cells = labelSpan2.cells.map(_._1).toList
      var span2AlignStart = -1
      // val (preSpan1, alignedSpan1) = span1Cells.span { gridCell =>
      val preSpan1= span1Cells.takeWhile { gridCell =>
        // println(s"Comparing '${gridCell.char}' ${gridCell.pageRegion.bbox.toString()}")
        span2AlignStart = span2Cells.indexWhere { case cell2 =>
          val bothGlyphs = gridCell.isGlyphCell() && cell2.isGlyphCell()
          val eq = gridCell.pageRegion.bbox === cell2.pageRegion.bbox
          // println(s"    to '${cell2.char}' ${cell2.pageRegion.bbox.toString()}   eq = ${eq}")
          bothGlyphs && eq
        }
        // println(s"    match @ ${span2AlignStart}")
        span2AlignStart < 0
      }
      // println(s"    Final match @ ${span2AlignStart}")

      val alignedSpan1 = span1Cells.drop(preSpan1.length)

      val aligned = alignedSpan1.align(span2Cells.drop(span2AlignStart))
      val thiss = preSpan1.map(This(_))
      val thats = span2Cells.take(span2AlignStart).map(That(_))

      if (thiss.length > 0 && thats.length > 0) {
        println(s"ERROR: Misalignment!!")
      }

      val alignment = thiss ++ thats ++ aligned

      val alignedCols = alignment.map{ _ match {
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

          if (pinColA.toString() == pinColB.toString()) {
            vjoin(
              a.char.toString,
              b.char.toString,
              " ",
              pinColA,
            )

          } else {
            vjoin(
              a.char.toString,
              b.char.toString,
              "✗",
              pinColA,
              "─",
              pinColB,
            )
          }
      }}

      hjoins(top, alignedCols)


      println(
       "Mis-aligned".box.hangIndent(
         vjoin(
           vspace(1),
           hjoins(top, alignedCols),
           vspace(1),
         )
       )
      )
    }
  }


}

