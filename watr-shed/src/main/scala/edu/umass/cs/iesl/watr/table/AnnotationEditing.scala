package edu.umass.cs.iesl.watr
package table


import TypeTags._
import watrmarks._
import corpora._
import utils.DoOrDieHandlers._

import ammonite.{ops => fs}
import _root_.io.circe, circe._
// , circe.syntax._
import circe.generic.auto._
import circe.generic._

import geometry._
import geometry.syntax._
import segment._
import textgrid._
import utils.{RelativeDirection => Dir}

import TextGridLabelWidget._
import textboxing.{TextBoxing => TB}, TB._
import scala.collection.mutable


import GeometryCodecs._

object OldZoneFormats extends TypeTagCodecs  {

  case class DocZones(
    stableId: String,
    pageGeometries: Seq[LTBounds],
    zones: Seq[Zone]
  )

  @JsonCodec
  case class Zone(
    id: Int,
    regions: Seq[PageRegion],
    label: String,
    order: Int,
    glyphDefs: Option[String] = None
  )

}

/*

 ** Create clean, new-style database.
 ** Import 250+gold, get report as to which pdfs are missing
 ** Add missing PDFs
 ** Import UmaCzi gold-50 labels (which are part of pmid-1296)
 ** Run diffs/reports

 */
object ImportZoneFormatToAnnot {
  import OldZoneFormats._
  val R = RelationModel

  import utils.PathUtils._

  def checkZoneImportsForMissingCorpusEntries(jsonFilename: String)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val docStore = corpusAccessApi.docStore

    val jsonStr = fs.read(jsonFilename.toPath())
    val jsonSeq = jsonStr.decodeOrDie[List[Json]]()

    val oldDocZones = jsonSeq.map{ _.decodeOrDie[DocZones]("Error decoding doc zone") }

    println(s"decoded ${jsonSeq.length} old zone sets")

    oldDocZones.foreach { docZones =>
      print(s"checking ${docZones.stableId}..  ")
      val maybeDocs = docZones.zones.map{ docZone =>
        val newZone = AnnotatedLocation.Zone(docZone.regions)
        val stableId = newZone.stableId
        docStore.getDocument(stableId)
      }
      if (maybeDocs.exists(_.isEmpty)) {
        println(s"ERROR: missing pdf")
      } else {
        println(s"ok.")
      }
    }
  }


  def importZoneUberJson(jsonFilename: String, skipImportIfNotInCorpus: Boolean, pathSuffix: String)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore

    val jsonStr = fs.read(jsonFilename.toPath())
    val jsonSeq = jsonStr.decodeOrDie[List[Json]]()

    val oldDocZones = jsonSeq.map{ _.decodeOrDie[DocZones]("Error decoding doc zone") }
    println(s"decoded ${jsonSeq.length} old zone sets")

    oldDocZones.foreach { docZones =>
      println(s"importing ${docZones.stableId}")
      docZones.zones.foreach{ docZone =>
        val label = Labels.fromString(docZone.label)
        val newZone = AnnotatedLocation.Zone(docZone.regions)
        val stableId = newZone.stableId
        val maybeDocId = docStore.getDocument(stableId)
        if (maybeDocId.isDefined) {
          val annotId = annotApi.createAnnotation(label, newZone)
          annotApi.setCorpusPath(annotId, CorpusPath(s"Bioarxiv.UmaCzi2018.250PlusGold.${pathSuffix}"))

          docZone.glyphDefs.foreach { glyphs =>
            annotApi.updateBody(annotId, AnnotationBody.TextGrid(glyphs))
          }
        } else {

          if (skipImportIfNotInCorpus) {
            println(".. skipping ")
          } else {
            println(s"ERROR: annotation for ${stableId}, PDF not in database")
          }
        }
      }
    }
  }
}

object AnnotationDiffs {
  import spindex._
  import utils.EnrichNumerics._

  // Testing function
  def deleteFakeAnnots()(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val annotApi = corpusAccessApi.annotApi
    annotApi.listPathAnnotations(CorpusPathQuery("TestAnnots")).foreach { annotId =>
      annotApi.deleteAnnotation(annotId)
    }
  }

  def createFakePerturbedAnnots(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore

    val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")
    val annots = annotApi.listDocumentAnnotations(docId)

    annots.foreach { annotId =>
      val annotRec = annotApi.getAnnotationRecord(annotId)
      annotRec.location match {
        case AnnotatedLocation.Zone(regions) =>
          val newRegions = regions.map { pageRegion =>
            val b = pageRegion.bbox.translate(1.3, 2.1).scale(2.percent)
            pageRegion.copy(bbox=b)
          }

          val newAnnotId = annotApi.createAnnotation(annotRec.label, AnnotatedLocation.Zone(newRegions))
          annotApi.setCorpusPath(newAnnotId, CorpusPath("TestAnnots"))
          annotRec.body.foreach { _ match {
            case AnnotationBody.TextGrid(textGridDef) =>
              // val textGrid = TextGrid.fromJsonStr(textGridDef)
              // Wreak havoc...


              annotApi.updateBody(newAnnotId, AnnotationBody.TextGrid(textGridDef))
          }}

        case _ =>
      }

    }
  }




  val R = RelationModel
  import TextGridFunctions._
  import scalaz.{@@ => _, _} //, Scalaz._
  import \&/._
  import scalaz.syntax.align._
  // import scalaz.syntax.std.list._
  import scalaz.std.list._
  // import scalaz.std.list.listSyntax._
  import scalaz.syntax.equal._

  val AnyLabel = Label.auto
  val GridCellLabel = Label.auto
  val GridCellIndex = Label.auto
  val TextGridsAtPoint = Label.auto
  val LabeledCell = Label.auto
  val AnnotRecAttr = Label.auto
  val LabelIndicatorPoint = Label.auto
  val LabeledSpanAttr = Label.auto


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

  def diffDocumentAnnots(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore
    val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")

    val pageIndexMap = initPageIndexes(stableId)

    val annots = annotApi.listDocumentAnnotations(docId)

    annots.foreach { annotId =>
      val annotRec = annotApi.getAnnotationRecord(annotId)

      annotRec.location match {
        case AnnotatedLocation.Zone(regions) =>
          regions.foreach { pageRegion =>
            val pageNum = pageRegion.page.pageNum
            val pageIndex = pageIndexMap(pageNum)
            val shape = pageIndex.shapes.indexShape(pageRegion.bbox, AnyLabel)
            pageIndex.shapes.setShapeAttribute(shape.id, AnnotRecAttr, annotRec)
          }

        case _ =>
      }

      println(s"Indexing: ${annotRec.label}")

    }


    pageIndexMap.toList.sortBy(_._1)
      .foreach { case (pageNum, pageIndex) =>
        val pageShapes = pageIndex.shapes.getAllShapes()
        if (pageShapes.length > 0) {

          pageIndex.shapes.initClustering(AnyLabel, _ => true)
          pageIndex.shapes.getAllShapes().foreach { shape =>
            val rect = shape.asInstanceOf[RectShape].shape
            val overlaps = pageIndex.shapes.searchShapes(rect)
            pageIndex.shapes.unionAll(AnyLabel, overlaps)
          }

          pageIndex.shapes.getClusters(AnyLabel).foreach { clusters =>
            val overlaps = clusters.filter(_.length > 1)

            if (overlaps.length > 0) {
              println(s"Overlaps Found on Page ${pageNum}")
              overlaps.foreach { overlapping =>
                val annotRecs = overlapping.map{ shape =>
                  pageIndex.shapes.getShapeAttribute[R.AnnotationRec](shape.id, AnnotRecAttr).get
                }
                val overlappingIds = annotRecs.map { _.id  }.mkString(", ")
                println(s"Overlapping ${overlappingIds}")

                for {
                  rec1 <- annotRecs
                  rec2 <- annotRecs
                  if rec1.id != rec2.id
                } {
                  println(s"Comparing ${rec1.id} to ${rec2.id}")
                  val oneDefined = rec1.body.isDefined || rec2.body.isDefined
                  val bothDefined = rec1.body.isDefined && rec2.body.isDefined
                  if(bothDefined) {
                    val body1 = rec1.body.get
                    val body2 = rec2.body.get

                    val textGrid1 = body1 match {
                      case AnnotationBody.TextGrid(textGridDef) => TextGrid.fromJsonStr(textGridDef)
                    }

                    val textGrid2 = body2 match {
                      case AnnotationBody.TextGrid(textGridDef) => TextGrid.fromJsonStr(textGridDef)
                    }
                    val box1 = textGridToIndentedBox(textGrid1)
                    println("Examining Text...")
                    println(box1.toString)

                    textGrid1.indexedCells().zipWithIndex.foreach {
                      case ((cell, row, col), i) =>
                        val bbox = cell.pageRegion.bbox
                        val shape = pageIndex.shapes.indexShape(bbox, GridCellLabel)
                        pageIndex.shapes.setShapeAttribute(shape.id, GridCellIndex, (cell, i))
                    }

                    val sharedAndGrid2Cells  = textGrid2.indexedCells().zipWithIndex.map {
                      case ((cell, row, col), i) =>
                        val bbox = cell.pageRegion.bbox
                        val ctrPoint = bbox.toPoint(Dir.Center)
                        val hits = pageIndex.shapes.searchShapes(ctrPoint, GridCellLabel)
                        hits.foreach { hitShape =>
                          val (cell, cellIndex) = pageIndex.shapes.getShapeAttribute[(TextGrid.GridCell, Int)](hitShape.id, GridCellIndex).get
                          println(s"hit> ${cell.char}  #${cellIndex}")
                        }

                        if (hits.nonEmpty) {
                          val grid1Cell = hits.head
                          val (g1Cell, g1CellIndex) = pageIndex.shapes.getShapeAttribute[(TextGrid.GridCell, Int)](grid1Cell.id, GridCellIndex).get
                          pageIndex.shapes.deleteShape(grid1Cell)
                          Both((g1Cell, g1CellIndex), (cell, i))
                        } else {
                          That( (cell, i) )
                        }
                    }

                    val leftoverGrid1Cells = pageIndex.shapes
                      .getShapesWithLabel(GridCellLabel).map { shape =>
                      val (g1Cell, g1CellIndex) = pageIndex.shapes.getShapeAttribute[(TextGrid.GridCell, Int)](shape.id, GridCellIndex).get
                      This( (g1Cell, g1CellIndex) )
                    }

                    val shared = sharedAndGrid2Cells.collect {
                      case b@ Both(_, _) => b
                    }

                    val sharedStr = shared.map {
                      case b@ Both(c1, c2) => c1._1.char
                    }.mkString("")

                    println(s"Shared text = ${sharedStr}")


                    // val textGrid = TextGrid.fromJsonStr(textGridDef)
                    // val labelTree = textGridToLabelTree(textGrid)
                    // val labelSpanTree = labelTreeToSpanTree(labelTree)

                    // labelSpanTree.map { case (maybeLabel, start, len) =>

                    // }
                  } else if (oneDefined) {
                    println(s"Only one annotation has char-level labeling")
                  } else {
                    println(s"Neither annotation has char-level labeling")
                  }

                }
              }
            } else {
              println(s"No overlapping annotations on Page ${pageNum}")
            }
          }
        } else {
          println(s"No annotations on page ${pageNum}")
      }

    }
  }


  def findOverlappingAnnotations(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): List[Seq[R.AnnotationRec]] = {
    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore
    val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")

    val pageIndexMap = initPageIndexes(stableId)

    val annots = annotApi.listDocumentAnnotations(docId)
    annots.foreach { annotId =>
      val annotRec = annotApi.getAnnotationRecord(annotId)


      annotRec.location match {
        case AnnotatedLocation.Zone(regions) =>
          regions.foreach { pageRegion =>
            val pageNum = pageRegion.page.pageNum
            val pageIndex = pageIndexMap(pageNum)
            val shape = pageIndex.shapes.indexShape(pageRegion.bbox, AnyLabel)
            pageIndex.shapes.setShapeAttribute(shape.id, AnnotRecAttr, annotRec)
          }

        case _ =>
      }
    }

    val allPageOverlaps = pageIndexMap.toList.sortBy(_._1)
      .flatMap { case (pageNum, pageIndex) =>
        pageIndex.shapes.initClustering(AnyLabel, _ => true)
        pageIndex.shapes.getAllShapes().foreach { shape =>
          val rect = shape.asInstanceOf[RectShape].shape
          val overlaps = pageIndex.shapes.searchShapes(rect)
          pageIndex.shapes.unionAll(AnyLabel, overlaps)
        }

        val clusters = pageIndex.shapes.getClusters(AnyLabel).get

        val pageOverlaps: Seq[Seq[R.AnnotationRec]] = clusters.filter(_.length > 1).map{ shapes=>
          shapes.map { shape =>
            pageIndex.shapes.getShapeAttribute[R.AnnotationRec](shape.id, AnnotRecAttr).get
          }
        }
        pageOverlaps
      }
    allPageOverlaps
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

  def diffDocumentAnnots0(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore
    val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")

    val pageIndexMap = initPageIndexes(stableId)

    val annots = annotApi.listDocumentAnnotations(docId)
    annots.foreach { annotId =>
      val annotRec = annotApi.getAnnotationRecord(annotId)
      annotRec.body.foreach { body =>
        body match {
          case AnnotationBody.TextGrid(textGridDef) =>
            val textGrid = TextGrid.fromJsonStr(textGridDef)
            // Iterate over all labeled spans
            val labelTree = textGridToLabelTree(textGrid)
            val marginalGloss = labelTreeToMarginals(labelTree, compactMarginals = true)
            val marginalGlossBox = marginalGlossToTextBlock(marginalGloss)
            val labelSpanTree = labelTreeToSpanTree(labelTree)

            // println(
            //   "Marginal Gloss".box.hangIndent(
            //     marginalGlossBox
            //   ).toString()
            // )

            val gridCells = textGrid.indexedCells()

            labelSpanTree.flatten.foreach { case (maybeLabel, start, len) =>
              maybeLabel.foreach { spanLabel =>
                val cells = gridCells.slice(start, start+len)
                val text = cells.map(_._1.char).mkString
                val cellSeqPinBlock = hjoins(top, cells.map(c => bioPinsToBlockRep(c._1.pins)))
                val textAndPinBlock  = vjoin(text.box, cellSeqPinBlock)


                // hjoins(TB.top, text.zip(pins).map{case (c1, c2) => c1.toString.box atop c2.toString }),
                val textAndPins = "Checking".box.hangIndent(
                  hjoin(
                    ">>",
                    textAndPinBlock
                  ))


                // println(s"Checking @ ${maybeLabel}")
                // println(indent(4, textAndPins))

                cells.foreach{ case (gridCell, start, len) =>
                  val cellRegion = gridCell.pageRegion
                  val pageNum = cellRegion.page.pageNum
                  val pageIndex = pageIndexMap(pageNum)

                  // Add to index whiteboard:
                  val cellBounds = gridCell.pageRegion.bbox
                  val hits = pageIndex.shapes.searchShapes(cellBounds, LabeledCell)
                  // With label pins
                  val pinSet: Seq[BioPin] = gridCell.pins
                  val pinStr = pinSet.map(_.toString()).mkString

                  if (hits.nonEmpty) {
                    // assume(hits.length==1)
                    // val hit = hits.head
                    if (hits.length > 1) {
                      println(s"Multiple Hits at ${gridCell.char} (start:${start}, len:${len}) ".hangIndent(vjoin(
                        textAndPins,
                        vspace(1),
                      )))
                    }
                    hits.foreach { hit =>

                      pageIndex.shapes.getShapeAttribute[TB.Box](hit.id, spanLabel :: TextGridsAtPoint) match {
                        case Some(otherTextAndPins) =>
                          if (textAndPins.toString() != otherTextAndPins.toString()) {
                            println("Diff Comparing ".hangIndent(vjoin(
                              vspace(1),
                              textAndPins,
                              vspace(1),
                              " --- ",
                              vspace(1),
                              otherTextAndPins,
                              vspace(1),
                              "######## "
                            )))
                          } else {
                            // println("Identical ".hangIndent(vjoin(
                            //   textAndPins,
                            //   vspace(1),
                            // )))

                          }

                        case None =>
                          // println("Only Copy".hangIndent(vjoin(
                          //   textAndPins,
                          //   vspace(1),
                          // )))

                      }

                    }

                  } else {
                    val cellCenter = cellBounds.toPoint(Dir.Center)
                    val shape = pageIndex.shapes.indexShape(cellCenter, LabeledCell)
                    pageIndex.shapes.setShapeAttribute(shape.id, GridCellLabel, pinStr)
                    pageIndex.shapes.setShapeAttribute(shape.id, spanLabel :: TextGridsAtPoint, textAndPins)
                  }
                }
              }
            }
        }
      }
    }
  }













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
    i2: Int
  )

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
        val queryBox = cellBounds

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
                  val differs =
                    span1.pinRepBox.toString() != span2.pinRepBox.toString()

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

// val pinReps = spansAtPoint.map(_.pinRepBox)
// val pinRepStrs = pinReps.map(_.toString)
// val uniquePinReps = pinRepStrs.toSet

// if (uniquePinReps.size > 1) {
//   println(s"Multiple labels found")
//   // Mark fine label entry as error/diff
//   val differingRecords = spansAtPoint.map(_.spanRecIndex)

//   // val contextStrings = spansAtPoint.map{ case (labelSpan, cellNum, pinRepBox) =>
//   val contextStrings = spansAtPoint.map{ case indicatorPointAttr =>
//     val labelSpan = indicatorPointAttr.labeledSpan
//     val cellNum = indicatorPointAttr.cellIndex
//     val idStr = labelSpan.annotId.toString()
//     val labelStr = labelSpan.label.fqn
//     val (pre, mid) = labelSpan.cells.splitAt(cellNum)
//     val (center, post) = mid.splitAt(1)
//     val preStr = pre.map(_._1.char).mkString
//     val postStr = post.map(_._1.char).mkString
//     val ctrStr = center.map(_._1.char).mkString

//     (preStr, ctrStr, postStr, idStr, labelStr)
//   }

//   println(
//     "Context Strings".box.hangIndent(
//       vjoin(
//         vspace(1),
//         hjoin(
//           vjoins(right, contextStrings.map(_._4.box)),
//           hspace(1),
//           vjoins(right, contextStrings.map(_._5.box)),
//           ":  ",
//           vjoins(right, contextStrings.map(_._1.box)),
//           "[",
//           vjoins(left, contextStrings.map(_._2.box)),
//           "]",
//           vjoins(left, contextStrings.map(_._3.box))
//         )
//       )
//     )
//   )
// }
