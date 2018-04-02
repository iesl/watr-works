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

object ImportZoneFormatToAnnot {
  import OldZoneFormats._
  val R = RelationModel

  import utils.PathUtils._

  def importZoneUberJson(jsonFilename: String)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val annotApi = corpusAccessApi.annotApi

    val jsonStr = fs.read(jsonFilename.toPath())
    val jsonSeq = jsonStr.decodeOrDie[List[Json]]()

    val oldDocZones = jsonSeq.map{ _.decodeOrDie[DocZones]("Error decoding doc zone") }
    println(s"decoded ${jsonSeq.length} old zone sets")

    oldDocZones.foreach { docZones =>
      // val docId = corpusAccessApi.docStore.getDocument(DocumentID(docZones.stableId))
      println(s"importing ${docZones.stableId}")
      docZones.zones.foreach{ docZone =>
        val label = Labels.fromString(docZone.label)
        val newZone = AnnotatedLocation.Zone(docZone.regions)
        val annotId = annotApi.createAnnotation(label, newZone)
        annotApi.setCorpusPath(annotId, CorpusPath("Bioarxiv.UmaCzi2018.PmidLinked.InitialLabels"))

        docZone.glyphDefs.foreach { glyphs =>
          annotApi.updateBody(annotId, AnnotationBody.TextGrid(glyphs))
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

  val AnyLabel = Label.auto
  val GridCellLabel = Label.auto
  val GridCellIndex = Label.auto
  val AnnotRecAttr = Label.auto

  def initPageIndexes(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): Map[Int@@PageNum, PageIndex] = {
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

    val pageIndexMap =initPageIndexes(stableId)

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
                        // assume(hits.length < 2)
                        println(s"hit count: ${hits.length}")
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

  def diffDocumentAnnots2(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore
    val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")

    val overlappingAnnots = findOverlappingAnnotations(stableId)

    for {
      annotRecs <- overlappingAnnots
      (rec1, i1) <- annotRecs.zipWithIndex
      rec2 <- annotRecs.drop(i1+1)
      if rec1.id != rec2.id
    } {
      println(s"Comparing ${rec1.id} to ${rec2.id}")
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
        // Iterate over all labeled spans
        val labelTree = textGridToLabelTree(textGrid1)
        val labelSpanTree = labelTreeToSpanTree(labelTree)

        val grid1Cells = textGrid1.indexedCells()
        labelSpanTree.flatten.foreach { case (maybeLabel, start, len) =>
          val text = grid1Cells.slice(start, start+len).map(_._1.char).mkString
          maybeLabel.foreach { l =>
            println(s"${l}  > ${text}")
          }
        }


      }
    }

  }
  import com.github.tototoshi.csv._
  // scala> val f = new File("out.csv")
  // scala> val writer = CSVWriter.open(f, append=true)
  // writer: com.github.tototoshi.csv.CSVWriter = com.github.tototoshi.csv.CSVWriter@41ad4de1
  // scala> writer.writeRow(List("a", "b", "c"))
  // scala> writer.writeRow(List("d", "e", "f"))
  // scala> writer.close()

  def writeLabelSpanCsv(stableId: String@@DocumentID)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val annotApi = corpusAccessApi.annotApi
    val docStore = corpusAccessApi.docStore
    val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")
    val csvFile = new java.io.File("out.csv")
    val csvWriter = CSVWriter.open(csvFile, append=true)

    val annots = annotApi.listDocumentAnnotations(docId)
    annots.foreach { annotId =>
      val annotRec = annotApi.getAnnotationRecord(annotId)
      annotRec.body.foreach { body =>
        body match {
          case AnnotationBody.TextGrid(textGridDef) =>
            val textGrid = TextGrid.fromJsonStr(textGridDef)
            // Iterate over all labeled spans
            val labelTree = textGridToLabelTree(textGrid)
            val labelSpanTree = labelTreeToSpanTree(labelTree)

            val grid1Cells = textGrid.indexedCells()
            labelSpanTree.flatten.foreach { case (maybeLabel, start, len) =>
              val text = grid1Cells.slice(start, start+len).map(_._1.char).mkString
              val encText = Json.fromString(text).noSpaces
              maybeLabel.foreach { l =>
                csvWriter.writeRow(List(stableId.unwrap, l.fqn, encText))
              }
            }
        }
      }

    }

    csvWriter.close()
  }

}
