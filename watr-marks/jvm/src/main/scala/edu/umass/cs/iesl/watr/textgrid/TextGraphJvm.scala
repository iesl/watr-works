package edu.umass.cs.iesl.watr
package textgrid

import TextGraph._
import utils.GraphPaper
import rtrees._
import utils.ExactFloats._
import TypeTags._
import geometry._
import geometry.syntax._
import watrmarks.Label
import utils.TreeShaper
import scalaz.{@@ => _, _} //, Scalaz._
import scalaz.syntax.std.list._
import textboxing.{TextBoxing => TB}, TB._

import utils.DoOrDieHandlers._
import utils.Debugging._
import utils.Maths._
import utils.SlicingAndDicing._
import utils.Color
import utils.EnrichNumerics._

/**
  *
  *  Coordinate Systems:
  *    Matrix Coords
  *       (row, col); represented by Graph paper Cell/Box classes
  *
  *    Real Coords
  *       (x, y, width, height); represented by Floats (as defined in LTBounds)
  **/


abstract class TextGraphJvm(
  val shapeIndex: LabeledShapeIndex[GeometricFigure, TextGraphShape.Attr, TextGraphShape]
) extends TextGraph {
  import TextGraphShape._


  def graphHeight(): Int = {
    if (shapeIndex.getAllShapes().nonEmpty) {
      val maxShape = shapeIndex.getAllShapes().maxBy(_.shape.minBounds.bottom)
      val maxY = maxShape.shape.minBounds.bottom
      rowNumForY(maxY) + 1
    } else 0
  }


  def graphWidth(): Int = {
    if (shapeIndex.getAllShapes().nonEmpty) {
      val maxShape = shapeIndex.getAllShapes().maxBy(_.shape.minBounds.right)
      val maxX = maxShape.shape.minBounds.right
      colNumForX(maxX) + 1
    } else 0
  }

  def graphArea(): GraphPaper.Box = {
    val height = graphHeight()
    val width = graphWidth()
    GraphPaper.boxAt(0, 0).setWidth(width).setHeight(height)
  }

  def appendRow(cells: Seq[GridCell]): Unit = {
    val nextRow = graphHeight()
    for {
      (cell, i) <- cells.zipWithIndex
    } {
      val graphSquare = GraphPaper.cellAt(i, nextRow)
      val realBounds = matrixToRealCoords(graphSquare)

      shapeIndex.indexShape{ id =>
        GlyphShape(realBounds, id, Some(cell))
      }
    }
  }

  def glyphCells(): Seq[(GlyphShape, GraphPaper.GridCell)] = {
    for {
      cell <- graphArea().getCells()
      query = matrixToRealCoords(cell)
      cellGlyphs = shapeIndex.searchShapes(query).collect{ case g: GlyphShape => g }
      if cellGlyphs.nonEmpty
    } yield {
      (cellGlyphs.head, cell)
    }
  }


  def getMatrixContent(fromRow: Int=0, len: Int=Int.MaxValue): Option[MatrixArea.Rows] = {
    val height = graphHeight()
    val width = graphWidth()


    val start = clamp(0, height)(fromRow)
    val end = clamp(start, height)(start+len)

    val rowBoxes = for {
      y <- start until end
    } yield GraphPaper.boxAt(0, y).setWidth(width)

    val rows = rowBoxes.toList.map{ box =>
      val rowQuery = matrixToRealCoords(box)
      val rowGlyphs = shapeIndex.searchShapes(rowQuery)
        .collect{ case g: GlyphShape => g }
        .sortBy(_.shape.minBounds.left)

      if (rowGlyphs.nonEmpty) {
        val rowRealBounds = rowGlyphs.map(_.shape).reduce(_ union _)
        val rowMatrixBounds = realToMatrixCoords(rowRealBounds)

        Some((MatrixArea.Row(rowMatrixBounds, rowGlyphs), rowRealBounds))

      } else None
    }

    val nonEmptyRows = rows.flatten

    if (nonEmptyRows.nonEmpty) {
      val rowsRealBounds = nonEmptyRows.map(_._2).reduce(_ union _)
      val rowsMatrixBounds = realToMatrixCoords(rowsRealBounds)
      Some(MatrixArea.Rows(rowsMatrixBounds, nonEmptyRows.map(_._1)))
    } else None
  }

  def getRows(): Seq[Seq[GridCell]] = {
    val height = graphHeight()
    val width = graphWidth()

    val rowBoxes = for {
      y <- 0 until height
    } yield GraphPaper.boxAt(0, y).setWidth(width)

    val rows = rowBoxes.toList.map{ box =>
      val rowQuery = matrixToRealCoords(box)
      shapeIndex.searchShapes(rowQuery)
        .collect{ case g: GlyphShape => g }
        .sortBy(_.shape.minBounds.left)
        .map(_.attr.get)
    }
    rows
  }


  def addLabel(row: Int, len: Int, label: Label, parent: Label): Option[LabeledSeq] = {
    // _addLabel(label, Some(parent), row, len)
    ???
  }

  def addLabel(row: Int, len: Int, label: Label): Option[LabeledSeq] = {
    // _addLabel(label, None, row, len)
    ???
  }


  // private def _addLabel(label: Label, parent: Option[Label], fromRow: Int, len: Int): Option[LabeledSeq] = {

  //   getMatrixContent(fromRow, len).flatMap{ content =>

  //     val labelStacks = getLabeledRegionStacks(content.area)

  //     val validUnlabeledTarget = labelStacks.isEmpty && parent.isEmpty

  //     val singleOverlappedLabel = labelStacks.length == 1

  //     val validLabeledTarget = labelStacks.length == 1 && {
  //       parent.exists{ parentLabel =>
  //         labelStacks.head.head.hasLabel(parentLabel)
  //       }
  //     }

  //     val realBounds = matrixToRealCoords(content.area)

  //     if (validUnlabeledTarget) {
  //       val newShape = shapeIndex.indexShape{ id =>
  //         LabeledSeq(realBounds, id, None).addLabels(label)
  //       }
  //       Some(newShape.asInstanceOf[LabeledSeq])
  //     } else if (singleOverlappedLabel && parent.isDefined) {
  //       val parentLabel = parent.orDie("")
  //       val overlappedLabel = labelStacks.head.head
  //       val meetsLabelConstraint = overlappedLabel.hasLabel(parentLabel)
  //       val isSubArea = realBounds.isContainedBy(overlappedLabel.bounds)

  //       if (meetsLabelConstraint && isSubArea) {

  //         val newShape = shapeIndex.indexShape{ id =>
  //           LabeledSeq(realBounds, id, Some(overlappedLabel.id)).addLabels(label)
  //         }

  //         Some(newShape.asInstanceOf[LabeledSeq])
  //       } else None

  //     } else None
  //   }

  // }

  def labelSequence(label: Label, parent: Option[Label], begin: Int, len: Int): Option[LabeledSeq] = {
    val cellsToLabel = glyphCells().drop(begin).take(len)

    val rowsOfCells = cellsToLabel.groupByPairs {
      case ((_, graphSquare1), (_, graphSquare2)) =>
        graphSquare1.y == graphSquare2.y
    }

    val rowMinBounds = rowsOfCells.map{ row =>
      GraphPaper.union(row.map(_._2.toBox()))
    }

    GraphPaper.union(rowMinBounds.flatten).map{ minBounds =>
      val realBounds = matrixToRealCoords(minBounds)

      // parent label constraints:
      val newlabelRange = RangeInt(begin, len)

      val labelStacks = getLabeledRegionStacks(minBounds)

      val overlappingLabelStacks = labelStacks.filter{ labelStack =>
        labelStack.headOption.exists { labeledSeq =>
          val seqRange = RangeInt(labeledSeq.begin, labeledSeq.len)
          seqRange.contains(newlabelRange)
        }
      }

      val validUnlabeledTarget = overlappingLabelStacks.isEmpty && parent.isEmpty

      val rowSpans = rowMinBounds.map{_ match {
        case Some(box) => (box.origin.x, box.width)
        case None => (0, 0)
      }}

      shapeIndex.indexShape { shapeId =>
        LabeledSeq(
          realBounds,
          shapeId,
          begin, len,
          None,
          Set(label),
          rowSpans
        )
      }
    }
  }


  def queryShapes(box: GraphPaper.Box): Seq[TextGraphShape] = {
    val query = matrixToRealCoords(box)
    shapeIndex.searchShapes(query)
  }

  val ts = TreeShaper[Int]

  def findLabelTrees(area: GraphPaper.Box): Seq[Tree[LabeledSeq]] = {
    val labelShapes = queryShapes(area).collect{ case g: LabeledSeq => g }

    val parentChildPairs = labelShapes.map{ labelShape =>
      val id = labelShape.id.unwrap
      val parentId = labelShape.parent.map(_.unwrap).getOrElse(0)
      (parentId, id)
    }
    val idTrees0 = ts.makeTreeFromPairs(parentChildPairs)
    val idTrees = idTrees0.headOption.map { t => t.subForest.toList }.getOrElse { List() }

    val shapeTrees = idTrees.toList.map{ tree =>
      tree.map { id =>
        labelShapes.find(_.id.unwrap == id).get
      }
    }
    shapeTrees
  }

  def getLabeledRegionStacks(area: GraphPaper.Box): Seq[Seq[LabeledSeq]] = {

    val labelTrees = findLabelTrees(area)

    labelTrees.toList.flatMap{ tree =>
      val leafToRootPaths = tree.loc.cojoin
        .toTree.levels
        .flatten.filter(_.isLeaf)
        .map(_.path)
        .toList

      leafToRootPaths
    }

  }
}

object TextGraphJvm {

  import _root_.io.circe
  import circe._
  import circe.syntax._
  import circe.literal._
  import circe.generic.semiauto._
  import LabeledShapeIndex._
  import rtrees.RTreeIndex._
  import GeometryCodecs._


  implicit def EncodeTextGraphJvm: Encoder[TextGraphJvm] = Encoder.instance { textGraphJvm =>
    val textRows = textGraphJvm.getRows().map{ row =>
      val glyphs = row.map { _.asJson }
      val lines = row.map(_.char).mkString.asJson
      (lines, glyphs)
    }

    Json.obj(
      "stableId" := textGraphJvm.stableId,
      "lines" := textRows.map(_._1),
      "glyphs" := textRows.map(_._2),
      // "shapeIndex" := textGraphJvm.shapeIndex,
    )
  }

  implicit def DecodeTextGraphJvm: Decoder[TextGraphJvm] = Decoder.instance { hCursor =>

    val stableId = hCursor.downField("stableId")
      .focus.orDie().decodeOrDie[String]()

    val shapeIndex = hCursor.downField("shapeIndex").focus.orDie()
      .decodeOrDie[ LabeledShapeIndex[
        GeometricFigure, TextGraphShape.Attr, TextGraphShape
      ] ]()

    Right(fromShapeIndex(DocumentID(stableId), shapeIndex))
  }


  def create(id: String@@DocumentID): TextGraphJvm = {
    val shapeIndex = LabeledShapeIndex.empty[GeometricFigure, TextGraphShape.Attr, TextGraphShape]
    new TextGraphJvm(shapeIndex) {
      def stableId: String@@DocumentID = id
    }
  }

  def fromShapeIndex(
    id: String@@DocumentID,
    shapeIndex: LabeledShapeIndex[GeometricFigure, TextGraphShape.Attr, TextGraphShape]
  ): TextGraphJvm = {
    new TextGraphJvm(shapeIndex) {
      def stableId: String@@DocumentID = id
    }
  }

  def textGraphToIndentedBox(textGraph: TextGraph): TB.Box = {
    //   val labelTree = textGraphToLabelTree(textGraph)

    //   val lls = flattenLabelTreeToLines(labelTree)

    //   val dbg = lls.map { _ match {
    //     case LabeledRowElem.CellGroupRow(labels, cells, depthMod) =>
    //       val text = cells.map(_.cells.map(_.char).mkString).mkString
    //       val depth = Indent * (labels.length+1+depthMod)
    //       (
    //         labels.map(_.fqn).mkString(", "),
    //         indent(
    //           depth,
    //           s"${text}"
    //         )
    //       )
    //     case LabeledRowElem.HeadingRow(labels, heading) =>
    //       val text = heading
    //       val depth = Indent * labels.length
    //       (
    //         labels.map(_.fqn).mkString(", "),
    //         indent(
    //           depth,
    //           s"▸ ${text}"
    //         )
    //       )
    //   }}

    //   vjoins(left, dbg.map(_._2))
    ???
  }

  import utils.AsciiGraphPaper

  def textGraphToGraphPaper(textGraph: TextGraphJvm): AsciiGraphPaper = {

    val height = textGraph.graphHeight()
    val width = textGraph.graphWidth()
    println(s"w/h = ${width}/${height }")
    val graphPaper = new AsciiGraphPaper(width, height, true)
    for {
      (cells, rowNum) <- textGraph.getRows().zipWithIndex
      (cell, colNum) <- cells.zipWithIndex
    } {
      val graphSquare = GraphPaper.cellAt(colNum, rowNum)
      graphPaper.drawChar(graphSquare, cell.char)
    }

    textGraph.shapeIndex.getAllShapes()
      .collect {
        case shape: TextGraphShape.LabeledSeq =>
          val bbox = shape.graphBox()
          println(s"shape: ${shape}  == bbox: ${bbox}")
          val c = Color(128, 128, 200)
          graphPaper.applyBgColor(bbox, c)

          bbox.getRows().zip(shape.rowSpans)
            .foreach{ case (rowBox, (begin, len)) =>
              println(s"row: ${rowBox}  (${begin} - ${len})")
              val underlineBox = rowBox.translate(begin, 0)
                .setWidth(len)

              graphPaper.underline(underlineBox)
            }
      }

    graphPaper
  }

}
