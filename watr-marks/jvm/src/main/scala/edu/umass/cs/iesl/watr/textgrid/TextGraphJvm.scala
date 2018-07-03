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
  // import TextGraph._

  val L = ShapeLabels

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
      val realBounds = shaveMargin(matrixToRealCoords(graphSquare))

      shapeIndex.indexShape{ id =>
        GlyphShape(realBounds, id, Some(cell))
      }
    }
  }

  // def glyphCells(): Seq[GlyphShape] = {
  def glyphCells(): Seq[(GlyphShape, GraphPaper.GridCell)] = {
    for {
      cell <- graphArea().getCells()
      query = shaveMargin(matrixToRealCoords(cell))
      cellGlyphs = shapeIndex.searchShapes(query).collect{ case g: GlyphShape => g }
      if cellGlyphs.nonEmpty
    } yield {
      (cellGlyphs.head, cell)
    }
  }

  def toLabeledSequence(): LabeledSequence.Things[GlyphShape] = {
    val allLabelTrees = findLabelTrees(graphArea())
    for {
      labelTree <- allLabelTrees
      level <- labelTree.levels
      labelShape <- level
    } {
      labelShape

    }
    val labeledSequence = for {
      (glyphShape, glyphCell) <- glyphCells()
    } yield {

      val attr = LabelTarget.Thing(glyphShape)
      val labelTreeAtCell = findLabelTrees(glyphCell.toBox())
      if (labelTreeAtCell.isEmpty) {

      }

      attr
    }
    val things = LabeledSequence.Things(labeledSequence)

    // things.addBioLabel(label: Label, begin: Int, len: Int)

    things
  }

  def getMatrixContent(fromRow: Int=0, len: Int=Int.MaxValue): Option[MatrixArea.Rows] = {
    val height = graphHeight()
    val width = graphWidth()


    val start = clamp(0, height)(fromRow)
    val end = clamp(start, height)(start+len)

    val rowBoxes = for {
      y <- start until end
    } yield GraphPaper.boxAt(0, y).extendRight(width-1)

    val rows = rowBoxes.toList.map{ box =>
      val realBox = matrixToRealCoords(box)
      val rowQuery = shaveMargin(realBox)
      val rowGlyphs = shapeIndex.searchShapes(rowQuery)
        .collect{ case g: GlyphShape => g }
        .sortBy(_.shape.minBounds.left)

      if (rowGlyphs.nonEmpty) {
        val rowRealBounds = rowGlyphs.map(_.shape).reduce(_ union _)
        val rowMatrixBounds = realToMatrixCoords(rowRealBounds)

        Some(
          (MatrixArea.Row(rowMatrixBounds, rowGlyphs),
            rowRealBounds))

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
      val realBox = matrixToRealCoords(box)
      val rowQuery = shaveMargin(realBox)
      shapeIndex.searchShapes(rowQuery)
        .collect{ case g: GlyphShape => g }
        .sortBy(_.shape.minBounds.left)
        .map(_.attr.get)
    }
    rows
  }


  def addLabel(row: Int, len: Int, label: Label, parent: Label): Option[LabelShape] = {
    _addLabel(label, Some(parent), row, len)
  }

  def addLabel(row: Int, len: Int, label: Label): Option[LabelShape] = {
    _addLabel(label, None, row, len)
  }

  private def _addLabel(label: Label, parent: Option[Label], fromRow: Int, len: Int): Option[LabelShape] = {

    getMatrixContent(fromRow, len).flatMap{ content =>

      val labelStacks = getLabeledRegionStacks(content.area)

      val validUnlabeledTarget = labelStacks.isEmpty && parent.isEmpty

      val singleOverlappedLabel = labelStacks.length == 1

      val validLabeledTarget = labelStacks.length == 1 && {
        parent.exists{ parentLabel =>
          labelStacks.head.head.hasLabel(parentLabel)
        }
      }

      val realBounds = shaveMargin(matrixToRealCoords(content.area))

      if (validUnlabeledTarget) {
        val newShape = shapeIndex.indexShape{ id =>
          LabelShape(realBounds, id, None).addLabels(label)
        }
        Some(newShape.asInstanceOf[LabelShape])
      } else if (singleOverlappedLabel && parent.isDefined) {
        val parentLabel = parent.orDie("")
        val overlappedLabel = labelStacks.head.head
        val meetsLabelConstraint = overlappedLabel.hasLabel(parentLabel)
        val isSubArea = realBounds.isContainedBy(overlappedLabel.bounds)

        if (meetsLabelConstraint && isSubArea) {

          val newShape = shapeIndex.indexShape{ id =>
            LabelShape(realBounds, id, Some(overlappedLabel.id)).addLabels(label)
          }

          Some(newShape.asInstanceOf[LabelShape])
        } else None

      } else None
    }

  }

  val ts = TreeShaper[Int]

  def findLabelTrees(area: GraphPaper.Box): Seq[Tree[LabelShape]] = {
    val realBounds = shaveMargin(matrixToRealCoords(area))
    val labelShapes = shapeIndex.searchShapes(realBounds)
      .collect{ case g: LabelShape => g }

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

  def getLabeledRegionStacks(area: GraphPaper.Box): Seq[Seq[LabelShape]] = {

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
      .decodeOrDie[
        LabeledShapeIndex[GeometricFigure, TextGraphShape.Attr, TextGraphShape]
      ]()

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
    val graphPaper = new AsciiGraphPaper(width, height, false)
    for {
      (cells, rowNum) <- textGraph.getRows().zipWithIndex
      (cell, colNum) <- cells.zipWithIndex
    } {
      val graphSquare = GraphPaper.cellAt(colNum, rowNum)
      graphPaper.drawChar(graphSquare, cell.char)
    }
    graphPaper
  }



}

object ShapeLabels {

  val Cell = Label.auto


}
