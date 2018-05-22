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

/**
  *
  *  Coordinate Systems:
  *    Matrix Coords
  *       (row, col); represented by Graph paper Cell/Box classes
  *
  *    Real Coords
  *       (x, y, width, height); represented by Floats (as defined in LTBounds)
  **/

sealed trait TextGraphShape extends LabeledShape[GeometricFigure, GridCell]

object TextGraphShape {

  case class GlyphShape(
    shape: LTBounds,
    id: Int@@ShapeID,
    attr: GridCell
  ) extends TextGraphShape {
    def labels(): Set[Label] = Set()
    def addLabels(l: Label*): GlyphShape = copy(
      // labels = this.labels ++ l.toSet
    )
  }

  type Shape = TextGraphShape


}


class TextGraphJvm extends TextGraph {
  import TextGraphShape._

  val shapeIndex = new LabeledShapeIndex[GeometricFigure, GridCell, TextGraphShape]()
  val L = ShapeLabels

  val cellWidth = 10d
  val cellHeight = 10d
  val cellMargin = 0.1

  def shaveMargin(b: LTBounds): LTBounds = b.shave(cellMargin)

  def matrixToRealCoords(box: GraphPaper.Box): LTBounds = {
    val width = (box.spanRight+1) * cellWidth
    val height = (box.spanDown+1) * cellHeight
    LTBounds.Doubles(
      box.origin.x * cellWidth,
      box.origin.y * cellHeight,
      width, height
    )
  }

  def matrixToRealCoords(gpCell: GraphPaper.GridCell): LTBounds = {
    LTBounds.Doubles(
      gpCell.x * cellWidth,
      gpCell.y * cellHeight,
      cellWidth,
      cellHeight
    )
  }

  def scaledSquare(x: Int, y: Int): Unit = {
    val graphSquare = GraphPaper.cellAt(x, y)
  }

  def rowNumForY(y: FloatExact): Int = {
    math.floor(y.asDouble() / cellHeight).toInt
  }

  def colNumForX(x: FloatExact): Int = {
    math.floor(x.asDouble() / cellWidth).toInt
  }

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


  def appendRow(cells: Seq[GridCell]): Unit = {
    val nextRow = graphHeight()
    for {
      (cell, i) <- cells.zipWithIndex
    } {
      val graphSquare = GraphPaper.cellAt(i, nextRow)
      val realBounds = shaveMargin(matrixToRealCoords(graphSquare))

      shapeIndex.indexShape{ id =>
        GlyphShape(realBounds, id, cell)
      }
    }
  }

  def getRows(): Seq[Seq[GridCell]] = {
    val height = graphHeight()
    val width = graphWidth()

    val rowBoxes = for {
      y <- 0 until height
    } yield GraphPaper.boxAt(0, y).modifySpan(x=width-1, y=0)

    val rows = rowBoxes.toList.map{ box =>
      val realBox = matrixToRealCoords(box)
      val rowQuery = shaveMargin(realBox)
      shapeIndex.searchShapes(rowQuery)
        .sortBy(_.shape.minBounds.left)
        .map(_.attr)
    }
    rows
  }

}

object ShapeLabels {

  val Cell = Label.auto

}


object TextGraphBuilder {

  def create(): TextGraph = {
    new TextGraphJvm {

    }
  }

}
