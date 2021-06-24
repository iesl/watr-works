package org.watrworks
package geometry

import utils.{M3x3Position => M3}

object GraphPaper {

  def ltb2box(bbox: Rect): Box = {
    val Rect.Ints(l, t, w, h) = bbox
    GraphPaper.Box(GridCell(l, t), w - 1, h - 1)
  }

  def boundsToBox(bbox: Rect): Box = ltb2box(bbox)

  case class CellDimensions(width: Int, height: Int)

  case class GridCell(x: Int, y: Int) {
    def toBox(): Box = Box(this, 0, 0)
  }

  def cellAt(x: Int, y: Int) = GridCell(x, y)
  def boxAt(x: Int, y: Int)  = Box(cellAt(x, y), 0, 0)

  case class Box(origin: GridCell, spanRight: Int, spanDown: Int) {
    val left: Int    = origin.x
    val top: Int     = origin.y
    val right: Int   = origin.x + spanRight
    val bottom: Int  = origin.y + spanDown
    val centerX: Int = left + spanRight / 2
    val centerY: Int = top + spanDown / 2
    val width: Int   = 1 + spanRight
    val height: Int  = 1 + spanDown

    def extendRight(x: Int): Box = {
      this.copy(spanRight = spanRight + x)
    }

    def extendDown(y: Int): Box = {
      this.copy(spanDown = spanDown + y)
    }

    def setWidth(w: Int): Box = {
      this.copy(spanRight = w - 1)
    }
    def setHeight(h: Int): Box = {
      this.copy(spanDown = h - 1)
    }

    def translate(x: Int, y: Int): Box = {
      Box(
        origin.copy(origin.x + x, origin.y + y),
        spanRight,
        spanDown
      )
    }

    def union(box2: Box): Box = {
      val minX = math.min(origin.x, box2.origin.x)
      val minY = math.min(origin.y, box2.origin.y)

      val maxRight  = math.max(right, box2.right)
      val maxBottom = math.max(bottom, box2.bottom)

      Box(GridCell(minX, minY), maxRight - minX, maxBottom - minY)
    }

    def toRect(): Rect = {
      Rect.Ints(left, top, spanRight + 1, spanDown + 1)
    }

    def getCell(dir: M3): GridCell = {
      dir match {
        case M3.Top         => GridCell(centerX, top)
        case M3.Bottom      => GridCell(centerX, bottom)
        case M3.Right       => GridCell(right, centerY)
        case M3.Left        => GridCell(left, centerY)
        case M3.TopLeft     => origin
        case M3.BottomLeft  => GridCell(left, bottom)
        case M3.TopRight    => GridCell(right, top)
        case M3.BottomRight => GridCell(right, bottom)
        case M3.Center      => GridCell(centerX, centerY)
      }
    }
    def getCells(dir: M3): Seq[GridCell] = {
      dir match {
        case M3.Top    => cells(left, top, right, top)
        case M3.Bottom => cells(left, bottom, right, bottom)
        case M3.Left   => cells(left, top, left, bottom)
        case M3.Right  => cells(right, top, right, bottom)
        case _          => Seq(getCell(dir))
      }
    }

    def getCells(): Seq[GridCell] =
      cells(origin.x, origin.y, origin.x + spanRight, origin.y + spanDown)

    private def cells(
      x1: Int,
      y1: Int,
      x2: Int,
      y2: Int
    ): Seq[GridCell] = {
      for {
        y <- y1 to y2
        x <- x1 to x2
      } yield GridCell(x, y)
    }

    def getRows(): Seq[Box] = {
      (top to bottom).map { y => boxAt(left, y).setWidth(width) }
    }
  }

  def union(boxes: Seq[Box]): Option[Box] = {
    boxes.headOption.map { bhead => boxes.tail.foldLeft(bhead)(_ union _) }
  }
}
