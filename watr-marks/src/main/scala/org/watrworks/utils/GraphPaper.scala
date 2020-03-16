package org.watrworks
package utils

import geometry._
import utils.{RelativeDirection => Dir}
// import scala.scalajs.js.annotation._

import scala.{ collection => sc }
import sc.Seq

abstract class GraphPaper {

  import GraphPaper._

  def drawChar(cell: GridCell, char: Char): Unit
  def drawBox(box: Box, borderChars: BorderChars = BorderLineStyle.SingleWidth): Unit
  def applyBgColor(box: Box, color: Color): Unit
  def applyFgColor(box: Box, color: Color): Unit
  def gradientHorizontal(gridbox: Box): Unit
  def cellDimensions(): CellDimensions

  def drawString(x: Int, y: Int, str: String): Unit = {
    str.zipWithIndex.foreach{ case (ch, i) =>
      drawChar(GraphPaper.GridCell(x+i, y), ch)
    }
  }

  def fillChar(box: Box, fill: Char): Unit = {
    box.getCells().foreach {cell =>
      drawChar(cell, fill)
    }
  }

  def border(gridbox: Box, color: Color): Unit = {
    borderLeftRight(gridbox, color)
    borderTopBottom(gridbox, color)
  }

  def borderLeftRight(gridbox: Box, color: Color): Unit = {
    for { y <- gridbox.origin.y until (gridbox.origin.y+gridbox.spanDown) } {
      val x1 = gridbox.origin.x
      val x2 = gridbox.origin.x+gridbox.spanRight-1
      applyBgColor(boxAt(x1, y), color)
      applyBgColor(boxAt(x2, y), color)
    }
  }

  def borderTopBottom(gridbox: Box, color: Color): Unit = {
    for { x <- gridbox.origin.x until (gridbox.origin.x+gridbox.spanRight) } {
      val y1 = gridbox.origin.y
      val y2 = gridbox.origin.y+gridbox.spanDown-1
      applyBgColor(boxAt(x, y1), color)
      applyBgColor(boxAt(x, y2), color)
    }
  }


  def shadeBackground(gridbox: Box, color: Color): Unit = {
    for {
      y <- gridbox.origin.y until (gridbox.origin.y+gridbox.spanDown)
      x <- gridbox.origin.x until (gridbox.origin.x+gridbox.spanRight)
    } {
      applyBgColor(boxAt(x, y), color)
    }
  }
}




object GraphPaper {

  def ltb2box(bbox: LTBounds): Box = {
    val LTBounds.Ints(l, t, w, h) = bbox
    GraphPaper.Box(GridCell(l, t), w-1, h-1)
  }

  def boundsToBox(bbox: LTBounds): Box = ltb2box(bbox)

  
  case class CellDimensions(
    width: Int,
    height: Int
  )

 
  case class GridCell(
    x: Int, y: Int
  ) {
    def toBox(): Box = Box(this, 0, 0)
  }

  def cellAt(x: Int, y: Int) = GridCell(x, y)
  def boxAt(x: Int, y: Int) = Box(cellAt(x, y), 0, 0)


  case class Box(
    origin: GridCell, spanRight: Int, spanDown: Int
  ) {
    val left: Int    = origin.x
    val top: Int     = origin.y
    val right: Int   = origin.x + spanRight
    val bottom: Int  = origin.y + spanDown
    val centerX: Int = left + spanRight/2
    val centerY: Int = top + spanDown/2
    val width: Int   = 1 + spanRight
    val height: Int  = 1 + spanDown


    def extendRight(x: Int): Box = {
      this.copy(spanRight = spanRight+x)
    }

    def extendDown(y: Int): Box = {
      this.copy(spanDown = spanDown+y)
    }

    def setWidth(w: Int): Box = {
      this.copy(spanRight = w-1)
    }
    def setHeight(h: Int): Box = {
      this.copy(spanDown = h-1)
    }

    def translate(x: Int, y: Int): Box = {
      Box(
        origin.copy(origin.x+x, origin.y+y),
        spanRight, spanDown
      )
    }

    def union(box2: Box): Box = {
      val minX = math.min(origin.x, box2.origin.x)
      val minY = math.min(origin.y, box2.origin.y)

      val maxRight = math.max(right, box2.right)
      val maxBottom = math.max(bottom, box2.bottom)

      Box(GridCell(minX, minY),
        maxRight - minX,
        maxBottom - minY
      )
    }


    def toLTBounds(): LTBounds = {
      LTBounds.Ints(left, top, spanRight+1, spanDown+1)
    }

    def getCell(dir: Dir): GridCell = {
      dir match {
        case Dir.Top         => GridCell(centerX, top)
        case Dir.Bottom      => GridCell(centerX, bottom)
        case Dir.Right       => GridCell(right, centerY)
        case Dir.Left        => GridCell(left, centerY)
        case Dir.TopLeft     => origin
        case Dir.BottomLeft  => GridCell(left, bottom)
        case Dir.TopRight    => GridCell(right, top)
        case Dir.BottomRight => GridCell(right, bottom)
        case Dir.Center      => GridCell(centerX, centerY)
      }
    }
    def getCells(dir: Dir): Seq[GridCell] = {
      dir match {
        case Dir.Top         => cells(left, top, right, top)
        case Dir.Bottom      => cells(left, bottom, right, bottom)
        case Dir.Left        => cells(left, top, left, bottom)
        case Dir.Right       => cells(right, top, right, bottom)
        case _               => Seq(getCell(dir))
      }
    }

    def getCells(): Seq[GridCell] = cells(origin.x, origin.y, origin.x+spanRight, origin.y+spanDown)

    private def cells(x1: Int, y1: Int, x2: Int, y2: Int): Seq[GridCell] = {
      for {
        y <- y1 to y2
        x <- x1 to x2
      } yield GridCell(x, y)
    }

    def getRows(): Seq[Box] = {
      (top to bottom).map{ y =>
        boxAt(left, y).setWidth(width)
      }
    }
  }

  case class BorderChars(str: String) {
    val chars = str.toCharArray()
    def charFor(dir: Dir) = dir match {
      case Dir.Top         => chars(0)
      case Dir.Bottom      => chars(1)
      case Dir.Right       => chars(2)
      case Dir.Left        => chars(3)
      case Dir.TopLeft     => chars(4)
      case Dir.TopRight    => chars(5)
      case Dir.BottomLeft  => chars(6)
      case Dir.BottomRight => chars(7)
      case Dir.Center      => ' '
    }

    def char1For(dir: Dir) = dir match {
      case Dir.Top         => chars(0)
      case Dir.Left        => chars(2)
      case _ => sys.error(s"no char1 for dir ${dir}")
    }

    def endcapFor(dir: Dir) = dir match {
      case Dir.Center      => chars(8)
      case Dir.Top         => chars(9+0)
      case Dir.Bottom      => chars(9+1)
      case Dir.Left        => chars(9+2)
      case Dir.Right       => chars(9+3)
      case _ => sys.error(s"no endcaps for dir ${dir}")
    }
  }

  def union(boxes: Seq[Box]): Option[Box]= {
    boxes.headOption.map{ bhead =>
      boxes.tail.foldLeft(bhead)(_ union _)
    }
  }

 
  object BorderLineStyle {
    //                             0123456789
    val DoubleWidth = BorderChars("══║║╔╗╚╝◻╦╩╠╣")
    val SingleWidth = BorderChars("──││┌┐└┘◻┬┴├┤")
    val Bold        = BorderChars("━━┃┃┏┓┗┛◻┳┻┣┫")
  }

}

import scala.collection.mutable

class CharBasedGraphPaper(
  val initWidth: Int,
  val initHeight: Int,
  useColor: Boolean=true
) extends GraphPaper {

  import GraphPaper._

  var width: Int = initWidth
  var height: Int = initHeight


  val charBuffer: mutable.ArrayBuffer[
    mutable.ArrayBuffer[Char]
  ] = mutable.ArrayBuffer.tabulate(height, width){ case (y, x) =>
      '░'
  }

  def cellDimensions(): CellDimensions= {
    CellDimensions(1, 1)
  }

  val colorMods: mutable.ArrayBuffer[
    mutable.ArrayBuffer[fansi.Attrs]
  ] = mutable.ArrayBuffer.tabulate(height, width){ case (y, x) =>
      fansi.Attrs.Empty
  }

  val charMods: mutable.ArrayBuffer[
    mutable.ArrayBuffer[fansi.Attrs]
  ] = mutable.ArrayBuffer.tabulate(height, width){ case (y, x) =>
      fansi.Attrs.Empty
  }

  private def resizeToFit(cell: GridCell): Unit = {
    val vpad = cell.y - height + 1
    val hpad = cell.x - width + 1
    if (vpad > 0) {
      charBuffer.appendAll(
        Array.fill(vpad)(mutable.ArrayBuffer.fill(width)('░'))
      )
      charMods.appendAll(
        Array.fill(vpad)(mutable.ArrayBuffer.fill(width)(fansi.Attrs.Empty))
      )
      colorMods.appendAll(
        Array.fill(vpad)(mutable.ArrayBuffer.fill(width)(fansi.Attrs.Empty))
      )
      height = height + vpad
    }
    if (hpad > 0) {
      charBuffer.foreach { lineBuffer =>
        lineBuffer.appendAll("░" * hpad)
      }
      charMods.foreach { lineBuffer =>
        lineBuffer.appendAll(Array.fill(hpad)(fansi.Attrs.Empty))
      }
      colorMods.foreach { lineBuffer =>
        lineBuffer.appendAll(Array.fill(hpad)(fansi.Attrs.Empty))
      }
      width = width + hpad
    }

  }
  def drawChar(cell: GridCell, char: Char): Unit = {
    resizeToFit(cell)
    charBuffer(cell.y)(cell.x) = char
  }

  def drawBox(box: Box, borderChars: BorderChars = BorderLineStyle.SingleWidth): Unit = {

    if (box.spanRight==0 && box.spanDown==0) {
      box.getCells(Dir.Left).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(Dir.Center))
      }
    } else if (box.spanRight==0) {
      box.getCells(Dir.Left).foreach { cell =>
        drawChar(cell, borderChars.char1For(Dir.Left))
      }
      box.getCells(Dir.Top).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(Dir.Top))
      }
      box.getCells(Dir.Bottom).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(Dir.Bottom))
      }
    } else if (box.spanDown==0) {
      box.getCells(Dir.Top).foreach { cell =>
        drawChar(cell, borderChars.char1For(Dir.Top))
      }
      box.getCells(Dir.Left).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(Dir.Left))
      }
      box.getCells(Dir.Right).foreach { cell =>
        drawChar(cell, borderChars.endcapFor(Dir.Right))
      }
    } else {
      List(Dir.Top, Dir.Bottom, Dir.Left, Dir.Right)
        .foreach{ d =>
          box.getCells(d).foreach(cell => drawChar(cell, borderChars.charFor(d)))
        }

      List(Dir.TopLeft, Dir.TopRight, Dir.BottomLeft, Dir.BottomRight)
        .foreach{ d =>
          box.getCells(d).foreach(cell => drawChar(cell, borderChars.charFor(d)))
        }

    }
  }

  private def modColor(cell: GridCell, a: fansi.Attrs): Unit = {
    val y = cell.y
    val x = cell.x
    val qq = colorMods(y)(x)
    colorMods(y)(x) = qq ++ a
  }
  private def modFormat(cell: GridCell, a: fansi.Attrs): Unit = {
    val y = cell.y
    val x = cell.x
    val qq = charMods(y)(x)
    charMods(y)(x) = qq ++ a
  }

  def underline(box: Box): Unit = {
    box.getCells().foreach { cell =>
      modFormat(cell, fansi.Underlined.On)
    }
  }

  def applyBgColor(box: Box, color: Color): Unit = {
    val rgb = color
    val fansiColor = fansi.Back.True(rgb.red, rgb.green, rgb.blue)
    box.getCells().foreach { cell =>
      modColor(cell, fansiColor)
    }
  }


  def applyFgColor(box: Box, color: Color): Unit = {
    val rgb = color
    val fansiColor = fansi.Color.True(rgb.red, rgb.green, rgb.blue)
    box.getCells().foreach { cell =>
      modColor(cell, fansiColor)
    }
  }

  def gradientHorizontal(gridbox: Box): Unit = {
    var r = 20
    val g = 20
    var b = 20
    // val xstep = 256 / gridbox.spanRight
    // val ystep = 256 / gridbox.spanDown
    for {
      y <- gridbox.origin.y until (gridbox.origin.y+gridbox.spanDown)
      _ <- List[Unit]({
        r = (r + 3) % 256
        b=0
      })
      x <- gridbox.origin.x until (gridbox.origin.x+gridbox.spanRight)
    } {
      // g = (g + 3) % 256
      b = (b + 2) % 256
      val qq = colorMods(y)(x)
      colorMods(y)(x) = qq ++ fansi.Back.True(r,g,b)
    }
  }

  def asColorString(): String = {
    if (useColor) {
      val rws = charBuffer.zipWithIndex.map{ case (charRow, rowNum) =>
        val chs = charRow.zipWithIndex.map{ case (char, colNum) =>
          val clr = colorMods(rowNum)(colNum)
          val fmt = charMods(rowNum)(colNum)
          clr(fmt(char.toString()))
        }
        chs.mkString
      }
      rws.mkString("\n")
    } else {
      asMonocolorString()
    }
  }

  def asMonocolorString(): String = {
    val rws = charBuffer.zipWithIndex.map{ case (charRow, rowNum) =>
      val chs = charRow.zipWithIndex.map{ case (char, colNum) =>
        val fmt = charMods(rowNum)(colNum)
        fmt(char.toString())
      }
      chs.mkString
    }
    rws.mkString("\n")
  }

}
