package edu.umass.cs.iesl.watr
package utils

import geometry._
import scala.collection.mutable
import utils.{RelativeDirection => Dir}

class GraphPaper(
  width: Int, height: Int, useColor: Boolean=true
) {

  import GraphPaper._

  lazy val box = Box(GridCell(0, 0), width, height)
  lazy val bbox = GraphPaper.box2ltb(box)

  val charBuffer: mutable.ArrayBuffer[
    mutable.ArrayBuffer[Char]
  ] = mutable.ArrayBuffer.tabulate(height, width){ case (y, x) =>
      '░'
  }

  val charMods: mutable.ArrayBuffer[
    mutable.ArrayBuffer[fansi.Attrs]
  ] = mutable.ArrayBuffer.tabulate(height, width){ case (y, x) =>
      fansi.Attrs.Empty
  }

  def drawString(x: Int, y: Int, str: String): Unit = {
    str.zipWithIndex.foreach{ case (ch, i) =>
      drawCell(GraphPaper.GridCell(x+i, y), ch)
    }
  }

  def drawCell(cell: GraphPaper.GridCell, char: Char): Unit = {
    charBuffer(cell.y)(cell.x) = char
  }


  def drawBox(box: GraphPaper.Box, borderChars: BorderChars = BorderLineStyle.SingleWidth): Unit = {

    if (box.spanRight==0 && box.spanDown==0) {
      box.getCells(Dir.Left).foreach { cell =>
        drawCell(cell, borderChars.endcapFor(Dir.Center))
      }
    } else if (box.spanRight==0) {
      box.getCells(Dir.Left).foreach { cell =>
        drawCell(cell, borderChars.char1For(Dir.Left))
      }
      box.getCells(Dir.Top).foreach { cell =>
        drawCell(cell, borderChars.endcapFor(Dir.Top))
      }
      box.getCells(Dir.Bottom).foreach { cell =>
        drawCell(cell, borderChars.endcapFor(Dir.Bottom))
      }
    } else if (box.spanDown==0) {
      box.getCells(Dir.Top).foreach { cell =>
        drawCell(cell, borderChars.char1For(Dir.Top))
      }
      box.getCells(Dir.Left).foreach { cell =>
        drawCell(cell, borderChars.endcapFor(Dir.Left))
      }
      box.getCells(Dir.Right).foreach { cell =>
        drawCell(cell, borderChars.endcapFor(Dir.Right))
      }
    } else {
      List(Dir.Top, Dir.Bottom, Dir.Left, Dir.Right)
        .foreach{ d =>
          box.getCells(d).foreach(cell => drawCell(cell, borderChars.charFor(d)))
        }

      List(Dir.TopLeft, Dir.TopRight, Dir.BottomLeft, Dir.BottomRight)
        .foreach{ d =>
          box.getCells(d).foreach(cell => drawCell(cell, borderChars.charFor(d)))
        }

    }

  }


  def fillFg(fill: Char, gridbox: GraphPaper.Box): Unit = {
    for {
      y <- gridbox.origin.y until (gridbox.origin.y+gridbox.spanDown)
      x <- gridbox.origin.x until (gridbox.origin.x+gridbox.spanRight)
    } {
      charBuffer(y)(x) = fill
    }
  }

  def topLeftFrame(fill: Char, gridbox: GraphPaper.Box): Unit = {
    val y0 = gridbox.origin.y+1
    val x = gridbox.origin.x+1

    // Left
    for { y <- gridbox.origin.y+2 until (gridbox.origin.y+gridbox.spanDown) } {
      charBuffer(y)(x) = '┊'
    }
    charBuffer(y0)(x) = fill

    // Top
    for { x <- gridbox.origin.x+2 until (gridbox.origin.x+gridbox.spanRight) } {
      val y = gridbox.origin.y+1
      charBuffer(y)(x) = '┄'
    }
  }

  def bottomRightFrame(fill: Char, gridbox: GraphPaper.Box): Unit = {
    val y0 = gridbox.origin.y+gridbox.spanDown-2
    val x = gridbox.origin.x+gridbox.spanRight-2

    // Right
    for { y <- gridbox.origin.y until (gridbox.origin.y+gridbox.spanDown-2) } {
      charBuffer(y)(x) = '│'
    }
    charBuffer(y0)(x) = fill

    for { x <- gridbox.origin.x until (gridbox.origin.x+gridbox.spanRight-2) } {
      charBuffer(y0)(x) = '─'
    }
  }


  def applyBgColor(x: Int, y: Int, color: Color): Unit = {
    val xy = charMods(y)(x)
    val rgb = color.toRGB
    val fansiColor = fansi.Back.True(rgb.red, rgb.green, rgb.blue)
    charMods(y)(x) = xy ++ fansiColor
  }

  def applyColor(x: Int, y: Int, color: Color): Unit = {
    val xy = charMods(y)(x)
    val rgb = color.toRGB
    val fansiColor = fansi.Color.True(rgb.red, rgb.green, rgb.blue)
    charMods(y)(x) = xy ++ fansiColor
  }

  def border(gridbox: GraphPaper.Box, color: Color): Unit = {
    borderLeftRight(gridbox, color)
    borderTopBottom(gridbox, color)
  }

  def borderLeftRight(gridbox: GraphPaper.Box, color: Color): Unit = {
    for { y <- gridbox.origin.y until (gridbox.origin.y+gridbox.spanDown) } {
      val x1 = gridbox.origin.x
      val x2 = gridbox.origin.x+gridbox.spanRight-1
      applyBgColor(x1, y, color)
      applyBgColor(x2, y, color)
    }
  }

  def borderTopBottom(gridbox: GraphPaper.Box, color: Color): Unit = {
    for { x <- gridbox.origin.x until (gridbox.origin.x+gridbox.spanRight) } {
      val y1 = gridbox.origin.y
      val y2 = gridbox.origin.y+gridbox.spanDown-1
      applyBgColor(x, y1, color)
      applyBgColor(x, y2, color)
    }
  }

  def gradientHorizontal(gridbox: GraphPaper.Box): Unit = {
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
      val qq = charMods(y)(x)
      charMods(y)(x) = qq ++ fansi.Back.True(r,g,b)
    }
  }

  def shadeBackground(gridbox: GraphPaper.Box, color: Color): Unit = {
    for {
      y <- gridbox.origin.y until (gridbox.origin.y+gridbox.spanDown)
      x <- gridbox.origin.x until (gridbox.origin.x+gridbox.spanRight)
    } {
      applyBgColor(x, y, color)
    }
  }

  def asColorString(): String = {
    if (useColor) {
      val rws = charBuffer.zipWithIndex.map{ case (charRow, rowNum) =>
        val chs = charRow.zipWithIndex.map{ case (char, colNum) =>
          val mod = charMods(rowNum)(colNum)
          mod(char.toString())
        }
        chs.mkString
      }
      rws.mkString("\n")
    } else {
      asMonocolorString()
    }
  }

  def asMonocolorString(): String = {
    charBuffer
      .map(_.mkString)
      .mkString("\n")
  }

}


object GraphPaper {

  def box2ltb(box: Box): LTBounds = {
    val Box(GridCell(l, t), w, h) = box
    LTBounds.Ints(l, t, w, h)
  }

  def ltb2box(bbox: LTBounds): GraphPaper.Box = {
    val LTBounds.Ints(l, t, w, h) = bbox
    GraphPaper.Box(GridCell(l, t), w, h)
  }

  case class GridCell(
    x: Int, y: Int
  )

  case class Box(
    origin: GridCell, spanRight: Int, spanDown: Int
  ) {
    val left: Int    = origin.x
    val top: Int     = origin.y
    val right: Int   = origin.x + spanRight
    val bottom: Int  = origin.y + spanDown
    val centerX: Int = left + spanRight/2
    val centerY: Int = top + spanDown/2

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

    private def cells(x1: Int, y1: Int, x2: Int, y2: Int): Seq[GridCell] = {
      for {
        y <- y1 to y2
        x <- x1 to x2
      } yield GridCell(x, y)
    }

  }

  def create(w: Int, h: Int, useColor: Boolean=false) = {
    new GraphPaper(w, h, useColor)
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

  object BorderLineStyle {
    //                             0123456789
    val DoubleWidth = BorderChars("══║║╔╗╚╝◻╦╩╠╣")
    val SingleWidth = BorderChars("──││┌┐└┘◻┬┴├┤")
    val Bold        = BorderChars("━━┃┃┏┓┗┛◻┳┻┣┫")
  }

}
