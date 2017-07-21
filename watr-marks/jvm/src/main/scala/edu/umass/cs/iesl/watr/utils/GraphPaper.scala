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

  val gridBuffer = mutable.ArrayBuffer
    .tabulate(height, width){ case (y, x) =>
      // val fcolor = fansi.Color.White("*")
      // val fcolor = fansi.Color.White("╌")
      // val fcolor = fansi.Color.Blue(" ")
      val fcolor =fansi.Color.White("░")
      fcolor

    }

  def drawCell(cell: GraphPaper.GridCell, char: Char): Unit = {
    gridBuffer(cell.y)(cell.x) = char.toString()
  }

  def drawBox(box: GraphPaper.Box, borderChars: BorderChars): Unit = {

    List(Dir.Top, Dir.Bottom, Dir.Left, Dir.Right)
      .foreach{ d =>
        box.getCells(d).foreach(cell => drawCell(cell, borderChars.charFor(d)))
      }

    List(Dir.TopLeft, Dir.TopRight, Dir.BottomLeft, Dir.BottomRight)
      .foreach{ d =>
        drawCell(box.getCell(d), borderChars.charFor(d))
      }
  }

  def fillFg(fill: Char, gridbox: GraphPaper.Box): Unit = {
    for {
      y <- gridbox.origin.y until (gridbox.origin.y+gridbox.height)
      x <- gridbox.origin.x until (gridbox.origin.x+gridbox.width)
    } {
      gridBuffer(y)(x) = fill.toString()
    }
  }

  def topLeftFrame(fill: Char, gridbox: GraphPaper.Box): Unit = {
    val y0 = gridbox.origin.y+1
    val x = gridbox.origin.x+1

    // Left
    for { y <- gridbox.origin.y+2 until (gridbox.origin.y+gridbox.height) } {
      gridBuffer(y)(x) = '┊'.toString()
    }
    gridBuffer(y0)(x) = fill.toString()

    // Top
    for { x <- gridbox.origin.x+2 until (gridbox.origin.x+gridbox.width) } {
      val y = gridbox.origin.y+1
      gridBuffer(y)(x) = '┄'.toString()
    }
  }

  def bottomRightFrame(fill: Char, gridbox: GraphPaper.Box): Unit = {
    val y0 = gridbox.origin.y+gridbox.height-2
    val x = gridbox.origin.x+gridbox.width-2

    // Right
    for { y <- gridbox.origin.y until (gridbox.origin.y+gridbox.height-2) } {
      gridBuffer(y)(x) = '│'.toString()
    }
    gridBuffer(y0)(x) = fill.toString()

    for { x <- gridbox.origin.x until (gridbox.origin.x+gridbox.width-2) } {
      gridBuffer(y0)(x) = '─'.toString()
    }
  }


  def applyBgColor(x: Int, y: Int, color: Color): Unit = {
    val xy = gridBuffer(y)(x)
    val rgb = color.toRGB
    val fansiColor = fansi.Back.True(rgb.red, rgb.green, rgb.blue)
    gridBuffer(y)(x) = fansiColor(xy)
  }

  def applyColor(x: Int, y: Int, color: Color): Unit = {
    val xy = gridBuffer(y)(x)
    val rgb = color.toRGB
    val fansiColor = fansi.Color.True(rgb.red, rgb.green, rgb.blue)
    gridBuffer(y)(x) = fansiColor(xy)
  }

  def border(gridbox: GraphPaper.Box, color: Color): Unit = {
    borderLeftRight(gridbox, color)
    borderTopBottom(gridbox, color)
  }

  def borderLeftRight(gridbox: GraphPaper.Box, color: Color): Unit = {
    for { y <- gridbox.origin.y until (gridbox.origin.y+gridbox.height) } {
      val x1 = gridbox.origin.x
      val x2 = gridbox.origin.x+gridbox.width-1
      applyBgColor(x1, y, color)
      applyBgColor(x2, y, color)
    }
  }

  def borderTopBottom(gridbox: GraphPaper.Box, color: Color): Unit = {
    for { x <- gridbox.origin.x until (gridbox.origin.x+gridbox.width) } {
      val y1 = gridbox.origin.y
      val y2 = gridbox.origin.y+gridbox.height-1
      applyBgColor(x, y1, color)
      applyBgColor(x, y2, color)
    }
  }

  def gradientHorizontal(gridbox: GraphPaper.Box): Unit = {
    var r = 20
    val g = 20
    var b = 20
    // val xstep = 256 / gridbox.width
    // val ystep = 256 / gridbox.height
    for {
      y <- gridbox.origin.y until (gridbox.origin.y+gridbox.height)
      _ <- List[Unit]({
        r = (r + 3) % 256
        b=0
      })
      x <- gridbox.origin.x until (gridbox.origin.x+gridbox.width)
    } {
      // g = (g + 3) % 256
      b = (b + 2) % 256
      val qq = gridBuffer(y)(x)
      // applyColor(x, y, color)
      gridBuffer(y)(x) = fansi.Back.True(r,g, b)(qq)
    }
  }

  def shadeBackground(gridbox: GraphPaper.Box, color: Color): Unit = {
    for {
      y <- gridbox.origin.y until (gridbox.origin.y+gridbox.height)
      x <- gridbox.origin.x until (gridbox.origin.x+gridbox.width)
    } {
      applyBgColor(x, y, color)
    }
  }

  def asString(): String = {
    if (useColor) {
      gridBuffer.map(_.mkString).mkString("\n")
    } else {
      asMonocolorString()
    }
  }

  def asMonocolorString(): String = {
    gridBuffer
      .map(_.map(_.plainText).mkString)
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
    origin: GridCell, width: Int, height: Int
  ) {
    val left: Int    = origin.x
    val top: Int     = origin.y
    val right: Int   = origin.x + width
    val bottom: Int  = origin.y + height
    val centerX: Int = left + width/2
    val centerY: Int = top + height/2

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

    def cells(x1: Int, y1: Int, x2: Int, y2: Int): Seq[GridCell] = {
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

  }
  object BorderLineStyle {
    val DoubleWidth = BorderChars( "══║║╔╗╚╝")
    val SingleWidth = BorderChars( "──││┌┐└┘")
    val Bold        = BorderChars( "━━┃┃┏┓┗┛")
  }

}
