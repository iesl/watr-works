package org.watrworks
package geometry

import scalaz._, Scalaz._
import utils.ExactFloats._
import utils.{BorderLineStyle}

object GeometryTestUtils extends WatrSpec {
  import utils.ConsoleGraphPaper
  import GraphPaper._

  def makeGraph(
    graphDimension: Rect
  ): ConsoleGraphPaper = {
    val w: Int = graphDimension.width.asInt()
    val h: Int = graphDimension.height.asInt()
    val g      = new ConsoleGraphPaper(w, h)
    drawBox(g, graphDimension)
    g
  }

  def drawBox(graphPaper: ConsoleGraphPaper, region: Rect): Unit = {
    graphPaper.drawBox(ltb2box(region), BorderLineStyle.SingleWidth)
  }
  def drawBoxBold(graphPaper: ConsoleGraphPaper, region: Rect): Unit = {
    graphPaper.drawBox(ltb2box(region), BorderLineStyle.Bold)
  }
  def drawBoxDouble(graphPaper: ConsoleGraphPaper, region: Rect): Unit = {
    graphPaper.drawBox(ltb2box(region), BorderLineStyle.DoubleWidth)
  }

  def assertExpectedText(expected: String, actual: String): Unit = {
    val l1s    = actual.split("\n").map(_.trim())
    val l2s    = expected.split("\n").map(_.trim())
    val zipped = l1s.zip(l2s)
    zipped.foreach { case (l1, l2) =>
      assertResult(l1)(l2)
    }
  }

}

object pngDrawing {
  import cats.implicits._

  import doodle.java2d.{Algebra => _, _}
  import doodle.syntax.{circle => circle_, text => text_, rectangle => rectangle_, _}
  import doodle.language.Basic
  import doodle.algebra.{Picture, Text}
  import doodle.effect.Writer._

  type MyAlgebra[x[_]] = Basic[x] with Text[x]
  type MyPicture       = Picture[MyAlgebra, Drawing, Unit]

  def circle    = circle_[MyAlgebra, Drawing](_)
  def text      = text_[MyAlgebra, Drawing](_)
  def rectangle = rectangle_[MyAlgebra, Drawing](_, _)

  def drawRect(rect: Rect, title: Option[String]): MyPicture = {
    val offsetOriginX = rect.getWidth / 2
    val offsetOriginY = -rect.getHeight / 2
    val offsetLocX    = rect.getLeft
    val offsetLocY    = -rect.getTop

    val offsetX = offsetOriginX + offsetLocX
    val offsetY = offsetOriginY + offsetLocY

    val rec = rectangle(rect.getWidth, rect.getHeight)

    title
      .map(s => {
        text(s)
          .scale(0.5, 0.5)
          .on(rec)
          .at(offsetX, offsetY)
      })
      .getOrElse(
        rec.at(offsetX, offsetY)
      )
  }

  implicit class RicherPicture(self: MyPicture) {
    def save(fpath: String) = {
      self.write[Png](fpath)
    }
  }

  import geometry.syntax._
  import doodle.core._

  def drawOctothorpe(outer: Rect, inner: Rect): MyPicture = {
    val burstRegions = inner.withinRegion(outer).burstAllPossibleDirections()

    val inner1 = drawRect(inner, Some("inner"))
      .fillColor(Color.rgba(100, 0, 0, 0.3))
      .strokeColor(Color.blue)
      .strokeWidth(2)

    val outer1 = drawRect(outer, Some("outer"))
      .fillColor(Color.rgba(0, 0, 100, 0.1))
      .strokeColor(Color.black)
      .strokeWidth(3)

    val adjacents = for {
      (dir, maybeRect: Option[Rect]) <- burstRegions
      rect                           <- maybeRect
    } yield {
      drawRect(rect, Some(s"${dir.toString()}"))
        .fillColor(Color.rgba(0, 100, 0, 0.3))
        .strokeColor(Color.blue)
        .strokeWidth(1)
    }

    val composed = (inner1 :: outer1 :: adjacents.to(List)).allOn

    composed
  }

}
