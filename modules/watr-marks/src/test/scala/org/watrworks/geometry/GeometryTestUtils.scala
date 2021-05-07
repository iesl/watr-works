package org.watrworks
package geometry

import scalaz._, Scalaz._
import utils.ExactFloats._
import utils.{
  BorderLineStyle
}

object GeometryTestUtils extends WatrSpec {
  import utils.ConsoleGraphPaper
  import GraphPaper._

  def makeGraph(
    graphDimension: Rect
  ): ConsoleGraphPaper = {
    val w: Int = graphDimension.width.asInt()
    val h: Int = graphDimension.height.asInt()
    val g = new ConsoleGraphPaper(w, h)
    drawBox(g, graphDimension)
    g
  }

  def drawBox(graphPaper: ConsoleGraphPaper, region: Rect): Unit  = {
    graphPaper.drawBox(ltb2box(region), BorderLineStyle.SingleWidth)
  }
  def drawBoxBold(graphPaper: ConsoleGraphPaper, region: Rect): Unit  = {
    graphPaper.drawBox(ltb2box(region), BorderLineStyle.Bold)
  }
  def drawBoxDouble(graphPaper: ConsoleGraphPaper, region: Rect): Unit  = {
    graphPaper.drawBox(ltb2box(region), BorderLineStyle.DoubleWidth)
  }

  def assertExpectedText(expected: String, actual: String): Unit = {
    val l1s = actual.split("\n").map(_.trim())
    val l2s = expected.split("\n").map(_.trim())
    val zipped = l1s.zip(l2s)
    zipped.foreach { case (l1, l2) =>
      assertResult(l1)(l2)
    }
  }

}
