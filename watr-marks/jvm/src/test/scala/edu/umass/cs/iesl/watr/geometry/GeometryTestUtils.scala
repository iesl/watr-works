package edu.umass.cs.iesl.watr
package geometry

import org.scalatest._

import org.scalacheck._
import scalaz._, Scalaz._
import scalaz.scalacheck.ScalaCheckBinding._
import Arbitrary._
import utils.ExactFloats._

trait ArbitraryGeometries {

  implicit def arbLTBounds: Arbitrary[LTBounds] = {
    (arbDouble |@| arbDouble |@| arbDouble |@| arbDouble)(
      LTBounds.Doubles.apply
    )
  }
}

object GeometryTestUtils extends FlatSpec {
  import utils.AsciiGraphPaper
  import utils.GraphPaper
  import GraphPaper._

  def makeGraph(
    graphDimension: LTBounds
  ): AsciiGraphPaper = {
    val w: Int = graphDimension.width.asInt()
    val h: Int = graphDimension.height.asInt()
    val g = new AsciiGraphPaper(w, h)
    drawBox(g, graphDimension)
    g
  }

  def drawBox(graphPaper: GraphPaper, region: LTBounds): Unit  = {
    graphPaper.drawBox(ltb2box(region), GraphPaper.BorderLineStyle.SingleWidth)
  }
  def drawBoxBold(graphPaper: GraphPaper, region: LTBounds): Unit  = {
    graphPaper.drawBox(ltb2box(region), GraphPaper.BorderLineStyle.Bold)
  }
  def drawBoxDouble(graphPaper: GraphPaper, region: LTBounds): Unit  = {
    graphPaper.drawBox(ltb2box(region), GraphPaper.BorderLineStyle.DoubleWidth)
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
