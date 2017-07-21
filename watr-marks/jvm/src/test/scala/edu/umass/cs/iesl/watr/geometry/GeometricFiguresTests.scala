package edu.umass.cs.iesl.watr
package geometry

import org.scalatest._

import org.scalacheck._
import scalaz._, Scalaz._
import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck.Prop._
import Arbitrary._
import utils.{RelativeDirection => Dir}
import utils.ExactFloats._

trait ArbitraryGeometries {

  implicit def arbLTBounds: Arbitrary[LTBounds] = {
    (arbDouble |@| arbDouble |@| arbDouble |@| arbDouble)(
      LTBounds.Doubles.apply
    )
  }
}

object GeometricFigureProperties extends Properties("GeometricFigureProperties") with ArbitraryGeometries {
  property("json <--> LTBounds") = forAll{ (bounds: LTBounds) =>
    true
  }
}

object GeometryTestUtils extends FlatSpec {
  import utils.GraphPaper
  import GraphPaper._

  def makeGraph(
    graphDimension: LTBounds
  ): GraphPaper = {
    val w: Int = graphDimension.width.asInt()
    val h: Int = graphDimension.height.asInt()
    GraphPaper.create(w+1, h+1)
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

class GeometricFigureTests  extends FlatSpec with Matchers {
  behavior of "Geometric Figures"

  import GeometryImplicits._
  import GeometryTestUtils._
  import textboxing.{TextBoxing => TB}, TB._

  it should "split figures h/v" in {
    val graphSize = LTBounds.Ints(0, 0, 14, 14)
    val outer = LTBounds.Ints(1, 1, 10, 10)
    val inner = LTBounds.Ints(3, 4, 5, 2)


    {
      val boxes = List(Dir.Left, Dir.Center, Dir.Right, Dir.Top, Dir.Bottom)
        .flatMap{ dir =>
          inner.withinRegion(outer)
            .adjacentRegion(dir)
            .map{ adjacent =>
              val g = makeGraph(graphSize)
              drawBox(g, graphSize)
              drawBoxDouble(g, outer)
              drawBox(g, inner)
              drawBoxBold(g, adjacent)
              g.asMonocolorString().mbox
            }
        }

      val actual = hcat(boxes).toString()
      val expectedOutput = {
        """|┌─────────────┐┌─────────────┐┌─────────────┐┌─────────────┐┌─────────────┐
           |│╔═════════╗░░││╔═════════╗░░││╔═════════╗░░││╔═┏━━━━┓══╗░░││╔═════════╗░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░┃░░░░┃░░║░░││║░░░░░░░░░║░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░┃░░░░┃░░║░░││║░░░░░░░░░║░░│
           |│┏━┓────┐░░║░░││║░┏━━━━┓░░║░░││║░┌────┏━━┓░░││║░┗━━━━┛░░║░░││║░┌────┐░░║░░│
           |│┃░┃░░░░│░░║░░││║░┃░░░░┃░░║░░││║░│░░░░┃░░┃░░││║░│░░░░│░░║░░││║░│░░░░│░░║░░│
           |│┗━┛────┘░░║░░││║░┗━━━━┛░░║░░││║░└────┗━━┛░░││║░└────┘░░║░░││║░┏━━━━┓░░║░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░┃░░░░┃░░║░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░┃░░░░┃░░║░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░┃░░░░┃░░║░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░┃░░░░┃░░║░░│
           |│╚═════════╝░░││╚═════════╝░░││╚═════════╝░░││╚═════════╝░░││╚═┗━━━━┛══╝░░│
           |│░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░│
           |│░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░│
           |└─────────────┘└─────────────┘└─────────────┘└─────────────┘└─────────────┘
           |""".stripMargin
      }
      assertExpectedText(expectedOutput, actual)
    }

    {
      val boxes = List(Dir.TopLeft, Dir.TopRight, Dir.BottomLeft, Dir.BottomRight)
        .flatMap{ dir =>
          inner.withinRegion(outer)
            .adjacentRegion(dir)
            .map{ adjacent =>
              val g = makeGraph(graphSize)
              drawBox(g, graphSize)
              drawBoxDouble(g, outer)
              drawBox(g, inner)
              drawBoxBold(g, adjacent)
              g.asMonocolorString().mbox
            }
        }

      val actual = hcat(boxes).toString()
      val expectedOutput = {

        """|┌─────────────┐┌─────────────┐┌─────────────┐┌─────────────┐
           |│┏━┓═══════╗░░││╔══════┏━━┓░░││╔═════════╗░░││╔═════════╗░░│
           |│┃░┃░░░░░░░║░░││║░░░░░░┃░░┃░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░│
           |│┃░┃░░░░░░░║░░││║░░░░░░┃░░┃░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░│
           |│┗━┛────┐░░║░░││║░┌────┗━━┛░░││║░┌────┐░░║░░││║░┌────┐░░║░░│
           |│║░│░░░░│░░║░░││║░│░░░░│░░║░░││║░│░░░░│░░║░░││║░│░░░░│░░║░░│
           |│║░└────┘░░║░░││║░└────┘░░║░░││┏━┓────┘░░║░░││║░└────┏━━┓░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││┃░┃░░░░░░░║░░││║░░░░░░┃░░┃░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││┃░┃░░░░░░░║░░││║░░░░░░┃░░┃░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││┃░┃░░░░░░░║░░││║░░░░░░┃░░┃░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││┃░┃░░░░░░░║░░││║░░░░░░┃░░┃░░│
           |│╚═════════╝░░││╚═════════╝░░││┗━┛═══════╝░░││╚══════┗━━┛░░│
           |│░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░│
           |│░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░│
           |└─────────────┘└─────────────┘└─────────────┘└─────────────┘
           |""".stripMargin
      }

      assertExpectedText(expectedOutput, actual)
    }

    {
      val boxes = List(
        List(Dir.Left, Dir.Center),
        List(Dir.Left, Dir.Right),
        List(Dir.Right, Dir.Center),
        List(Dir.Left, Dir.Bottom),
        List(Dir.Right, Dir.BottomRight),
        List(Dir.Right, Dir.Bottom)
      ).flatMap{ dirs =>
        inner.withinRegion(outer)
          .adjacentRegions(dirs:_*)
          .map{ adjacent =>
            val g = makeGraph(graphSize)
            drawBox(g, graphSize)
            drawBoxDouble(g, outer)
            drawBox(g, inner)
            drawBoxBold(g, adjacent)
            g.asMonocolorString().mbox
          }
      }

      val actual = hcat(boxes).toString()
      val expectedOutput = {
        """|┌─────────────┐┌─────────────┐┌─────────────┐┌─────────────┐┌─────────────┐┌─────────────┐
           |│╔═════════╗░░││╔═════════╗░░││╔═════════╗░░││╔═════════╗░░││╔═════════╗░░││╔═════════╗░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░│
           |│┏━━━━━━┓░░║░░││┏━━━━━━━━━┓░░││║░┏━━━━━━━┓░░││┏━━━━━━┓░░║░░││║░┌────┏━━┓░░││║░┏━━━━━━━┓░░│
           |│┃░│░░░░┃░░║░░││┃░│░░░░│░░┃░░││║░┃░░░░│░░┃░░││┃░│░░░░┃░░║░░││║░│░░░░┃░░┃░░││║░┃░░░░│░░┃░░│
           |│┗━━━━━━┛░░║░░││┗━━━━━━━━━┛░░││║░┗━━━━━━━┛░░││┃░└────┃░░║░░││║░└────┃░░┃░░││║░┃────┘░░┃░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││┃░░░░░░┃░░║░░││║░░░░░░┃░░┃░░││║░┃░░░░░░░┃░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││┃░░░░░░┃░░║░░││║░░░░░░┃░░┃░░││║░┃░░░░░░░┃░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││┃░░░░░░┃░░║░░││║░░░░░░┃░░┃░░││║░┃░░░░░░░┃░░│
           |│║░░░░░░░░░║░░││║░░░░░░░░░║░░││║░░░░░░░░░║░░││┃░░░░░░┃░░║░░││║░░░░░░┃░░┃░░││║░┃░░░░░░░┃░░│
           |│╚═════════╝░░││╚═════════╝░░││╚═════════╝░░││┗━━━━━━┛══╝░░││╚══════┗━━┛░░││╚═┗━━━━━━━┛░░│
           |│░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░│
           |│░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░││░░░░░░░░░░░░░│
           |└─────────────┘└─────────────┘└─────────────┘└─────────────┘└─────────────┘└─────────────┘
           |""".stripMargin
      }

      assertExpectedText(expectedOutput, actual)
    }























    // inner.withinRegion(outer).adjacentRegion(Dir.Left)
    //   .foreach { adjacent =>
    //     val g = makeGraph(graphSize)
    //     drawBoxDouble(g, outer)
    //     drawBox(g, inner)
    //     println(g.asString)
    //     println()

    //     drawBoxBold(g, adjacent)
    //     println(g.asString)
    //     // assertResult(LTBounds.Ints(0, 30, 20, 10))(adj)
    //   }

    // inner.withinRegion(outer).adjacentRegion(Dir.Right)
    //   .foreach { adj =>
    //     assertResult(LTBounds.Ints(30, 30, 70, 10))(adj)
    //   }
    // inner.withinRegion(outer).adjacentRegion(Dir.Top)
    //   .foreach { adj =>
    //     assertResult(LTBounds.Ints(20, 0, 10, 30))(adj)
    //   }

    // inner.withinRegion(outer).adjacentRegion(Dir.Bottom)
    //   .foreach { adj =>
    //     assertResult(LTBounds.Ints(20, 40, 10, 60))(adj)
    //   }
  }
}

// {
//   // Left, Right, Left/Center, Right/Center, Left/Right =>
//   """|
//      |░░░░░░░░░░░░░░░░░░░░░░░░░░~░░░░░░░░░░░░░░░░░░░░░░░░░░~░░░░░░░░░░░░░░░░░░░░░░░░░░
//      |░░░░░░░░░░░░░░░░░░░░░░░░░░~░░░░░░░░░░░░░░░░░░░░░░░░░░~░░░░░░░░░░░░░░░░░░░░░░░░░░
//      |░░░╔═══════════════╗░░░░░░~░░░╔═══════════════╗░░░░░░~░░░╔═══════════════╗░░░░░░
//      |░░░║               ║░░░░░░~░░░║               ║░░░░░░~░░░║               ║░░░░░░
//      |░░░║               ║░░░░░░~░░░║               ║░░░░░░~░░░║               ║░░░░░░
//      |░░░║               ║░░░░░░~░░░║               ║░░░░░░~░░░║               ║░░░░░░
//      |░░░║   ┌──────┐    ║░░░░░░~░░░┌───┐           ║░░░░░░~░░░║          ┌────┐░░░░░░
//      |░░░║   │      │    ║░░░░░░~░░░│   │           ║░░░░░░~░░░║          │    │░░░░░░
//      |░░░║   └──────┘    ║░░░░░░~░░░└───┘           ║░░░░░░~░░░║          └────┘░░░░░░
//      |░░░║               ║░░░░░░~░░░║               ║░░░░░░~░░░║               ║░░░░░░
//      |░░░╚═══════════════╝░░░░░░~░░░╚═══════════════╝░░░░░░~░░░╚═══════════════╝░░░░░░
//      |░░░░░░░░░░░░░░░░░░░░░░░░░░~░░░░░░░░░░░░░░░░░░░░░░░░░░~░░░░░░░░░░░░░░░░░░░░░░░░░░
//      |""".stripMargin


//   """|
//      |░░░░░░░░░░░░░░░░░░░░░░░░░░
//      |░░░╔═══┌──────┐════╗░░░░░░
//      |░░░║   │      │    ║░░░░░░
//      |░░░║   │      │    ║░░░░░░
//      |░░░║   │      │    ║░░░░░░
//      |░░░║   └──────┘    ║░░░░░░
//      |░░░║               ║░░░░░░
//      |░░░║   ┌──────┐    ║░░░░░░
//      |░░░║   │      │    ║░░░░░░
//      |░░░╚═══└──────┘════╝░░░░░░
//      |░░░░░░░░░░░░░░░░░░░░░░░░░░
//      |""".stripMargin
// }
