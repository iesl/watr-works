package org.watrworks
package geometry

class RectangularCutTests extends WatrSpec {
  behavior of "Geometric Figures"

  import syntax._
  import GeometryTestUtils._
  import utils.{RelativeDirection => Dir}
  import textboxing.{TextBoxing => TB}, TB._
  import utils.ExactFloats._
  import ammonite.{ops => fs}

  import pngDrawing._
  import doodle.core._
  import doodle.syntax._

  val graphSize = Rect.Ints(0, 0, 14, 14)

  val scratchDir = fs.pwd / "img-scratch.d"
  override protected def beforeAll(): Unit = {
    if (fs.exists(scratchDir)) {
      fs.rm(scratchDir)
    }
    fs.mkdir(scratchDir)
  }

  it should "find min/max separating rects between 2 rects" in {
    val testName = "min-max-sep"
    val centerRect = Rect.Doubles(100, 100, 100, 100)
    val others = List(
      // Left of
      Rect.Doubles(10, 80, 40, 40),
      Rect.Doubles(10, 140, 40, 40),
      Rect.Doubles(10, 180, 40, 40),

      // Above
      Rect.Doubles(80, 10, 40, 40),
      Rect.Doubles(140, 10, 40, 40),
      Rect.Doubles(180, 10, 40, 40),

      // Right
      Rect.Doubles(210, 80, 40, 40),
      Rect.Doubles(210, 140, 40, 40),
      Rect.Doubles(210, 180, 40, 40),

      // Below
      Rect.Doubles(80, 210, 40, 40),
      Rect.Doubles(140, 210, 40, 40),
      Rect.Doubles(180, 210, 40, 40),

      // No Intersection
      Rect.Doubles(10, 10, 40, 40),

    )
      val ctr = drawRect(centerRect, Some("Focus"))
        .fillColor(Color.rgba(0, 0, 128, 0.3))
        .strokeColor(Color.black)
        .strokeWidth(1)

    for {
      (other, exampleNum) <- others.zipWithIndex
      (sepRect, sepDir)   <- centerRect.minSeparatingRect(other)
    } yield {

      val oth = drawRect(other, Some("Other"))
        .fillColor(Color.rgba(128, 0, 0, 0.3))
        .strokeColor(Color.black)
        .strokeWidth(1)

      val sep = drawRect(sepRect, Some(sepDir.toString()))
        .fillColor(Color.rgba(0, 128, 0, 0.3))
        .strokeColor(Color.red)
        .strokeWidth(2)

      val pngPath   = scratchDir / s"test-${testName}-${exampleNum}.png"
      val composed = List(ctr, oth, sep).allOn
      composed.save(pngPath.toString())
    }

  }

  it should "burst overlapping regions into all contiguous rectangles" in {

    val testName = "burst-overlapping"

    List(
      (
        Rect.Doubles(100, 100, 400, 400),
        Rect.Doubles(150, 150, 100, 200)
      ),
      (
        Rect.Doubles(100, 100, 400, 400),
        Rect.Doubles(100, 100, 400, 100)
      ),
      (
        Rect.Doubles(100, 100, 400, 400),
        Rect.Doubles(100, 80, 400, 100)
      ),
      (
        Rect.Doubles(100, 100, 400, 400),
        Rect.Doubles(100, 120, 400, 100)
      )
    ).zipWithIndex.foreach { case ((outer, inner), exampleNum) =>
      val octoImage = drawOctothorpe(outer, inner)
      val pngPath   = scratchDir / s"test-${testName}-${exampleNum}.png"
      octoImage.save(pngPath.toString())
    }
  }

  it should "split rectangles vertically and horizontally" in {
    val rect = Rect.Ints(1, 1, 10, 10)

    {
      val (lsplit, rsplit) = rect.splitHorizontal(10.toFloatExact())
      println("Horizontal Split")
      println(s" l: ${lsplit}")
      println(s" r: ${rsplit}")
    }
    {
      val vsplit           = 20.toFloatExact()
      val (lsplit, rsplit) = rect.splitVertical(vsplit)
      println(s"Vertical Split ${rect} at ${vsplit}")
      println(s" l: ${lsplit}")
      println(s" r: ${rsplit}")
    }

  }
  it should "find  adjacent regions when inner rect is not strictly within outer" in {
    val graphSize = Rect.Ints(0, 0, 14, 14)
    val outer     = Rect.Ints(3, 3, 6, 6)
    val inner     = Rect.Ints(2, 2, 4, 4)

    def drawAdjacencyDiagram(adjacent: Rect): Box = {
      val g = makeGraph(graphSize)
      drawBoxDouble(g, outer)
      drawBox(g, inner)
      drawBoxBold(g, adjacent)
      g.asMonocolorString().box
    }

    {
      val boxes = List(Dir.Left, Dir.Center, Dir.Right, Dir.Top, Dir.Bottom)
        .map { dir =>
          (dir, inner.withinRegion(outer).adjacentRegion(dir))
        }
        .map { case (d @ _, maybAdjacent) =>
          maybAdjacent
            .map { adjacent =>
              drawAdjacencyDiagram(adjacent)
            }
            .getOrElse { "  <empty>  ".box }
        }

      val expectedOutput = {
        """|  <empty>  ┌────────────┐┌────────────┐  <empty>  ┌────────────┐
           |           │░░░░░░░░░░░░││░░░░░░░░░░░░│           │░░░░░░░░░░░░│
           |           │░┌──┐░░░░░░░││░┌──┐░░░░░░░│           │░┌──┐░░░░░░░│
           |           │░│┏━┓══╗░░░░││░│╔═│┏━┓░░░░│           │░│╔═│══╗░░░░│
           |           │░│┃░┃░░║░░░░││░│║░│┃░┃░░░░│           │░│║░│░░║░░░░│
           |           │░└┗━┛░░║░░░░││░└──┘┗━┛░░░░│           │░└──┘░░║░░░░│
           |           │░░║░░░░║░░░░││░░║░░░░║░░░░│           │░░┏━┓░░║░░░░│
           |           │░░║░░░░║░░░░││░░║░░░░║░░░░│           │░░┃░┃░░║░░░░│
           |           │░░╚════╝░░░░││░░╚════╝░░░░│           │░░┗━┛══╝░░░░│
           |           │░░░░░░░░░░░░││░░░░░░░░░░░░│           │░░░░░░░░░░░░│
           |           │░░░░░░░░░░░░││░░░░░░░░░░░░│           │░░░░░░░░░░░░│
           |           │░░░░░░░░░░░░││░░░░░░░░░░░░│           │░░░░░░░░░░░░│
           |           │░░░░░░░░░░░░││░░░░░░░░░░░░│           │░░░░░░░░░░░░│
           |           └────────────┘└────────────┘           └────────────┘
           |""".stripMargin
      }
      val actualOutput = hcat(top, boxes).toString()
      // println(actualOutput)
      assertExpectedText(expectedOutput, actualOutput)
    }
    {
      val boxes = List(
        List(Dir.Left, Dir.Center),
        List(Dir.Left, Dir.Right),
        List(Dir.Right, Dir.Center),
        List(Dir.Left, Dir.Bottom),
        List(Dir.Right, Dir.BottomRight),
        List(Dir.Right, Dir.Bottom)
      ).flatMap { dirs =>
        inner
          .withinRegion(outer)
          .adjacentRegions(dirs: _*)
          .map { adjacent => drawAdjacencyDiagram(adjacent) }
      }

      val expectedOutput = {
        """|┌────────────┐┌────────────┐┌────────────┐┌────────────┐┌────────────┐┌────────────┐
           |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
           |│░┌──┐░░░░░░░││░┌──┐░░░░░░░││░┌──┐░░░░░░░││░┌──┐░░░░░░░││░┌──┐░░░░░░░││░┌──┐░░░░░░░│
           |│░│┏━┓══╗░░░░││░│╔═│┏━┓░░░░││░│┏━━━━┓░░░░││░│╔═│══╗░░░░││░│╔═│┏━┓░░░░││░│┏━━━━┓░░░░│
           |│░│┃░┃░░║░░░░││░│║░│┃░┃░░░░││░│┃░│░░┃░░░░││░│║░│░░║░░░░││░│║░│┃░┃░░░░││░│┃░│░░┃░░░░│
           |│░└┗━┛░░║░░░░││░└──┘┗━┛░░░░││░└┗━━━━┛░░░░││░└──┘░░║░░░░││░└──┘┃░┃░░░░││░└┃─┘░░┃░░░░│
           |│░░║░░░░║░░░░││░░║░░░░║░░░░││░░║░░░░║░░░░││░░┏━┓░░║░░░░││░░║░░┃░┃░░░░││░░┃░░░░┃░░░░│
           |│░░║░░░░║░░░░││░░║░░░░║░░░░││░░║░░░░║░░░░││░░┃░┃░░║░░░░││░░║░░┃░┃░░░░││░░┃░░░░┃░░░░│
           |│░░╚════╝░░░░││░░╚════╝░░░░││░░╚════╝░░░░││░░┗━┛══╝░░░░││░░╚══┗━┛░░░░││░░┗━━━━┛░░░░│
           |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
           |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
           |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
           |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
           |└────────────┘└────────────┘└────────────┘└────────────┘└────────────┘└────────────┘
           |""".stripMargin
      }

      val actualOutput = hcat(top, boxes).toString()
      // println(actualOutput)
      assertExpectedText(expectedOutput, actualOutput)
    }
  }

  it should "find regions adjacent to rectangle within enclosing region" in {
    val graphSize = Rect.Ints(0, 0, 14, 14)
    val outer     = Rect.Ints(1, 1, 10, 10)
    val inner     = Rect.Ints(3, 4, 5, 2)

    def drawAdjacencyDiagram(adjacent: Rect): Box = {
      // println(s"drawAdjacencyDiagram")
      // println(s"   graph: ${graphSize}")
      // println(s"   inner: ${inner}")
      // println(s"   outer: ${outer}")
      // println(s"   adjacent: ${adjacent}")
      val g = makeGraph(graphSize)
      drawBoxDouble(g, outer)
      drawBox(g, inner)
      drawBoxBold(g, adjacent)
      g.asMonocolorString().box
    }

    {
      val boxes = List(Dir.Left, Dir.Center, Dir.Right, Dir.Top, Dir.Bottom)
        .flatMap { dir =>
          inner
            .withinRegion(outer)
            .adjacentRegion(dir)
            .map { adjacent => drawAdjacencyDiagram(adjacent) }
        }

      val expectedOutput = {
        """|┌────────────┐┌────────────┐┌────────────┐┌────────────┐┌────────────┐
           |│╔════════╗░░││╔════════╗░░││╔════════╗░░││╔═┏━━━┓══╗░░││╔════════╗░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░┃░░░┃░░║░░││║░░░░░░░░║░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░┗━━━┛░░║░░││║░░░░░░░░║░░│
           |│┏┓┌───┐░░║░░││║░┏━━━┓░░║░░││║░┌───┐┏━┓░░││║░┌───┐░░║░░││║░┌───┐░░║░░│
           |│┗┛└───┘░░║░░││║░┗━━━┛░░║░░││║░└───┘┗━┛░░││║░└───┘░░║░░││║░└───┘░░║░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░┏━━━┓░░║░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░┃░░░┃░░║░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░┃░░░┃░░║░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░┃░░░┃░░║░░│
           |│╚════════╝░░││╚════════╝░░││╚════════╝░░││╚════════╝░░││╚═┗━━━┛══╝░░│
           |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
           |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
           |└────────────┘└────────────┘└────────────┘└────────────┘└────────────┘
           |""".stripMargin
      }

      val actualOutput = hcat(top, boxes).toString()
      // println(actualOutput)
      assertExpectedText(expectedOutput, actualOutput)
    }

    {
      val boxes = List(Dir.TopLeft, Dir.TopRight, Dir.BottomLeft, Dir.BottomRight)
        .flatMap { dir =>
          inner
            .withinRegion(outer)
            .adjacentRegion(dir)
            .map { adjacent => drawAdjacencyDiagram(adjacent) }
        }

      val expectedOutput = {
        """|┌────────────┐┌────────────┐┌────────────┐┌────────────┐
           |│┏┓═══════╗░░││╔══════┏━┓░░││╔════════╗░░││╔════════╗░░│
           |│┃┃░░░░░░░║░░││║░░░░░░┃░┃░░││║░░░░░░░░║░░││║░░░░░░░░║░░│
           |│┗┛░░░░░░░║░░││║░░░░░░┗━┛░░││║░░░░░░░░║░░││║░░░░░░░░║░░│
           |│║░┌───┐░░║░░││║░┌───┐░░║░░││║░┌───┐░░║░░││║░┌───┐░░║░░│
           |│║░└───┘░░║░░││║░└───┘░░║░░││║░└───┘░░║░░││║░└───┘░░║░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││┏┓░░░░░░░║░░││║░░░░░░┏━┓░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││┃┃░░░░░░░║░░││║░░░░░░┃░┃░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││┃┃░░░░░░░║░░││║░░░░░░┃░┃░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││┃┃░░░░░░░║░░││║░░░░░░┃░┃░░│
           |│╚════════╝░░││╚════════╝░░││┗┛═══════╝░░││╚══════┗━┛░░│
           |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
           |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
           |└────────────┘└────────────┘└────────────┘└────────────┘
           |""".stripMargin
      }

      val actualOutput = hcat(top, boxes).toString()
      // println(actualOutput)
      assertExpectedText(expectedOutput, actualOutput)
    }

    {
      val boxes = List(
        List(Dir.Left, Dir.Center),
        List(Dir.Left, Dir.Right),
        List(Dir.Right, Dir.Center),
        List(Dir.Left, Dir.Bottom),
        List(Dir.Right, Dir.BottomRight),
        List(Dir.Right, Dir.Bottom)
      ).flatMap { dirs =>
        inner
          .withinRegion(outer)
          .adjacentRegions(dirs: _*)
          .map { adjacent => drawAdjacencyDiagram(adjacent) }
      }

      val expectedOutput = {
        """|┌────────────┐┌────────────┐┌────────────┐┌────────────┐┌────────────┐┌────────────┐
           |│╔════════╗░░││╔════════╗░░││╔════════╗░░││╔════════╗░░││╔════════╗░░││╔════════╗░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░│
           |│┏━━━━━┓░░║░░││┏━━━━━━━━┓░░││║░┏━━━━━━┓░░││┏━━━━━┓░░║░░││║░┌───┐┏━┓░░││║░┏━━━━━━┓░░│
           |│┗━━━━━┛░░║░░││┗━━━━━━━━┛░░││║░┗━━━━━━┛░░││┃░└───┃░░║░░││║░└───┘┃░┃░░││║░┃───┘░░┃░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││┃░░░░░┃░░║░░││║░░░░░░┃░┃░░││║░┃░░░░░░┃░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││┃░░░░░┃░░║░░││║░░░░░░┃░┃░░││║░┃░░░░░░┃░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││┃░░░░░┃░░║░░││║░░░░░░┃░┃░░││║░┃░░░░░░┃░░│
           |│║░░░░░░░░║░░││║░░░░░░░░║░░││║░░░░░░░░║░░││┃░░░░░┃░░║░░││║░░░░░░┃░┃░░││║░┃░░░░░░┃░░│
           |│╚════════╝░░││╚════════╝░░││╚════════╝░░││┗━━━━━┛══╝░░││╚══════┗━┛░░││╚═┗━━━━━━┛░░│
           |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
           |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
           |└────────────┘└────────────┘└────────────┘└────────────┘└────────────┘└────────────┘
           |""".stripMargin
      }

      val actualOutput = hcat(top, boxes).toString()
      // println(actualOutput)
      assertExpectedText(expectedOutput, actualOutput)
    }
  }
}
