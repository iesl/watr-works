package org.watrworks
package geometry

class RectangularCutTests extends WatrSpec {
  behavior of "Geometric Figures"

  import syntax._
  import GeometryTestUtils._
  import utils.{RelativeDirection => Dir}
  import textboxing.{TextBoxing => TB}, TB._
  import utils.ExactFloats._

  val graphSize = Rect.Ints(0, 0, 14, 14)

  it should "bug fix this example" in {
    //  runSearch: BottomLeft (l:0.00, t:55.83, w:187.73, h:738.17)
    //  focus: (l:187.73, t:55.82, w:0.01, h:0.01) horizon: (l:0.00, t:0.00, w:595.00, h:794.00)
    //
    //  TopRight: Some((l:187.74, t:0.00, w:407.26, h:55.82))
    //  Center: Some((l:187.73, t:55.82, w:0.01, h:0.01))
    //  Left: Some((l:0.00, t:55.82, w:187.73, h:0.01))
    //  BottomLeft: Some((l:0.00, t:55.83, w:187.73, h:738.17))
    //  Bottom: Some((l:187.73, t:55.83, w:0.01, h:738.17))
    //  Top: Some((l:187.73, t:0.00, w:0.01, h:55.82))
    //  BottomRight: Some((l:187.74, t:55.83, w:407.26, h:738.17))
    //  TopLeft: Some((l:0.00, t:0.00, w:187.73, h:55.82))
    //  Right: Some((l:187.74, t:55.82, w:407.26, h:0.01))
    //
    //
    //
    // @ A: within area=(l:0.00, t:55.83, w:187.73, h:738.17), focus=(l:187.73, t:55.82, w:0.01, h:0.01) from p=(187.73, 55.82)
    val focus        = Rect.IntReps(18773, 5582, 1, 1)
    val horizon      = Rect.IntReps(0, 0, 59500, 79400)
    val burstRegions = focus.withinRegion(horizon).burstAllPossibleDirections()
    val asdf         = s"""${burstRegions.map({ case (k, v) => s"$k: $v" }).mkString("  ", "\n  ", "")}"""
    println(asdf)

    // Bottom: Some((l:187.73, t:55.83, w:0.01, h:738.17))
    // Bottom: Some((l:187.73, t:55.83, w:0.01, h:738.17))
    // BottomLeft: Some((l:0.00, t:55.83, w:187.73, h:738.17))
    // BottomLeft: Some((l:0.00, t:55.83, w:187.73, h:738.17))
    // BottomRight: Some((l:187.74, t:55.83, w:407.26, h:738.17))
    // BottomRight: Some((l:187.74, t:55.83, w:407.26, h:738.17))
    // Center: Some((l:187.73, t:55.82, w:0.01, h:0.01))
    // Center: Some((l:187.73, t:55.82, w:0.01, h:0.01))
    // Left: Some((l:0.00, t:55.82, w:187.73, h:0.01))
    // Left: Some((l:0.00, t:55.82, w:187.73, h:0.01))
    // Right: Some((l:187.74, t:55.82, w:407.26, h:0.01))
    // Right: Some((l:187.74, t:55.82, w:407.26, h:0.01))
    // Top: Some((l:187.73, t:0.00, w:0.01, h:55.82))
    // Top: Some((l:187.73, t:0.00, w:0.01, h:55.82))
    // TopLeft: Some((l:0.00, t:0.00, w:187.73, h:55.82))
    // TopLeft: Some((l:0.00, t:0.00, w:187.73, h:55.82))
    // TopRight: Some((l:187.74, t:0.00, w:407.26, h:55.82))
    // TopRight: Some((l:187.74, t:0.00, w:407.26, h:55.82))

  }

  it should "burst overlapping regions into all contiguous rectangles" in {
    // Result is:
    //   Intersection ++
    //   val AllAdjacent: List[RelativeDirection] = List(
    //     Left        ,
    //     Right       ,
    //     Top         ,
    //     Bottom      ,
    //     TopLeft     ,
    //     TopRight    ,
    //     BottomLeft  ,
    //     BottomRight
    //   )

    List(
      (
        Rect.Ints(2, 2, 4, 1),
        Rect.Ints(2, 2, 4, 1), {
          """|┌────────────┐
             |│░░░░░░░░░░░░│
             |│░┣━━┫░░░░░░░│
             |│░░░░░░░░░░░░│
             |│░░░░░░░░░░░░│
             |│░░░░░░░░░░░░│
             |│░░░░░░░░░░░░│
             |│░░░░░░░░░░░░│
             |│░░░░░░░░░░░░│
             |│░░░░░░░░░░░░│
             |│░░░░░░░░░░░░│
             |│░░░░░░░░░░░░│
             |│░░░░░░░░░░░░│
             |└────────────┘
             |""".stripMargin
        }
      ),
      (
        Rect.Ints(1, 3, 4, 1),
        Rect.Ints(2, 2, 4, 4), {
          """|┌────────────┐┌────────────┐┌────────────┐┌────────────┐┌────────────┐┌────────────┐┌────────────┐
             |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░┌──┐░░░░░░░││░┌──┐░░░░░░░││░┣━┫┐░░░░░░░││░┌──┐░░░░░░░││░┌──◻░░░░░░░││░┌──┐░░░░░░░││░┌──┐░░░░░░░│
             |│╠┣━┫│░░░░░░░││◻│═╣│░░░░░░░││╠│═╣│░░░░░░░││╠│═╣│░░░░░░░││╠│═╣│░░░░░░░││╠│═╣◻░░░░░░░││╠│═╣│░░░░░░░│
             |│░│░░│░░░░░░░││░│░░│░░░░░░░││░│░░│░░░░░░░││░┏━┓│░░░░░░░││░│░░│░░░░░░░││░│░░│░░░░░░░││░│░░┳░░░░░░░│
             |│░└──┘░░░░░░░││░└──┘░░░░░░░││░└──┘░░░░░░░││░┗━┛┘░░░░░░░││░└──┘░░░░░░░││░└──┘░░░░░░░││░└──┻░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░││░░░░░░░░░░░░│
             |└────────────┘└────────────┘└────────────┘└────────────┘└────────────┘└────────────┘└────────────┘
             |""".stripMargin

        }
      ),
      (
        Rect.Ints(2, 2, 4, 1),
        Rect.Ints(2, 2, 6, 1), {
          """|┌────────────┐┌────────────┐
             |│░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░┣━━┫─┤░░░░░││░├───┣┫░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░│
             |│░░░░░░░░░░░░││░░░░░░░░░░░░│
             |└────────────┘└────────────┘
             |""".stripMargin

        }
      )
    ) foreach { case (bbox1, bbox2, expectedOutput @ _) =>
      // val burstRegions = bbox1.withinRegion(bbox2).burstAll()
      val burstRegions = bbox1.withinRegion(bbox2).burstAllPossibleDirections()

      def drawAdjacencyDiagram(adjacent: Rect): Box = {
        val g = makeGraph(graphSize)
        drawBoxDouble(g, bbox1)
        drawBox(g, bbox2)
        drawBoxBold(g, adjacent)
        g.asMonocolorString().box
      }

      val emptyRegions = burstRegions.filter(_._2.isEmpty).map { case (dir, _) =>
        s"${dir}".box
      }

      val emptyDirs = vjoins(TB.AlignLeft, "<Empty>".box +: emptyRegions)

      val adjs = burstRegions.filter(_._2.isDefined).map { case (dir, maybeRect) =>
        maybeRect.map(r => vjoin(TB.AlignCenter, s"${dir}", drawAdjacencyDiagram(r))).get
      }

      val actualOutput = hcat(TB.top, adjs ++ List(emptyDirs))
      println(actualOutput)
    // assertExpectedText(expectedOutput, actualOutput.toString())
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
