package org.watrworks
package geometry

import org.scalactic.TripleEqualsSupport

class RectangularCutTests extends WatrSpec {
  behavior of "Geometric Figures"

  import syntax._
  import GeometryTestUtils._
  import utils.{RelativeDirection => Dir}
  import textboxing.{TextBoxing => TB}, TB._
  import utils.ExactFloats._
  import ammonite.{ops => fs}

  val graphSize = Rect.Ints(0, 0, 14, 14)

  object images {
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

    def rectToDoodle(rect: Rect, title: Option[String]): MyPicture = {
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
  }

  val scratchDir = fs.pwd / "img-scratch.d"
  override protected def beforeAll(): Unit = {
    if (fs.exists(scratchDir)) {
      fs.rm(scratchDir)
    }
    fs.mkdir(scratchDir)
  }

  it should "svg redux burst overlapping regions into all contiguous rectangles" in {
    import doodle.core._
    import doodle.syntax._
    import images._
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
      val burstRegions = inner.withinRegion(outer).burstAllPossibleDirections()

      val inner1 = rectToDoodle(inner, Some("inner"))
        .fillColor(Color.rgba(100, 0, 0, 0.3))
        .strokeColor(Color.blue)
        .strokeWidth(2)

      val outer1 = rectToDoodle(outer, Some("outer"))
        .fillColor(Color.rgba(0, 0, 100, 0.1))
        .strokeColor(Color.black)
        .strokeWidth(3)

      val adjacents = for {
        (dir, maybeRect: Option[Rect]) <- burstRegions
        rect                           <- maybeRect
        // if dir != Dir.Center
      } yield {
        rectToDoodle(rect, Some(s"${dir.toString()}"))
          .fillColor(Color.rgba(0, 100, 0, 0.3))
          .strokeColor(Color.blue)
          .strokeWidth(1)
      }

      val composed = (inner1 :: outer1 :: adjacents.to(List)).allOn

      val pngPath = scratchDir / s"test-${testName}-${exampleNum}.png"
      composed.save(pngPath.toString())
    }
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
