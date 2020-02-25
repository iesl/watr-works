package edu.umass.cs.iesl.watr
package textboxing

import scalaz.syntax.ToIdOps
import scalaz.syntax.std.ToListOps

import scala.language.postfixOps
import scala.language.implicitConversions

import scala.scalajs.js.annotation._
import scala.annotation.tailrec
import scala.{ collection => sc }

sealed trait Alignment

object Alignment {
  case object AlignFirst extends Alignment
  case object AlignLast extends Alignment
  case object AlignCenter1 extends Alignment
  case object AlignCenter2 extends Alignment

}

sealed trait Content

object Content {
  import TextBoxing._
  case object Blank extends Content
  case class Text(s:String) extends Content
  case class Row(bs:Seq[Box]) extends Content
  case class Col(bs:Seq[Box]) extends Content
  case class SubBox(hAlign: Alignment, vAlign: Alignment, b:Box) extends Content
}



@JSExportTopLevel("TextBoxing") @JSExportAll
object TextBoxing extends ToListOps with ToIdOps {
  // Data type for specifying the alignment of boxes.
  import Alignment._
  import Content._


  val AlignLeft = AlignFirst
  val AlignRight = AlignLast
  val AlignCenter = AlignCenter1

  // Align boxes along their top/bottom/left/right
  def top = AlignFirst
  def bottom = AlignLast
  def left = AlignFirst
  def right = AlignLast

  // Align boxes centered, but biased to the left/top (center1) or
  //  right/bottom (center2) in the case of unequal parities.
  def center1    = AlignCenter1
  def center2    = AlignCenter2

  // pad box with an empty indentation box
  def indent(n:Int, b:Box): Box =
     emptyBox(1, n) + b

  def indent(b:Box): Box = indent(4, b)

  // Implicit to use bare string literals as boxes.
  implicit def stringToBox(s: String): Box = {
    linesToBox(scala.io.Source.fromString(s).getLines.toSeq)
  }

  // The basic data type.  A box has a specified size and some sort of contents.
  @JSExportTopLevel("Box") @JSExportAll
  case class Box(rows:Int, cols:Int, content: Content) {
    // Paste two boxes together horizontally, using a default (top) alignment.
    def +(r: Box) : Box = beside(r)
    def beside(r: Box) : Box =
      hcat(top, Seq(this,r))

    // Paste two boxes together horizontally with a single intervening
    //   column of space, using a default (top) alignment.
    def +|(r: Box) : Box = besideS(r)
    def besideS(r: Box) : Box =
      hcat(top, Seq(this, hspace(1), r))

    // Paste two boxes together vertically, using a default (left)
    //   alignment.
    def %(r: Box) : Box = atop(r)
    def atop(b: Box): Box = {
      vcat(left, Seq(this,b))
    }
    // Paste two boxes together vertically with a single intervening row
    //   of space, using a default (left) alignment.
    def %|(b: Box)  : Box = atopS(b)
    def atopS(b: Box): Box =
      vcat(left, Seq(this, vspace(1), b))

    override def toString = {
      // TODO: catch exceptions and fallback to safer rendering code
      render(this)
    }

    def lines:Seq[String] = renderBox(this)

    // def clipLeft(cols:Int): Box =
    def dropCols(cols:Int): Box =
      linesToBox(this.lines.map(_.drop(cols)))

    def takeCols(cols:Int): Box =
      linesToBox(this.lines.map(_.take(cols)))
  }


  implicit def charToBox(c: Char): Box = {
    tbox(c.toString)
  }

  implicit class CharToBox(val theChar: Char) extends AnyVal {
    def box: Box = charToBox(theChar)
  }

  implicit class BoxingConstructors(val theString: String) extends AnyVal {
    def box: Box = unrenderString(theString)

    def padCentered(width: Int, centerIndex: Int): String = {
      val trim = theString.trim()
      val padTo = width - trim.length()
      if (padTo > 0) {
        val (pre, /*post=*/_) = trim.splitAt(centerIndex)
        val prePadLen = (width/2) - pre.length
        val postPadLen = width - (prePadLen + trim.length)

        val prePad = " " * prePadLen
        val postPad = " " * postPadLen

        prePad+trim+postPad
      } else theString
    }

    def hangIndent(b: Box): Box = {
      theString.box atop indent(4, b)
    }
  }

  // Given a string, split it back into a box
  def unrenderString(s: String): Box =
    linesToBox(s.split("\n").toIndexedSeq)

  // Given a list of strings, create a box
  def linesToBox(lines: Seq[String]): Box =
    vjoins(lines.map(tbox(_)))


  case class RowSpec(
    alignment: Alignment,
    width: Int = 0
  )

  class Grid(
    val rowSpec: Seq[RowSpec],
    val rows: List[Row] = List()
  )

  object Grid {
    def aligned(aligns: Alignment*): Grid = {
      new Grid(aligns.map(a => RowSpec(a)))
    }

    def withCols(i: Int, align: Alignment): Grid = {
      aligned((0 until i).map(_ => align):_*)
    }

    // Min width per column
    def widthAligned(aligns: (Int, Alignment)*): Grid = {
      new Grid(aligns.map(a => RowSpec(a._2, a._1)))
    }

    implicit class RicherGrid(val theGrid: Grid) extends AnyVal {
      def addRow(bs: Box*): Grid = {
        new Grid(
          theGrid.rowSpec,
          Row(bs) :: theGrid.rows
        )
      }

      def toBox(): Box = {
        val allRows = theGrid.rows.reverse.map { row =>

          val rowCells = row.bs.zipAll(theGrid.rowSpec,
            nullBox, RowSpec(AlignLeft, 0)
          ).map {case (cellBox, spec) =>
            if (spec.width > 0) {
              Box(
                rows = cellBox.rows,
                cols = math.max(spec.width, cellBox.cols),
                SubBox(
                  hAlign = spec.alignment,
                  vAlign = AlignFirst,
                  cellBox
                )
              )
            } else cellBox

          }
          rowCells.toList
        }


        @tailrec
        def loop(bs: List[List[Box]], acc: List[List[Box]]): List[List[Box]] = {
          if (bs.exists(_.nonEmpty)) {
            val h = bs.map(_.headOption.getOrElse(nullBox))
            val ts = bs.map(_.tailOption.getOrElse(Nil))
            loop(ts, h :: acc)
          } else acc.reverse
        }
        val inv = loop(allRows, List[List[Box]]())

        hjoins(left,
          inv.map(vjoins(left, _))
        )
      }
    }
  }


  // The null box, which has no content and no size.
  def nullBox = emptyBox(0, 0)
  def hspace(n: Int) = emptyBox(0, n)
  def vspace(n: Int) = emptyBox(n, 0)

  // @emptyBox r c@ is an empty box with @r@ rows and @c@ columns.
  //   Useful for effecting more fine-grained positioning of other
  //   boxes, by inserting empty boxes of the desired size in between
  //   them.
  def emptyBox(r: Int, c: Int): Box =
    Box(r, c, Blank)

  // A @1x1@ box containing a single character.
  def char(c: Char): Box =
    Box(1, 1, Text(c.toString))

  // A (@1 x len@) box containing a string of length @len@.
  def tbox(s:String):  Box =
    Box(1, s.length, Text(s))


  // Glue a list of boxes together horizontally, with the given alignment.
  def hcat(align: Alignment, bs: Seq[Box]): Box = {
    def h = (0 +: (bs map (_.rows))).max
    def w = (bs map (_.cols)).sum
    Box(h, w, Row(bs.map(alignVert(align, h, _))))
  }

  def hjoin(a:Alignment, sep:Box)(bs:Box*): Box =
    hcat(a, bs.toList intersperse sep)


  def hjoin(bs:Box*): Box = hjoin(top, nullBox)(bs:_*)

  def hjoinWith(a:Alignment, sep:Box, bs:Seq[Box]): Box =
    hcat(a, bs.toList intersperse sep)

  def hjoins(a: Alignment, bs:Seq[Box]):Box = hjoinWith(a, nullBox, bs)

  // Glue a list of boxes together vertically, with the given alignment.
  def vcat(align: Alignment, bs: Seq[Box]): Box = {
    def h = (bs map (_.rows)).sum
    def w = (0 +: (bs map (_.cols))) max
    val aligned = (b:Box) => alignHoriz(align, w, b)

    Box(h, w, Col(bs map aligned))
  }


  def vjoinWith(a:Alignment, p:Box, bs:Seq[Box]):  Box =
    vcat(a, bs.toList intersperse p)

  def vjoin(a:Alignment, bs:Box*): Box = vjoinWith(a, nullBox, bs.toList)

  def vjoin(bs:Box*): Box = vjoinWith(left, nullBox, bs.toList)

  def vjoinTrailSep(a:Alignment, sep:Box, bs:Box*): Box = {
    val starts = bs.slice(0, bs.length-1)
    vcat(a,
      starts.map(_+sep) ++ bs.slice(bs.length-1, bs.length)
    )
  }

  def vjoins(a:Alignment, sep:Box, bs:Seq[Box]):Box =
    vcat(a, bs.toList intersperse sep)

  def vjoins(bs:Seq[Box]):Box = vjoins(left, nullBox, bs)
  def vjoins(a: Alignment, bs:Seq[Box]):Box = vjoins(a, nullBox, bs)


  def boxlf(b: Box): Box =
    vspace(1).atop(b)

  implicit class BoxOps(val theBox: Box) extends AnyVal {
    def padTop1 = boxlf(theBox)

    def alignRight(a:Alignment): Box = {
      hcat(right, Seq(theBox))
    }

    def width(w: Int): Box = {
      theBox
    }

    def transpose(): Box = {
      hcat(top,
        theBox.lines.map(l => vcat(left, l.toList.map(_.toString.box)))
      )

    }

    def hangIndent(b: Box): Box = {
      theBox atop indent(4, b)
    }

  }
  //------------------------------------------------------------------------------
  //  Paragraph flowing  ---------------------------------------------------------
  //------------------------------------------------------------------------------


  implicit class BoxSeqOps(val theBoxes: Seq[Box]) extends AnyVal {
    def mkHBox(separator: Box) =
      hjoinWith(top, separator, theBoxes)

    def mkVBox(separator: Box) =
      vjoinWith(left, separator, theBoxes)
  }

  //------------------------------------------------------------------------------
  //  Alignment  -----------------------------------------------------------------
  //------------------------------------------------------------------------------

  // alignHoriz algn n bx creates a box of width n, with the
  //   contents and height of bx, horizontally aligned according to
  //   algn.
  def alignHoriz(a:Alignment, cols:Int, b:Box): Box = {
    Box(b.rows, cols, SubBox(a, AlignFirst, b))
  }

  // // alignVert creates a box of height n, with the contents and width of bx, vertically aligned according to algn
  def alignVert(a:Alignment, r:Int, b:Box): Box =
      Box(r, (b.cols), SubBox(AlignFirst, a, b))


  // align ah av r c bx creates an r x c box with the contents
  //   of bx, aligned horizontally according to ah and vertically
  //   according to av.
  def align(ah: Alignment, av: Alignment, r: Int, c: Int, bx: Box): Box =
    Box(r, c, SubBox(ah, av, bx))

  // Move a box \"up\" by putting it in a larger box with extra rows,
  //   aligned to the top.  See the disclaimer for 'moveLeft'.
  def moveUp(n: Int, b: Box): Box =
    alignVert(top, b.rows + n, b)


  // Move a box down by putting it in a larger box with extra rows,
  //   aligned to the bottom.  See the disclaimer for 'moveLeft'.
  def moveDown (n:Int, b: Box): Box =
    alignVert(bottom, b.rows + n, b)

  // Move a box left by putting it in a larger box with extra columns,
  //   aligned left.  Note that the name of this function is
  //   something of a white lie, as this will only result in the box
  //   being moved left by the specified amount if it is already in a
  //   larger right-aligned context.
  def moveLeft(n:Int, b: Box): Box =
    alignHoriz(left, b.cols + n, b)


  // Move a box right by putting it in a larger box with extra
  //   columns, aligned right.  See the disclaimer for 'moveLeft'.
  def moveRight(n:Int, b: Box): Box =
    alignHoriz(right, b.cols + n, b)


  // Render a 'Box' as a String, suitable for writing to the screen or a file.
  def render(b: Box): String = renderBox(b) |> (_.mkString("\n"))

  // Generate a string of spaces.
  def blanks(n:Int): String =
    " " * n


  def merge(sss: Seq[Seq[String]]): Seq[String] = {
    (sss foldLeft Seq[String]()) { case (acc, ss) =>
        acc.zipAll(ss, "", "") map {case (s1, s2) => s1+s2}
    }
  }


  // Render a box as a list of lines.
  def renderBox(box: Box): Seq[String] = box match {
    case Box(r, c, Blank)             => resizeBox(r, c, List(""))
    case Box(r, c, Text(t))           => resizeBox(r, c, List(t))
    case Box(r, c, Col(bs))           => bs.flatMap(renderBoxWithCols(c, _)) |> (resizeBox(r, c, _))
    case Box(r, c, SubBox(ha, va, b)) => resizeBoxAligned(r, c, ha, va)(renderBox(b))
    case Box(r, c, Row(bs))           => {
      bs.map( renderBoxWithRows(r, _)) |> merge |> (resizeBox(r, c, _))
    }
  }

  // Render a box as a list of lines, using a given number of rows.
  def renderBoxWithRows(r:Int, b: Box): Seq[String] =
    renderBox (b.copy(rows = r))

  // Render a box as a list of lines, using a given number of columns.
  def renderBoxWithCols(c:Int, b: Box): Seq[String] =
    renderBox (b.copy(cols=c))

  // // Resize a rendered list of lines.
  def resizeBox(rows:Int, cols:Int, ss:Seq[String]): Seq[String] = {
    val takec: Seq[String] = ss map ({ s =>
      val slen = s.length

      if (slen == cols) s
      else if (slen > cols) s.substring(cols)
      else s + (" "*(cols-slen))
    })

    if (takec.length == rows) takec
    else if (takec.length > rows) takec.take(rows)
    else takec ++ ((1 to rows-takec.length).map(_ => " "*cols))
  }

  def resizeBoxAligned(r: Int, c: Int, ha: Alignment, va: Alignment): Seq[String] => Seq[String] = {
    ss => takePadAlignList(va, blanks(c), r, {
      ss.map (takePadAlignStr(ha, " ", c, _))
    })
  }

  def numFwd(a:Alignment, i:Int): Int = a match {
    case AlignFirst    => i
    case AlignLast     => 0
    case AlignCenter1  => i / 2
    case AlignCenter2  => (i+1) / 2
  }

  def numRev(a:Alignment, i:Int): Int = a match {
    case AlignFirst    => 0
    case AlignLast     => i
    case AlignCenter1  => (i+1) / 2
    case AlignCenter2  => i / 2
  }

  // takePadAlignXX  is like 'takePad', but with alignment.  That is, we
  //   imagine a copy of `xs` extended infinitely on both sides with
  //   copies of `a`, and a window of size `n` placed so that `xs` has
  //   the specified alignment within the window; `takePA algn a n xs`
  //   returns the contents of this window.
  def takePadAlignList(align:Alignment, pad:String, n:Int, xs: Seq[String]): Seq[String] = {
    val (a0, a1) = xs.splitAt(numRev(align, xs.length))

    val padRev = (0 until numRev(align, n)-a0.length).map(_ => pad)
    val padFwd = (0 until numFwd(align, n)-a1.length).map(_ => pad)

    padRev ++ a0 ++ a1 ++ padFwd
  }

  def takePadAlignStr(align:Alignment, pad:String, n:Int, xs: String): String = {
    val (a0, a1) = xs.splitAt(numRev(align, xs.length))

    val padRev = pad*(numRev(align, n) - a0.length)
    val padFwd = pad*(numFwd(align, n) - a1.length)

    padRev + a0 ++ a1 + padFwd
  }

  // take n copies from list, padding end with A if necessary
  def takePad[A](a:A, n:Int): Seq[A] => Seq[A] = { aas =>
    val pad = if (n <= aas.length) 0 else n - aas.length
    aas.take(n) ++ LazyList.continually(a).take(pad)
  }

  def repeat(b:Box): LazyList[Box] = {
    LazyList.continually(b)
  }

  private def borderLR(c:String)(b:Box): Box = {
    val b0 = vjoin( repeat(c).take(b.rows):_* )
    tbox("+") % b0 % tbox("+")
  }

  def borderLeft(c:String)(b:Box): Box = {
    val leftPad = vjoin( repeat(c).take(b.rows):_* )
    leftPad + b
  }


  def borderTB(c:String)(b:Box): Box = {
    hjoin( repeat(c).take(b.cols):_* )
  }

  def border(b:Box): Box = {
    val lr = borderLR("|")(b)
    val tb = borderTB("-")(b)

    lr + (tb % b % tb) + lr
  }
  def borderTop(b:Box): Box = {
    val tb = borderTB("-")(b)
    tb % b
  }
  def borderBottom(b:Box): Box = {
    val tb = borderTB("-")(b)
    b % tb
  }


  def padLine(lc:String, rc:String, fill:String, space:String)(l:String): String = {
    val lpad = l.takeWhile(_==' ')
    val rpad = l.reverse.takeWhile(_==' ')
    val left =  lc + fill*lpad.size + space
    val right = space + fill*rpad.size + rc
    left + l.trim + right
  }

  def borderInlineH(b:Box): Box = {
    if (b.rows==0) b
    else if (b.rows==1)
      tbox("[") + b + tbox("]")
    else {
      val lines:Seq[String] = renderBox(b)

      linesToBox(
        padLine("┌", "┐", "─", " ")(lines.head) +:
          (lines.drop(1).take(lines.length-2).map(
            str => "│ "+str+" │"
          ) ++ List(padLine("└", "┘", "─", " ")(lines.last))))
    }
  }

  def borderLeftRight(l:String, r:String)(b:Box): Box = {
    linesToBox(
      renderBox(b).map(l+_+r)
    )
  }

  def borderInlineTop(b:Box): Box = {
    if (b.rows==0) b
    else if (b.rows==1)
      tbox("[") + b + tbox("]")
    else {
      val lines:Seq[String] = renderBox(b)

      linesToBox(
        padLine("┌", "┐", "─", " ")(lines.head) +:
          (lines.drop(1).take(lines.length-1).map(
            str => "│ "+str+" │"
          ) ++ List(padLine("└", "┘", "─", "─")("─"*lines.last.length))))
    }
  }



  object OneRow {
    // functions only make sense if the Box is a single row
    def bracket(l:Char, r:Char, b: Box): Box = {
      val lb = l.toString.box
      val rb = r.toString.box
      lb + b + rb
    }

    def dquote(b: Box): Box = bracket('"', '"', b)
    def squareBracket(b: Box): Box = bracket('[', ']', b)
    def curlyBrace(b: Box): Box = bracket('{', '}', b)

  }


  object SemanticLayout {

    sealed trait ParaContent

    case class Para(paraWidth : Int, paraContent : ParaContent)
    case class Block(fullLines : Seq[Line], lastLine  : Line) extends ParaContent
    case class Line(len: Int, words: Seq[Word])
    case class Word(len:Int, word:String)


    def unwords(ws:Seq[String]) = ws.mkString(" ")

    // para algn w t is a box of width w, containing text t,
    //   aligned according to algn, flowed to fit within the given
    //   width.
    def para(a:Alignment, n:Int, t:String): Box = {
      flow(n, t) |> (ss => mkParaBox(a, ss.length, ss))
    }


    // columns w h t is a list of boxes, each of width w and height
    //   at most h, containing text t flowed into as many columns as
    //   necessary.
    def columns(a:Alignment, w:Int, h:Int, t:String):Seq[Box] =
      flow(w, t) map (_.grouped(h)) map (ss => mkParaBox(a, h, ss.toSeq))


    // makes a box of height n with the text ss
    //   aligned according to a
    def mkParaBox(a:Alignment, n:Int, ss:Seq[String]): Box =
      alignVert(top, n, vcat(a, ss.map(stringToBox(_))))


    def words(s:String): Seq[String] = {
      val wordSplit = """\s+""".r
      (for {
        l <- scala.io.Source.fromString(s).getLines.toSeq
        w <- wordSplit.split(l)
      } yield {
        w.trim
      })
    }


    // Flow the given text into the given width.
    def flow(n:Int, t:String): Seq[String] = {
      val wrds = words(t) map mkWord
      val para = wrds.foldLeft (emptyPara(n)) { case(acc, e) => addWordP(acc, e) }
      para |> getLines |> (_.map(_.take(n)))
    }


    // val paraWidth: Lens[Para, Int] = lensu((obj, v) => obj copy (paraWidth = v), _.paraWidth)
    // val paraContent: Lens[Para, ParaContent] = lensu((obj, v) => obj copy (paraContent = v), _.paraContent)

    // val fullLines: Lens[Block, Seq[Line]] = lensu((obj, v) => obj copy (fullLines = v), _.fullLines)
    // val lastLine: Lens[Block, Line] = lensu((obj, v) => obj copy (lastLine = v), _.lastLine)

    def emptyPara(pw: Int) : Para =
      Para(pw, (Block(Nil, (Line(0, Nil)))))

    def getLines(p:Para): Seq[String] = {
      // def process =  (l:Seq[Line]) => l.reverse map Line.getWords map (_.map(Word.getWord)) map (_.reverse) map unwords
      // def process =  (l:Seq[Line]) => l.reverse.map(_.words).map(_.map(_.word)) map (_.reverse) map unwords
      def process(l:Seq[Line]): Seq[String] = {
        l.reverse.map(_.words).map(_.map(_.word)) map (_.reverse) map unwords
      }

      p match {
        case Para(_, (Block(ls, l))) =>
          if (l.len == 0) process(ls)
          else            process(l+:ls)
      }
    }


    def mkLine(ws: Seq[Word]):  Line =
      Line((ws map (_.len)).sum + ws.length - 1, ws)

    def startLine(w: Word) : Line =
      mkLine(w :: Nil)



    def mkWord(w:String): Word = Word(w.length, w)

    def addWordP(p:Para, w: Word ): Para = {
      p match {
        case Para(pw, (Block(fl,l))) =>
          if (wordFits(pw,w,l))
            Para(pw, Block(fl, addWordL(w, l)))
          else
            Para(pw, Block((l+:fl), startLine(w)))
      }
    }


  def addWordL(w:Word, l:Line) : Line = l match {
    case Line(len, ws) => Line((len + w.len + 1), (w+:ws))
  }


  def wordFits(pw:Int, w:Word, l:Line): Boolean =
    l.len == 0 || l.len + w.len + 1 <= pw

  }

}


// object App extends App {
//   import TextBoxing._

//   def multiLineStringToBox(): Unit = {

//     val animationStyle = {
//       """|<svg:style>
//          |  .path {
//          |    stroke-dasharray: 1000;
//          |    stroke-dashoffset: 1000;
//          |    animation: dash 5s linear forwards;
//          |  }
//          |
//          |  @keyframes dash {
//          |    to {
//          |      stroke-dashoffset: 0;
//          |    }
//          |  }
//          |</svg:style>
//          | """.stripMargin.box
//     }

//     println(animationStyle.toString())
//     println(borderInlineTop(
//       "Inline-header top header" atop animationStyle
//     ))
//   }
//   // multiLineStringToBox()


//   def sampleText1 = vjoin(center2, nullBox)(
//     tbox("Lorem ipsum dolor sit amet"),
//     tbox("Anyconsectetur adipisicing elit, sed do eiusmod tempor"),
//     tbox("incididunt ut labore et dolore magna "),
//     tbox("aliqua. Ut enim ad minim veniam, ")
//   )

//   println(sampleText1.toString())
//   def sampleText2 = vjoin(center1, nullBox)(
//     tbox("Lorem ipsum dolor sit amet"),
//     tbox("Anyconsectetur adipisicing elit, sed do eiusmod tempor"),
//     tbox("aliqua. Ut enim ad minim veniam, "),
//     tbox("incididunt ut labore et dolore magna "),
//     tbox("aliqua. Ut enim ad minim veniam, ")
//   )

//   def rawText = """|Lorem ipsum dolor sit amet
//                    |Anyconsectetur adipisicing elit, sed do eiusmod tempor
//                    |aliqua. Ut enim ad minim veniam,
//                    |incididunt ut labore et dolore magna
//                    |aliqua. Ut enim ad minim veniam
//                    |""".stripMargin


//   def sampleText3 = vjoin(
//     tbox("Lorem ipsum dolor sit amet")
//   )

//   def sampleBox1 = hjoin(right, nullBox)( sampleText1, "  <-|||->  ", sampleText2)


//   def sampleBox2 = vjoin(center1, nullBox)(hjoin(center1, sampleText1, "  <-|||->  ", sampleText2), sampleText3)

//   def sampleBox3 = vjoin(right, nullBox)(sampleText1, "  <-|||-> ", sampleText2)


//   val flowed = para(left)(20)(rawText)
//   println(borderInlineH(
//     "Inline-header label" atop flowed
//   ))

//   println("\n\n")

//   println(borderInlineTop(
//     "Inline-header top header" atop sampleBox1
//   ))

//   println("\n\n")

//   println(border(
//     "simple border" atop sampleBox2
//   ))


//   println("\n\n")

//   println(borderLeftRight("--> ", " <--")(
//     "Left/right border" atop sampleBox3
//   ))

//   println("\n\n")

//   println("Matrices")



// }
