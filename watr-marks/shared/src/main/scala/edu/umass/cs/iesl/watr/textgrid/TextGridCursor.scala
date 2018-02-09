package edu.umass.cs.iesl.watr
package textgrid

import scalaz.{@@ => _, _} , Scalaz._

import watrmarks._

object GridCursor {
  def init(z: Zipper[TextGrid.GridCell]) = new GridCursor {
    def zipper: Zipper[TextGrid.GridCell] = z
  }

  def init(optZ: Option[Zipper[TextGrid.GridCell]]): Option[GridCursor] =
    optZ.map{ z =>
      new GridCursor {
        def zipper: Zipper[TextGrid.GridCell] = z
      }
    }

  def init(cells: Seq[TextGrid.GridCell]): Option[GridCursor] =
    init(cells.toList.toZipper)

}

trait GridCursor { self =>
  def zipper: Zipper[TextGrid.GridCell]

  def focus: TextGrid.GridCell = zipper.focus

  def next: Option[GridCursor] =
    GridCursor.init{ zipper.next }

  def prev: Option[GridCursor] =
    GridCursor.init{ zipper.previous }

  def insertLeft(g: TextGrid.GridCell): GridCursor =
    GridCursor.init{ zipper.insertLeft(g)}

  def insertRight(g: TextGrid.GridCell): GridCursor =
    GridCursor.init{ zipper.insertRight(g)}


  def atStart: Boolean = zipper.atStart
  def atEnd: Boolean = zipper.atEnd

  def start: GridCursor = GridCursor.init{ zipper.start }
  def end: GridCursor = GridCursor.init{ zipper.end }

  def toWindow(): Window = {
    Window(Seq(focus), zipper.lefts, zipper.focus, zipper.rights)
  }

  def move(n: Int): Option[GridCursor] =
    GridCursor.init{ zipper.move(n) }

  def slurpRight(p: TextGrid.GridCell => Boolean): Window = {
    val (winTail, tail) = zipper.rights.span(p)
    val window = zipper.focus +: winTail
    Window(window, zipper.lefts, zipper.focus, tail)
  }

  def findNext(p: TextGrid.GridCell => Boolean): Option[GridCursor] = {
    GridCursor.init{ zipper.findNext(p) }
  }

  def findPrevious(p: TextGrid.GridCell => Boolean): Option[GridCursor] = {
    GridCursor.init{ zipper.findPrevious(p) }
  }

  def toRow: TextGrid.Row = {
    TextGrid.Row.fromCells(zipper.toList)
  }

  def addLabel(label: Label): Unit = {
    focus.addLabel(label)
  }

  import scala.annotation.tailrec

  @tailrec
  final def unfoldCursorToRow(f: GridCursor => Option[GridCursor]): TextGrid.Row  = {
    // def loop(f: GridCursor => Option[GridCursor]): TextGrid.Row  = {}
    f(self) match {
      case Some(cmod) => cmod.next match {
        case Some(cnext) => cnext.unfoldCursorToRow(f)
        case None => cmod.toRow

      }
      case None => self.toRow
    }
  }


  def insertCharLeft(c: Char): GridCursor =  insertLeft(focus.createInsert(c))
  // def insertCharRight(c: Char): GridCursor =  insertRight(focus.createRightInsert(c))
}




object Window {
  def apply(window: Seq[TextGrid.GridCell], lefts: Stream[TextGrid.GridCell], focus: TextGrid.GridCell, rights:Stream[TextGrid.GridCell]): Window = {
    new Window {
      def cells: Seq[TextGrid.GridCell] = window

      def winStart: Zipper[TextGrid.GridCell] =
        Zipper.zipper(lefts, focus, rights)
    }
  }
}



sealed trait Window { self =>
  def winStart: Zipper[TextGrid.GridCell]
  def cells: Seq[TextGrid.GridCell]

  def atStart: Boolean = winStart.atStart
  def atEnd: Boolean = winStart.atEnd

  def debugString(): String = {
    val rs = winStart.rights.map(_.char).mkString
    val ls = winStart.lefts.map(_.char).mkString.reverse
    val ws = cells.map(_.char).mkString
    s"""($ls [${ws}] $rs)"""

  }

  def closeWindow(): GridCursor = {
    val newLefts = cells.init.reverse.toStream ++ winStart.lefts
    val newFocus = cells.last

    GridCursor.init{
      Zipper.zipper(newLefts, newFocus, winStart.rights)
    }
  }

  def nextCursor(): Option[GridCursor] = {
    GridCursor.init{
      winStart.next.map{znext =>
        Zipper.zipper(
          cells.reverse.toStream ++ znext.lefts,
          znext.focus,
          znext.rights
        )
      }
    }
  }

  def removePins(label: Label): Unit = {
    cells.foreach { c =>
      c.removeLabel(label)
    }
  }

  def addLabel(label: Label): Unit = {
    if (cells.length==1) {
      cells.foreach(_.addPin(label.U))
    } else if (cells.length > 1) {
      cells.head.addPin(label.B)
      cells.last.addPin(label.L)
      cells.drop(1).dropRight(1).foreach(
        _.addPin(label.I)
      )
    }
  }

  def widen(n: Int): Window = {
    val (as, bs) = winStart.rights.splitAt(n)

    Window(cells++as, winStart.lefts, winStart.focus, bs)
  }

  // Reverse Seq.inits()
  def rinits[A](as: Stream[A]): Seq[Seq[A]]= {
    (1 to as.length) map { as.slice(0, _) }
  }

  /// HOTSPOT: reverse, toList, insertSpacesInRow
  def slurpRight(p: (Seq[TextGrid.GridCell], TextGrid.GridCell) => Boolean): Window = {

    // println(s"Checking winStart.right: ${winStart.rights.map(_.char).toList.mkString}")
    val slurped = rinits(winStart.rights)
      .map{ init =>
        // println(s"    init= '${init.map(_.char).toList.mkString}'")
        val nextWin = cells ++ init
        (nextWin.init, nextWin.last)
      }
      .takeWhile { case (h, t) => p(h, t) }

    val slcells = slurped.map{case (ws, t) => ws :+ t}.toList
      .lastOption
      .getOrElse { cells }

    Window(slcells, winStart.lefts, winStart.focus, winStart.rights.drop(slurped.length))

  }

  def extendRight(char: Char): Window = {
    val ins = winStart.focus.createInsert(char)
    Window(
      cells :+ ins,
      winStart.lefts, winStart.focus, winStart.rights
    )

  }
}
