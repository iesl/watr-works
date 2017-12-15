package edu.umass.cs.iesl.watr
package textgrid

import scalaz.{@@ => _, _} , Scalaz._

import watrmarks._
// import geometry._
// import geometry.PageComponentImplicits._


object GridCursor {
  def init(z: Zipper[TextGrid.GridCell]) = new GridCursor {
    def zipper: Zipper[TextGrid.GridCell] = z
  }

  def init(optZ: Option[Zipper[TextGrid.GridCell]]) = optZ.map{ z =>
    new GridCursor {
      def zipper: Zipper[TextGrid.GridCell] = z
    }
  }

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

  def unfoldCursorToRow(f: GridCursor => Option[GridCursor]): TextGrid.Row  = {
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

      def zipper: Zipper[TextGrid.GridCell] =
        Zipper.zipper(lefts, focus, rights)
    }
  }
}


sealed trait Window { self =>
  def zipper: Zipper[TextGrid.GridCell]
  def cells: Seq[TextGrid.GridCell]

  def atStart: Boolean = zipper.atStart
  def atEnd: Boolean = zipper.atEnd

  def debugString(): String = {
    val rs = zipper.rights.map(_.char).mkString
    val ls = zipper.lefts.map(_.char).mkString.reverse
    val ws = cells.map(_.char).mkString
    s"""($ls [${ws}] $rs)"""

  }

  def toLastCursor(): GridCursor = {
    val newLefts = cells.init.reverse.toStream ++ zipper.lefts
    val newFocus = cells.last

    GridCursor.init{
      Zipper.zipper(newLefts, newFocus, zipper.rights)
    }
  }

  def nextCursor(): Option[GridCursor] = {
    GridCursor.init{
      zipper.next.map{znext =>
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

  def slurpRight(p: (Seq[TextGrid.GridCell], TextGrid.GridCell) => Boolean): Window = {
    val slurped = zipper.rights
      .inits.toSeq.reverse.drop(1).map{ init =>
        val nextWin = cells ++ init
        (nextWin.init, nextWin.last)
      }
      .takeWhile { case (h, t) =>
        val b = p(h, t)
        // println(s"""  slurpRight(win=[${h.map(_.char).mkString}], cell=${t.char}) = ${b}""")
        b
      }

    val slcells = slurped.map{case (ws, t) => ws :+ t}.toList
      .lastOption
      .getOrElse { cells }

    Window(slcells, zipper.lefts, zipper.focus, zipper.rights.drop(slurped.length))

  }

  def extendRight(char: Char): Window = {
    val ins = zipper.focus.createInsert(char)
    Window(
      cells :+ ins,
      zipper.lefts, zipper.focus, zipper.rights
    )

  }
}
