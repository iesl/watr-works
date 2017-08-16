package edu.umass.cs.iesl.watr
package textgrid

import scala.collection.mutable
import watrmarks._
import geometry._
import scalaz.{@@ => _, _} , Scalaz._
// import utils.SlicingAndDicing._

sealed trait FontInfo

case object NoFonts extends FontInfo

object TextGrid {
  type SetType[A] = mutable.Set[A]
  type PinSet = SetType[BioPin]


  sealed trait GridCell {
    def pageRegion: PageRegion

    def char: Char

    val pins: PinSet = mutable.HashSet[BioPin]()

    def fonts: FontInfo = NoFonts

    def labels: SetType[Label] = pins.map(_.label)

    def addPin(p: BioPin): Unit = pins.add(p)

    def addLabel(l: Label): Unit = addPin(l.U)

    def removeLabel(l: Label): Unit = {
      pins.retain(_.label != l)
    }

    def createLeftInsert(ch: Char): InsertCell = LeftInsertCell(ch, this)
    def createRightInsert(ch: Char): InsertCell = RightInsertCell(ch, this)

    def hasLabel(l: Label): Boolean = {
      pins.exists(_.label == l)
    }

    def hasPin(p: BioPin): Boolean = {
      pins.contains(p)
    }

  }

  case class PageItemCell(
    headItem: PageItem,
    tailItems: Seq[PageItem] = Seq(),
    override val char: Char,
    regionFunc: (PageItem, Seq[PageItem]) => PageRegion = (h, _) => h.pageRegion
  ) extends GridCell {

    override val pageRegion: PageRegion = regionFunc(headItem, tailItems)

    def createLeftExpansion(ch: Char): ExpansionCell = LeftExpansionCell(ch, this)
    def createRightExpansion(ch: Char): ExpansionCell = RightExpansionCell(ch, this)
  }

  abstract class ExpansionCell(
    char: Char,
    root: PageItemCell
  ) extends GridCell {
    override val pageRegion: PageRegion = root.pageRegion
    // TODO override addPin() = root.addPin()
  }

  case class LeftExpansionCell(
    override val char: Char,
    root: PageItemCell
  ) extends ExpansionCell(char, root)

  case class RightExpansionCell(
    override val char: Char,
    root: PageItemCell
  ) extends ExpansionCell(char, root)


  abstract class InsertCell(
    char: Char,
    root: GridCell
  ) extends GridCell {
    override val pageRegion: PageRegion = root.pageRegion
    // TODO override addPin() = root.addPin()
  }

  case class LeftInsertCell(
    override val char: Char,
    root: GridCell
  ) extends InsertCell(char, root)

  case class RightInsertCell(
    override val char: Char,
    root: GridCell
  ) extends InsertCell(char, root)


  trait Row {
    def cells: Seq[GridCell]

    def toCursor(): Option[Cursor] = {
      ZipCursor.init(cells.toList.toZipper)
    }

    def foreach(f: GridCell => Unit): Unit  = {
      cells.foreach(f(_))
    }


    def toText(): String = {
      cells.map(_.char).mkString("")
    }


    def serialize(props: SerializationProps): Int = {

      val lineCells = cells.map{ _ match {
        case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>
          val items = (headItem +: tailItems).map{ pageItem =>
            val page = pageItem.pageRegion.page
            if (!props.pageIdMap.contains(page.pageId)) {
              props.pageIdMap.put(page.pageId, (page.stableId, page.pageNum))
            }
            val LTBounds.IntReps(l, t, w, h) = pageItem.pageRegion.bbox
            s"""[${page.pageId}, [$l, $t, $w, $h]]"""
          }

          items.mkString("[", ",", "]")
        case cell@ TextGrid.LeftExpansionCell(char, root)  => '"'+"el"+'"'
        case cell@ TextGrid.RightExpansionCell(char, root) => '"'+"er"+'"'
        case cell@ TextGrid.LeftInsertCell(char, root)     => '"'+"il"+'"'
        case cell@ TextGrid.RightInsertCell(char, root)    => '"'+"ir"+'"'
      }}

      val lineDefStr = lineCells.mkString(",")
      val lineDef = s"""[${lineDefStr}]"""
      val lineNum = props.nextLineNum
      props.lineMap.put(lineNum, (lineDef, toText))
      lineNum
    }

  }

  class SerializationProps {
    val pageIdMap = mutable.Map[Int@@PageID, (String@@DocumentID, Int@@PageNum)]()
    val lineMap = mutable.Map[Int, (String, String)]()

    def nextLineNum: Int = if (lineMap.keySet.isEmpty) 0 else lineMap.keySet.max + 1
  }

  // trait Grid {}

  abstract class MutableRow extends Row {
    override val cells: mutable.ArrayBuffer[GridCell] = mutable.ArrayBuffer()
  }


  object Row {
    def fromCells(init: Seq[GridCell]): Row = new MutableRow {
      cells.appendAll(init)
    }


  }


  object Window {
    def apply(window: Seq[GridCell], lefts: Stream[GridCell], focus: GridCell, rights:Stream[GridCell]): Window = {
      new Window {
        def cells: Seq[GridCell] = window

        def zipper: Zipper[GridCell] =
          Zipper.zipper(lefts, focus, rights)
      }
    }
  }


  sealed trait Window { self =>
    def zipper: Zipper[GridCell]
    def cells: Seq[GridCell]

    def atStart: Boolean = zipper.atStart
    def atEnd: Boolean = zipper.atEnd

    def debugString(): String = {
      val rs = zipper.rights.map(_.char).mkString
      val ls = zipper.lefts.map(_.char).mkString.reverse
      val ws = cells.map(_.char).mkString
      s"""($ls [${ws}] $rs)"""

    }

    def toLastCursor(): Cursor = {
      val newLefts = cells.init.reverse.toStream ++ zipper.lefts
      val newFocus = cells.last

      ZipCursor.init{
        Zipper.zipper(newLefts, newFocus, zipper.rights)
      }
    }

    def nextCursor(): Option[Cursor] = {
      ZipCursor.init{
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

    def slurpRight(p: (Seq[GridCell], GridCell) => Boolean): Window = {
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
      val ins = zipper.focus.createRightInsert(char)
      Window(
        cells :+ ins,
        zipper.lefts, zipper.focus, zipper.rights
      )

    }
  }

  sealed trait Cursor { self =>
    def focus: GridCell
    def next: Option[Cursor]
    def prev: Option[Cursor]

    def move(n: Int): Option[Cursor]

    def insertRight(g: GridCell): Cursor
    def insertLeft(g: GridCell): Cursor
    def atStart: Boolean
    def atEnd: Boolean

    def start: Cursor
    def end: Cursor

    def findNext(p: GridCell => Boolean): Option[Cursor]
    def findPrevious(p: GridCell => Boolean): Option[Cursor]

    def toWindow(): Window

    def slurpRight(p: GridCell => Boolean): Window

    def addLabel(label: Label): Unit = {
      focus.addLabel(label)
    }

    def foreachC(f: Cursor => Option[Cursor]): Row  = {
      f(self) match {
        case Some(cmod) => cmod.next match {
          case Some(cnext) => cnext.foreachC(f)
          case None => cmod.toRow

        }
        case None => self.toRow
      }
    }

    // def foreach(f: Cursor => Unit): Unit  = {
    //   f(this)
    //   next match {
    //     case Some(n) => n.foreach(f)
    //     case None => ()
    //   }
    // }

    def toRow: Row

    def insertCharLeft(c: Char): Cursor =  insertLeft(focus.createLeftInsert(c))
    def insertCharRight(c: Char): Cursor =  insertRight(focus.createRightInsert(c))
  }

  object ZipCursor {
    def init(z: Zipper[GridCell]) = new ZipCursor {
      def zipper: Zipper[GridCell] = z
    }

    def init(optZ: Option[Zipper[GridCell]]) = optZ.map{ z =>
      new ZipCursor {
        def zipper: Zipper[GridCell] = z
      }
    }

    // def init(z: Zipper[Seq[GridCell]]) = new ZipCursor {
    //   def zipper: Zipper[Seq[GridCell]] = z
    // }

    // def init(optZ: Option[Zipper[Seq[GridCell]]]) = optZ.map{ z =>
    //   new ZipCursor {
    //     def zipper: Zipper[Seq[GridCell]] = z
    //   }
    // }
  }

  trait ZipCursor extends Cursor {
    def zipper: Zipper[GridCell]

    def focus: GridCell = zipper.focus

    def next: Option[Cursor] =
      ZipCursor.init{ zipper.next }

    def prev: Option[Cursor] =
      ZipCursor.init{ zipper.previous }

    def insertLeft(g: GridCell): Cursor =
      ZipCursor.init{ zipper.insertLeft(g)}

    def insertRight(g: GridCell): Cursor =
      ZipCursor.init{ zipper.insertRight(g)}


    def atStart: Boolean = zipper.atStart
    def atEnd: Boolean = zipper.atEnd

    def start: Cursor = ZipCursor.init{ zipper.start }
    def end: Cursor = ZipCursor.init{ zipper.end }

    def toWindow(): Window = {
      Window(Seq(focus), zipper.lefts, zipper.focus, zipper.rights)
    }

    def move(n: Int): Option[Cursor] =
      ZipCursor.init{ zipper.move(n) }

    def slurpRight(p: GridCell => Boolean): Window = {
      val (winTail, tail) = zipper.rights.span(p)
      val window = zipper.focus +: winTail
      Window(window, zipper.lefts, zipper.focus, tail)
    }

    def findNext(p: GridCell => Boolean): Option[Cursor] = {
      ZipCursor.init{ zipper.findNext(p) }
    }

    def findPrevious(p: GridCell => Boolean): Option[Cursor] = {
      ZipCursor.init{ zipper.findPrevious(p) }
    }

    def toRow: Row = {
      Row.fromCells(zipper.toList)
    }
  }

  // trait ZipMultiCursor extends MultiCursor {
  //   def zipper: Zipper[Seq[GridCell]]

  //   def focus: Seq[GridCell] = zipper.focus

  //   def next: Option[Cursor] =
  //     ZipCursor.init{ zipper.next }

  //   def prev: Option[Cursor] =
  //     ZipCursor.init{ zipper.previous }

  //   def insertLeft(g: GridCell): Cursor =
  //     ZipCursor.init{ zipper.insertLeft(Seq(g))}

  //   def insertRight(g: GridCell): Cursor =
  //     ZipCursor.init{ zipper.insertRight(Seq(g))}


  //   def atStart: Boolean = zipper.atStart
  //   def atEnd: Boolean = zipper.atEnd

  //   def move(n: Int): Option[Cursor] =
  //     ZipCursor.init{ zipper.move(n) }

  //   def toRow: Row = {
  //     Row.fromCells(zipper.toList.flatten)
  //   }
  // }

}
