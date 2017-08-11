package edu.umass.cs.iesl.watr
package textgrid

import scala.collection.mutable
import watrmarks._
import geometry._
import scalaz.{@@ => _, _} , Scalaz._
import utils.SlicingAndDicing._

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

    def createLeftInsert(ch: Char): InsertCell = LeftInsertCell(ch, this)
    def createRightInsert(ch: Char): InsertCell = RightInsertCell(ch, this)

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

    def sliding(n: Int): Option[Cursor]

    def foreach(f: GridCell => Unit): Unit  = {
      cells.foreach(f(_))
    }

    def groupBy(
      groupf: (GridCell, GridCell) => Boolean
    ): Option[Cursor] = {

      val grouped = cells.groupByPairs(groupf)

      grouped.toList.toZipper.map{zip =>
        new ZipCursor {
          def zipper: Zipper[Seq[GridCell]] = zip
        }
      }
    }

    def toText(): String = {

      // cells.map { _ match {
      //   case cell@ TextGrid.LeftExpansionCell(char, root)  => char.toString()
      //   case cell@ TextGrid.RightExpansionCell(char, root) => char.toString()
      //   case cell@ TextGrid.LeftInsertCell(char, loc)      => char.toString()
      //   case cell@ TextGrid.RightInsertCell(char, loc)     => char.toString()
      //   case cell@ TextGrid.PageItemCell(headItem, tailItems, char, _) =>
      // }}

      cells.map(_.char).mkString("")
    }
  }

  trait Grid {

  }

  abstract class MutableRow extends Row {
    override val cells: mutable.ArrayBuffer[GridCell] = mutable.ArrayBuffer()

    def sliding(n: Int): Option[Cursor] = {
      cells.sliding(n).toList.toZipper.map{zip =>
        new ZipCursor {
          def zipper: Zipper[Seq[GridCell]] = zip
        }
      }
    }
  }


  object Row {

    def fromCells(init: Seq[GridCell]): Row = new MutableRow {
      cells.appendAll(init)
    }
  }



  sealed trait Cursor { self =>
    def focus: Seq[GridCell]
    def next: Option[Cursor]
    def prev: Option[Cursor]

    def move(n: Int): Option[Cursor]

    def insertRight(g: GridCell): Cursor
    def insertLeft(g: GridCell): Cursor
    def atStart: Boolean
    def atEnd: Boolean

    def addLabel(label: Label): Unit = {
      if (focus.length==1) {
        focus.foreach(_.addPin(label.U))
      } else if (focus.length > 1) {
        focus.head.addPin(label.B)
        focus.last.addPin(label.L)
        focus.drop(1).dropRight(1).foreach(
          _.addPin(label.I)
        )
      }
    }

    def unfoldBy(f: Cursor => Option[Cursor]): Cursor  = {
      f(self)
        .flatMap(_.next.map(_.unfoldBy(f)))
        .getOrElse(self)
    }

    def foreach(f: Cursor => Unit): Unit  = {
      f(this)
      next match {
        case Some(n) => n.foreach(f)
        case None => ()
      }
    }

    def toRow: Row
  }

  object ZipCursor {
    def init(z: Zipper[Seq[GridCell]]) = new ZipCursor {
      def zipper: Zipper[Seq[GridCell]] = z
    }

    def init(optZ: Option[Zipper[Seq[GridCell]]]) = optZ.map{ z =>
      new ZipCursor {
        def zipper: Zipper[Seq[GridCell]] = z
      }
    }
  }

  trait ZipCursor extends Cursor {
    def zipper: Zipper[Seq[GridCell]]

    def focus: Seq[GridCell] = zipper.focus

    def next: Option[Cursor] =
      ZipCursor.init{ zipper.next }

    def prev: Option[Cursor] =
      ZipCursor.init{ zipper.previous }

    def insertLeft(g: GridCell): Cursor =
      ZipCursor.init{ zipper.insertLeft(Seq(g))}

    def insertRight(g: GridCell): Cursor =
      ZipCursor.init{ zipper.insertRight(Seq(g))}


    def atStart: Boolean = zipper.atStart
    def atEnd: Boolean = zipper.atEnd

    def move(n: Int): Option[Cursor] =
      ZipCursor.init{ zipper.move(n) }

    def toRow: Row = {
      Row.fromCells(zipper.toList.flatten)
    }
  }

}
