package org.watrworks
package textgrid

import scala.collection.mutable
import geometry._
import geometry.PageComponentImplicits._
import textboxing.{TextBoxing => TB}, TB._
import annots._

import utils.SlicingAndDicing._

trait TextGrid { self =>

  import TextGrid._

  def documentId: String @@ DocumentID

  object cellLabels extends LabeledSequence[GridCell] {
    def labelTargets(): Seq[GridCell] = self.gridCells()
  }

  def rows(): Seq[Row]

  def toText(): String = {
    rows().map(_.toText()).mkString("\n")
  }

  def splitOneLeafLabelPerLine(): TextGrid = {
    val splitRows = rows().flatMap { row =>
      row.splitOnLeafLabels()
    }

    TextGrid.fromRows(documentId, splitRows)
  }

  def gridCells(): Seq[GridCell] = {
    indexedCells().map(_._1)
  }

  def indexedCells(): Seq[(GridCell, Int, Int)] = {
    for {
      (row, rowNum) <- rows().zipWithIndex
      (cell, colNum) <- row.cells().zipWithIndex
    } yield { (cell, rowNum, colNum) }
  }

  def rowAt(row: Int): Option[Row] = {
    if (0 <= row && row < rows().length) {
      Some(rows().apply(row))
    } else None
  }

  def indexedCellAt(row: Int, col: Int): Option[(Int, GridCell)] = {
    val (pre, rest) = indexedCells().span { case (cell @ _, r, c) =>
      r != row && c != col
    }
    rest.headOption.map { case (cell, _, _) =>
      (pre.length, cell)
    }
  }

  def indexAt(row: Int, col: Int): Option[Int] = {
    indexedCellAt(row, col).map(_._1)
  }

  def cellAt(row: Int, col: Int): Option[GridCell] = {
    rowAt(row).flatMap { r =>
      r.get(col)
    }
  }

  def pageBounds(): Seq[PageRegion] = {

    val allBounds = rows().flatMap { row => row.pageBounds() }

    val regionsByPages = allBounds.groupBy(_.page.pageNum)
    regionsByPages.map { case (pageNum @ _, pageRegions) =>
      val headRegion = pageRegions.head.page
      val pageBbox = pageRegions.map(_.bbox).reduce(_ union _)
      PageRegion(
        headRegion,
        pageBbox
      )
    }.toList
  }

  def split(row: Int, col: Int): Option[TextGrid] = {
    if (0 <= row && row < rows().length) {
      rows().apply(row).split(col).map { case (row1, row2) =>
        val (pre, post) = rows().splitAt(row)
        val end = row1 +: row2 +: (post.drop(1))
        val newRows = pre ++ end
        TextGrid.fromRows(documentId, newRows)
      }
    } else None
  }

  def slurp(row: Int): Option[TextGrid] = {
    if (0 <= row && row < rows().length - 1) {
      val (pre, post) = rows().splitAt(row + 1)
      val r1 = pre.last
      val r2 = post.head

      val r12 = r1.append(r2)
      val newRows = pre.dropRight(1) ++ (r12 +: post.drop(1))
      val newGrid = TextGrid.fromRows(documentId, newRows)

      // rows can only be joined if they share the same label stack
      val maybeNewGrid = r12.cells().headOption.map { c0 =>
        val headCellPins = c0.pins
        val pinlen = headCellPins.length
        val equalPinStackSize = r12.cells().forall(_.pins.length == pinlen)

        if (equalPinStackSize) {
          if (pinlen == 0) Some(newGrid)
          else {
            val headTopPin = c0.topPin().get
            val validJoin = headTopPin.isBegin || headTopPin.isInside && {
              val allInsideButLast = r12.cells().tail.dropRight(1).forall { c =>
                val ctop = c.topPin().get
                ctop.isInside
              }
              val lastpin = r12.cells().last.topPin().get
              val endsWithInsideOrLast = lastpin.isInside || lastpin.isLast

              allInsideButLast && endsWithInsideOrLast
            }

            if (validJoin) Some(newGrid) else None
          }
        } else None
      }
      maybeNewGrid.flatten
    } else None
  }
}

object TextGrid {

  sealed trait GridCell extends LabelTarget {
    def pageRegion: PageRegion

    def char: Char

    def createInsert(ch: Char): InsertCell = InsertCell(ch, this.pageRegion)

    def showCell(): Box = {
      vjoin(left, char.toString(), showPinsVert())
    }

    def isGlyphCell(): Boolean

    val prepended = mutable.ArrayBuffer[Char]()
    val appended = mutable.ArrayBuffer[Char]()
    def prepend(ch: Char): Unit = prepended.prepend(ch)
    def append(ch: Char): Unit = appended.append(ch)

    def expand(): Seq[GridCell] = {
      val pre = prepended.map(createInsert(_))
      val post = appended.map(createInsert(_))

      (pre ++ (this +: post)).toSeq
    }
  }

  def mbrRegionFunc(h: PageItem, tail: Seq[PageItem]): PageRegion = {
    (h +: tail).map(_.pageRegion).reduce { _ union _ }
  }

  case class PageItemCell(
    headItem: PageItem,
    tailItems: Seq[PageItem] = List(),
    override val char: Char,
    regionFunc: (PageItem, Seq[PageItem]) => PageRegion = mbrRegionFunc(_, _)
  ) extends GridCell {
    override val pageRegion: PageRegion = regionFunc(headItem, tailItems)
    def isGlyphCell(): Boolean = true
  }

  case class InsertCell(
    char: Char,
    insertAt: PageRegion
  ) extends GridCell {
    override val pageRegion: PageRegion = insertAt
    def isGlyphCell(): Boolean = false
  }

  trait Row extends LabelTarget with LabeledSequence[GridCell] {

    def expand(): Row = Row.fromCells(cells().flatMap(_.expand()))

    def cells(): Seq[GridCell] = labelTargets()

    private def isSpace(gc: GridCell) = gc.char == ' '
    private def trimRight(cs: Seq[GridCell]) = cs.reverse.dropWhile(isSpace(_)).reverse

    // Text reshaping:
    def trimRight(): Row = {
      Row.fromCells(trimRight(cells()))
    }

    def padRight(): Row = {
      cells().lastOption.map { c =>
        Row.fromCells(cells() :+ c.createInsert(' '))
      } getOrElse { this }
    }

    def split(col: Int): Option[(Row, Row)] = {
      if (0 < col && col < cells().length) {
        val (c1, c2) = cells().splitAt(col)

        Some(
          (
            Row.fromCells(c1),
            Row.fromCells(c2)
          )
        )
      } else None
    }

    def splitOnLeafLabels(): Seq[Row] = {
      val groups = cells().groupByPairs((a, b) => {
        (a.topPin(), b.topPin()) match {
          case (Some(pin1), Some(pin2)) =>
            val isBIL = pin1.isBegin && (pin2.isInside || pin2.isLast)
            val isIIL = pin1.isInside && (pin2.isInside || pin2.isLast)
            val sameLabel = pin1.label == pin2.label
            sameLabel && (isBIL || isIIL)

          case (None, None) => true
          case _            => false
        }
      })

      groups.map { group =>
        val r = Row.fromCells(group)
        r.cells().head.topPin().foreach { pin =>
          r.addLabel(pin.label)
        }
        r
      }
    }

    def append(row: Row): Row = {
      Row.fromCells(cells() ++ row.cells())
    }

    def pageBounds(): Seq[PageRegion] = {
      val regionsByPages = cells().groupBy(_.pageRegion.page.pageNum)

      regionsByPages.map { case (pageNum @ _, pageRegions) =>
        val headRegion = pageRegions.head.pageRegion
        val pageBbox = pageRegions.map(_.pageRegion.bbox).reduce(_ union _)
        PageRegion(
          headRegion.page,
          pageBbox
        )
      }.toList
    }

    def foreach(f: GridCell => Unit): Unit = {
      cells().foreach(f(_))
    }

    def toText(): String = {
      cells().map(_.char).mkString("")
    }

    def showRow(): Box = {
      hcat(top, cells().map(_.showCell()))
    }
  }

  object Row {
    def fromCells(init: Seq[GridCell]): Row = new Row {
      override val labelTargets = init // labelTargets.appendAll(init)
    }
  }

  def fromRows(id: String @@ DocumentID, init: Seq[Row]): TextGrid = new TextGrid {
    override val documentId = id
    override val rows = init
  }
}