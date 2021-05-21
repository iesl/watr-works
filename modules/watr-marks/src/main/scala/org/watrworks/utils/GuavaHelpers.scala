package org.watrworks
package utils

import com.google.{common => guava}
import guava.{collect => gcol}
import textboxing.{TextBoxing => TB}, TB._
import scalaz.{@@ => _, Ordering => _, _}, Scalaz._
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.annotation.tailrec

case class AxisLabels(
  rowLabel: String,
  colLabel: String
)

object GuavaHelpers {

  import scalaz.syntax.std.list._

  def switchRowsToCols[A](in: Seq[Seq[A]], pad: A): List[List[A]] = {

    @tailrec
    def loop(bs: List[List[A]], acc: List[List[A]]): List[List[A]] = {
      if (bs.exists(_.nonEmpty)) {
        val h  = bs.map(_.headOption.getOrElse(pad))
        val ts = bs.map(_.tailOption.getOrElse(Nil))
        loop(ts, h :: acc)
      } else acc.reverse
    }

    loop(in.toList.map(_.toList), List[List[A]]())
  }

  def initTable[RowT: Ordering, ColT: Ordering, A <: Any]()
    : TabularData[RowT, ColT, A, Unit, Unit] = {
    TabularData[RowT, ColT, A, Unit, Unit](
      gcol.HashBasedTable.create[RowT, ColT, Any]()
    )
  }

  def initTableFromGuava[RowT: Ordering, ColT: Ordering, A <: Any](
    table: gcol.Table[RowT, ColT, Any]
  ): TabularData[RowT, ColT, A, Unit, Unit] = {
    TabularData[RowT, ColT, A, Unit, Unit](table, None, None, None)
  }

  implicit val StringShow = Show.shows[String](s => s)

  implicit def ShowGuavaMultiSet[A](implicit ShowA: Show[A]): Show[gcol.TreeMultiset[A]] = {

    Show.shows[gcol.TreeMultiset[A]](ms => {
      ms.entrySet()
        .asScala
        .to(List)
        .map(entry => {
          val count = entry.getCount()
          val elem  = entry.getElement()
          (count, elem)
        })
        .sortBy(_._1)
        .reverse
        .map({ case (count, elem) =>
          val shElem = ShowA.show(elem)
          s"${count}▸$shElem"
        })
        .mkString(",")

    })
  }

}
object TabularData {

  type MarginalAccFunc[A, B] = (B, A) => B
  type MarginalAcc[A, B]     = (B, MarginalAccFunc[A, B])
}

case class TabularData[RowT: Ordering, ColT: Ordering, A, RowMarginT, ColMarginT](
  table: gcol.Table[RowT, ColT, Any],
  rowMarginals: Option[Map[RowT, RowMarginT]] = None,
  colMarginals: Option[Map[ColT, ColMarginT]] = None,
  topLabel: Option[String] = None,
  leftLabel: Option[String] = None
) {

  type TabularDataT          = TabularData[RowT, ColT, A, RowMarginT, ColMarginT]
  type TabularDataMapA[U]    = TabularData[RowT, ColT, U, RowMarginT, ColMarginT]
  type TabularDataMapRowM[U] = TabularData[RowT, ColT, A, U, ColMarginT]
  type TabularDataMapColM[U] = TabularData[RowT, ColT, A, RowMarginT, U]

  def getRowMarginal(rowk: RowT): Option[RowMarginT] = {
    rowMarginals.flatMap { _.get(rowk) }
  }

  def getColMarginal(colk: ColT): Option[ColMarginT] = {
    colMarginals.flatMap { _.get(colk) }
  }

  def addLeftLabel(l: String): TabularDataT = {
    copy(leftLabel = Some(l))
  }
  def addTopLabel(l: String): TabularDataT = {
    copy(topLabel = Some(l))
  }

  def computeRowMarginals[RowMarginU](z: => RowMarginU)(
    f: (RowMarginU, A) => RowMarginU
  ): TabularDataMapRowM[RowMarginU] = {
    val rms = foldLeftRows(z)(f).toMap
    copy(rowMarginals = Some(rms))
  }

  def computeColMarginals[ColMarginU](z: => ColMarginU)(
    f: (ColMarginU, A) => ColMarginU
  ): TabularDataMapColM[ColMarginU] = {
    val cms = foldLeftColumns(z)(f).toMap
    copy(colMarginals = Some(cms))
  }

  def rowKeys    = table.rowKeySet().asScala.toList.sorted
  def columnKeys = table.columnKeySet().asScala.toList.sorted

  def getColumn(c: ColT): Seq[(RowT, A)] = {
    table.column(c).entrySet().asScala.toList.map { e =>
      (e.getKey, e.getValue.asInstanceOf[A])
    }
  }

  def getColumns(): Seq[(ColT, Seq[(RowT, A)])] = {
    columnKeys.toList.sorted.map(c => (c, getColumn(c)))
  }

  def getRow(r: RowT): Seq[(ColT, A)] = {
    table.row(r).entrySet().asScala.toList.map { e =>
      (e.getKey, e.getValue.asInstanceOf[A])
    }
  }

  def getRows(): Seq[(RowT, Seq[(ColT, A)])] = {
    rowKeys.toList.sorted.map(r => (r, getRow(r)))
  }

  def set(r: RowT, c: ColT, a: A): Unit = {
    table.put(r, c, a)
  }

  def apply(r: RowT, c: ColT): A = {
    table.get(r, c).asInstanceOf[A]
  }

  def get(r: RowT, c: ColT): Option[A] = {
    val a = apply(r, c)
    if (a != null) Some(a) else None
  }

  def modify(r: RowT, c: ColT, fa: A => A): Unit = {
    get(r, c) match {
      case Some(a) => table.put(r, c, fa(a))
      case None    =>
    }
  }

  def modifyOrSet(r: RowT, c: ColT, fa: A => A, z: => A): Unit = {
    get(r, c) match {
      case Some(a) => table.put(r, c, fa(a))
      case None    => table.put(r, c, z)
    }
  }

  def useOrSet(r: RowT, c: ColT, fa: A => Unit, z: => A): Unit = {
    get(r, c) match {
      case Some(a) => fa(a)
      case None    => table.put(r, c, z)
    }
  }

  def foreach(f: A => Unit): Unit = for {
    rowk <- rowKeys
    colk <- columnKeys
  } get(rowk, colk).foreach(f)

  def foreachLocation(f: (A, Int, Int) => Unit): Unit = for {
    (rowk, rowi) <- rowKeys.zipWithIndex
    (colk, coli) <- columnKeys.zipWithIndex
  } get(rowk, colk).foreach(a => f(a, rowi, coli))

  def modEach(f: A => A): Unit = for {
    rowk <- rowKeys
    colk <- columnKeys
  } modify(rowk, colk, f)

  def map[B](f: A => B): TabularDataMapA[B] = {
    val table2 = gcol.HashBasedTable.create[RowT, ColT, Any]()

    for {
      rowk <- rowKeys
      colk <- columnKeys
    } {
      get(rowk, colk).foreach { a =>
        if (a != null) table2.put(rowk, colk, f(a))
      }
    }

    copy(table = table2)
  }

  def foldLeftColumns[B](z: => B)(f: (B, A) => B): Seq[(ColT, B)] = {
    for {
      colk <- columnKeys.toList.sorted
    } yield {
      val column = table.column(colk)
      val total = column.asScala.toSeq.foldLeft(z) { case (acc, (rowk @ _, a)) =>
        f(acc, a.asInstanceOf[A])
      }
      (colk, total)
    }
  }

  def foldLeftRows[B](z: => B)(f: (B, A) => B): Seq[(RowT, B)] = {
    for {
      rowk <- rowKeys.toList.sorted
    } yield {
      val row = table.row(rowk)
      val total = row.asScala.toSeq.foldLeft(z) { case (acc, (colk @ _, a)) =>
        f(acc, a.asInstanceOf[A])
      }
      (rowk, total)
    }
  }

  import GuavaHelpers._

  def showBox(zero: String = "∅")(implicit
    ShowA: Show[A]
  ): TB.Box = {

    val rowKeys    = table.rowKeySet().asScala.toList.sorted
    val columnKeys = table.columnKeySet().asScala.toList.sorted

    val rows = (0 until rowKeys.length).map { r =>
      mutable.ArrayBuffer.fill[String](columnKeys.length)(zero)
    }

    foreachLocation { case (a, rown, coln) =>
      rows(rown)(coln) = ShowA.shows(a)
    }

    val cellBoxes = rows.map { row =>
      val (r, g, b)  = (20, 30, 128)
      val fa = fansi.Color.True(r, g, b)
      row.to(List).map(s => {
        s.box
      }).intersperse(" ┆ ")

    }

    // ====
    val withRowMarginals = rowMarginals.map { rms =>
      cellBoxes
        .zip(
          rms.map { case (rowk @ _, rm) =>
            hjoin(" ▐ ", rm.toString().box)
          }
        )
        .map { case (rowCells, rm) => rowCells :+ rm }
    } getOrElse { cellBoxes }

    val withRowAndColMarginals: Seq[Seq[Box]] = colMarginals.map { cms =>
      val cmBoxes = cms.map(_._2.toString().box).toList.intersperse(" ▒ ")

      withRowMarginals :+ cmBoxes
    } getOrElse { withRowMarginals }

    val rowLabels = vspace(1) atop vjoins(right, rowKeys.map(_.toString().box))
    val colLabels =
      columnKeys.map(_.toString().mkString.box).intersperse(" ▒ ") ++ List(
        " ▜ ".box,
        hspace(1)
      )

    val colWise = switchRowsToCols(
      withRowAndColMarginals,
      emptyBox(1, 1)
    )

    val withColLabels =
      colLabels.zipAll(colWise, hspace(1), List()).map { case (l, col) => l :: col }

    val cols = hjoins(
      top,
      withColLabels.map { col =>
        vjoins(left, col)
      }
    )
    val tLabel = topLabel.map("  ".box + _.box).getOrElse(emptyBox(0, 0))
    val lLabel = leftLabel
      .map(l => {
        vjoins(left, l.toList.map(_.toString().box))
      })
      .getOrElse(emptyBox(0, 0))

    vjoinWith(
      left,
      // vspace(1),
      nullBox,
      List(
        tLabel,
        hjoinWith(
          top,
          nullBox,
          List(
            borderLeftRight("", " ┇ ")(lLabel),
            hspace(1),
            hjoin(
              borderLeftRight("", " ▌ ")(rowLabels),
              cols
            )
          )
        )
      )
    )

  }

}
