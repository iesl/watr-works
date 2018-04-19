package edu.umass.cs.iesl.watr
package utils

import com.google.{common => guava}
import guava.{collect => gcol}
import textboxing.{TextBoxing => TB}, TB._
import scalaz.{@@ => _, Ordering => _, _}, Scalaz._
import scala.collection.JavaConverters._

object GuavaHelpers {
  import scala.collection.mutable

  case class GuavaTableMatrix[X, Y, A](
    rowKeys: List[X],
    colKeys: List[Y],
    rows: Seq[Seq[A]]
  )

  def guavaTableToLabeledBox[A: Ordering, B: Ordering, C <: Any](
    table: gcol.Table[A, B, C], zero: C,
    topLabel: String ,
    leftLabel: String,
    rightMarginalFunc: (C, C) => C,
    bottomMarginalFunc: (C, C) => C
  ): TB.Box = {
    guavaTableToBox(table, zero,
      Some(topLabel), Some(leftLabel),
      Some(rightMarginalFunc), Some(bottomMarginalFunc)
    )
  }

  def guavaTableToBox[A: Ordering, B: Ordering, C <: Any](
    table: gcol.Table[A, B, C], zero: C,
    topLabel: Option[String] = None,
    leftLabel: Option[String] = None,
    rightMarginalFunc: Option[(C, C) => C] = None,
    bottomMarginalFunc: Option[(C, C) => C] = None,
  ): TB.Box = {
    val rowKeys = table.rowKeySet().asScala.toList.sorted
    val columnKeys = table.columnKeySet().asScala.toList.sorted
    val rows = (0 until rowKeys.length).map{ r =>
      mutable.ArrayBuffer.fill[C](columnKeys.length)(zero)
    }

    for {
      (rowk, rown) <- rowKeys.zipWithIndex
      (colk, coln) <- columnKeys.zipWithIndex
    }  {
      val c = table.get(rowk, colk)
      if (c!=null) {
        rows(rown)(coln) = c
      }
    }

    var boxedTable = rows.map{ row =>
      row.map(_.toString().box).toList.intersperse(" ┆ ")
    }

    boxedTable = rightMarginalFunc.map{ f =>
      val sums = rowKeys.map{ k =>
        val row = table.row(k).asScala.values.toList
        List(" |= ".box, row.reduce(f).toString.box)
      }
      boxedTable.zip(sums).map { case (r, s) => r ++ s }
    } getOrElse { boxedTable }

    boxedTable = bottomMarginalFunc.map{ f =>
      val sums = columnKeys.map{ k =>
        val col = table.column(k).asScala.values.toList
        col.reduce(f).toString.box
      }
      boxedTable :+ (List(hspace(1), hspace(1)))
      boxedTable :+ sums.intersperse(" = ") // .map(x => TB.borderTB("=")(x))
    } getOrElse { boxedTable }


    val rowLabels = vspace(1) atop vjoins(right, rowKeys.map(_.toString().box))
    // val colLabels  = columnKeys.map(_.toString().box).intersperse(" ║ ")
    val colLabels  = columnKeys.map(_.toString().takeRight(8).mkString.box).intersperse(" ║ ")

    boxedTable = colLabels +: boxedTable


    // val grid = Grid.withCols(rowKeys.length, TB.AlignRight)
    //   .addRow(colLabels:_*)

    // val filledGrid =
    //   rows.zip(rowLabels).zip(rightMarginals)
    //     .foldLeft(grid){ case (accGrid, ((row, rowLabel), rightMarginal)) =>
    //       val colBoxes = row.map(c => c.toString().mbox).toList.intersperse(" ┆ ".box)
    //       accGrid.addRow((rowLabel:: " ┃ ".box :: (colBoxes :+ rightMarginal)):_*)
    //     }

    // filledGrid.addRow(bottomMarginals:_*).toBox()
    // val rowBoxes = boxedTable.map{ row =>
    //   hjoins(top, row)
    // }

    val colWise = switchRowsToCols(boxedTable, emptyBox(1, 1))

    val cols = hjoins(top,
      colWise.map{ col =>
        vjoins(left, col)
      }
    )

    hjoin(
      borderLeftRight("", " ┃ ")(rowLabels),
      cols
    )

  }

  import scala.annotation.tailrec
  import scalaz.syntax.std.list._

  def switchRowsToCols[A](in: Seq[Seq[A]], pad: A): List[List[A]] = {

    @tailrec
    def loop(bs: List[List[A]], acc: List[List[A]]): List[List[A]] = {
      if (bs.exists(_.nonEmpty)) {
        val h = bs.map(_.headOption.getOrElse(pad))
        val ts = bs.map(_.tailOption.getOrElse(Nil))
        loop(ts, h :: acc)
      } else acc.reverse
    }

    loop(in.toList.map(_.toList), List[List[A]]())
  }



  def initTable[RowT: Ordering, ColT: Ordering, A <: Any](): TabularData[RowT, ColT, A] = {
    new TabularData[RowT, ColT, A](
      gcol.HashBasedTable.create[RowT, ColT, Any]()
    )
  }

  def initTableFromGuava[RowT: Ordering, ColT: Ordering, A <: Any](
    table: gcol.Table[RowT, ColT, Any]
  ): TabularData[RowT, ColT, A] = {
    new TabularData[RowT, ColT, A](table)
  }

}


class TabularData[RowT: Ordering, ColT: Ordering, A](
  table: gcol.Table[RowT, ColT, Any]
) {
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

  // def getOrElse(r: RowT, c: ColT, default: => A): A = {
  //   val a = get(r, c)
  //   if (a != null) a else default
  // }

  def modify(r: RowT, c: ColT, fa: A => A): Unit = {
    get(r, c) match {
      case Some(a0) =>
        table.put(r, c, fa(a0))
      case None =>
    }
  }

  def modifyOrSet(r: RowT, c: ColT, fa: A => A, z: => A): Unit = {
    get(r, c) match {
      case Some(a0) =>
        table.put(r, c, fa(a0))
      case None =>
        table.put(r, c, z)
    }
  }

  def foreach(f: A => Unit): Unit = {
    val rowKeys = table.rowKeySet().asScala
    val columnKeys = table.columnKeySet().asScala

    for {
      rowk <- rowKeys
      colk <- columnKeys
    } {
      get(rowk, colk).foreach(f)
    }
  }

  def modEach(f: A => A): Unit = {

    val rowKeys = table.rowKeySet().asScala
    val columnKeys = table.columnKeySet().asScala

    for {
      rowk <- rowKeys
      colk <- columnKeys
    } {
      modify(rowk, colk, f)
    }

  }

  def map[B](f: A => B): TabularData[RowT, ColT, B] = {
    val table2 = gcol.HashBasedTable.create[RowT, ColT, Any]()

    val rowKeys = table.rowKeySet().asScala
    val columnKeys = table.columnKeySet().asScala

    for {
      rowk <- rowKeys
      colk <- columnKeys
    } {
      get(rowk, colk).foreach { a =>
        if (a != null) table2.put(rowk, colk, f(a))
      }
    }

    new TabularData[RowT, ColT, B](table2)
  }

  def mapColumns[B](z: => B)(f: (B, A) => B): Seq[(ColT, B)] = {
    val columnKeys = table.columnKeySet().asScala.toList.sorted

    for {
      colk <- columnKeys
    } yield {
      val column = table.column(colk)
      val total = column.asScala.toSeq.foldLeft(z){ case  (acc, (rowk, a)) =>
        f(acc, a.asInstanceOf[A])
      }
      (colk, total)
    }
  }

  def mapRows[B](z: => B)(f: (B, A) => B): Seq[(RowT, B)] = {
    val rowKeys = table.rowKeySet().asScala.toList.sorted

    for {
      rowk <- rowKeys
    } yield {
      val row = table.row(rowk)
      val total = row.asScala.toSeq.foldLeft(z){ case  (acc, (colk, a)) =>
        f(acc, a.asInstanceOf[A])
      }
      (rowk, total)
    }
  }

  def toReportBox(): TB.Box = {
    GuavaHelpers.guavaTableToBox(
      table, 0
    )
  }


}
