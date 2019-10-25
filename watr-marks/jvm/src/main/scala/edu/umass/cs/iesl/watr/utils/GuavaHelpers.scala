package edu.umass.cs.iesl.watr
package utils

import com.google.{common => guava}
import guava.{collect => gcol}
import textboxing.{TextBoxing => TB}, TB._
import scalaz.{@@ => _, Ordering => _, _}, Scalaz._
import scala.collection.JavaConverters._
import scala.collection.mutable

case class AxisLabels(
  rowLabel: String,
  colLabel: String,
)

object GuavaHelpers {

  def guavaTableToLabeledBox[A: Ordering, B: Ordering, C <: Any](
    table: gcol.Table[A, B, C], zero: C,
    topLabel: String,
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
    val colLabels  = columnKeys.map(_.toString().takeRight(8).mkString.box).intersperse(" ║ ")

    boxedTable = colLabels +: boxedTable


    // val grid = Grid.withCols(rowKeys.length, TB.AlignRight)
    //   .addRow(colLabels:_*)

    // val filledGrid =
    //   rows.zip(rowLabels).zip(rightMarginals)
    //     .foldLeft(grid){ case (accGrid, ((row, rowLabel), rightMarginal)) =>
    //       val colBoxes = row.map(c => c.toString().box).toList.intersperse(" ┆ ".box)
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




  def initTable[RowT: Ordering, ColT: Ordering, A <: Any](): TabularData[RowT, ColT, A, Unit, Unit] = {
    new TabularData[RowT, ColT, A, Unit, Unit](
      gcol.HashBasedTable.create[RowT, ColT, Any](),
      None, None, None
    )
  }

  def initTableFromGuava[RowT: Ordering, ColT: Ordering, A <: Any](
    table: gcol.Table[RowT, ColT, Any]
  ): TabularData[RowT, ColT, A, Unit, Unit] = {
    new TabularData[RowT, ColT, A, Unit, Unit](table,
      None, None, None
    )
  }


  implicit object StringShow extends Show[String] {
    override def show(f: String): Cord = Cord(FingerTree.three("\"", f, "\"")(Cord.sizer).toTree)
    override def shows(f: String): String = f
  }

}


class TabularData[RowT: Ordering, ColT: Ordering, A, RM, CM](
  table: gcol.Table[RowT, ColT, Any],
  rowMarginals: Option[Seq[(RowT, RM)]],
  colMarginals: Option[Seq[(ColT, CM)]],
  axisLabels: Option[AxisLabels]
) {

  def getRowMarginal(rowk: RowT): Option[RM] = {
    rowMarginals.flatMap{ rm =>
      rm.filter(_._1==rowk).headOption.map(_._2)
    }
  }

  def getColMarginal(colk: ColT): Option[CM] = {
    colMarginals.flatMap{ rm =>
      rm.filter(_._1==colk).headOption.map(_._2)
    }
  }

  def addLabels(rowLabel: String, colLabel: String): TabularData[RowT, ColT, A, RM, CM] = {
    new TabularData(
      table,
      rowMarginals,
      colMarginals,
      Some(AxisLabels(rowLabel, colLabel))
    )
  }

  def computeRowMarginals[B](z: => B)(f: (B, A) => B): TabularData[RowT, ColT, A, B, CM] = {
    val rms = mapRows(z)(f)
    new TabularData(
      table,
      Some(rms),
      colMarginals,
      axisLabels
    )
  }

  def computeColMarginals[B](z: => B)(f: (B, A) => B): TabularData[RowT, ColT, A, RM, B] = {
    val cms = mapColumns(z)(f)
    new TabularData(
      table,
      rowMarginals,
      Some(cms),
      axisLabels
    )
  }

  def rowKeys(): Set[RowT] = table.rowKeySet().asScala.toSet
  def columnKeys(): Set[ColT] = table.columnKeySet().asScala.toSet

  def getColumn(c: ColT): Seq[(RowT, A)] = {
    table.column(c).entrySet().asScala.toList.map{ e =>
      (e.getKey, e.getValue.asInstanceOf[A])
    }
  }


  def getRow(r: RowT): Seq[(ColT, A)] = {
    table.row(r).entrySet().asScala.toList.map{ e =>
      (e.getKey, e.getValue.asInstanceOf[A])
    }
  }

  def getRows(): Seq[(RowT, Seq[(ColT, A)])] = {
    rowKeys().toList.sorted.map(r => (r, getRow(r)))
  }

  def getColumns(): Seq[(ColT, Seq[(RowT, A)])] = {
    columnKeys().toList.sorted.map(c => (c, getColumn(c)))
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
    for {
      rowk <- rowKeys
      colk <- columnKeys
    } {
      get(rowk, colk).foreach(f)
    }
  }

  def foreachLocation(f: (A, Int, Int) => Unit): Unit = {
    for {
      (rowk, rowi) <- rowKeys.zipWithIndex
      (colk, coli) <- columnKeys.zipWithIndex
    } get(rowk, colk).foreach(a => f(a, rowi, coli))
  }

  def modEach(f: A => A): Unit = {
    for {
      rowk <- rowKeys
      colk <- columnKeys
    } {
      modify(rowk, colk, f)
    }
  }

  def map[B](f: A => B): TabularData[RowT, ColT, B, RM, CM] = {
    val table2 = gcol.HashBasedTable.create[RowT, ColT, Any]()

    for {
      rowk <- rowKeys
      colk <- columnKeys
    } {
      get(rowk, colk).foreach { a =>
        if (a != null) table2.put(rowk, colk, f(a))
      }
    }

    new TabularData[RowT, ColT, B, RM, CM](table2, rowMarginals, colMarginals, axisLabels)
  }

  def mapColumns[B](z: => B)(f: (B, A) => B): Seq[(ColT, B)] = {
    for {
      colk <- columnKeys.toList.sorted
    } yield {
      val column = table.column(colk)
      val total = column.asScala.toSeq.foldLeft(z){ case  (acc, (rowk, a)) =>
        f(acc, a.asInstanceOf[A])
      }
      (colk, total)
    }
  }

  def mapRows[B](z: => B)(f: (B, A) => B): Seq[(RowT, B)] = {
    for {
      rowk <- rowKeys.toList.sorted
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

  import GuavaHelpers._


  def showBox(zero: String="")(implicit
    ShowA: Show[A],
  ): TB.Box = {

    val rowKeys = table.rowKeySet().asScala.toList.sorted
    val columnKeys = table.columnKeySet().asScala.toList.sorted

    val rows = (0 until rowKeys.length).map{ r =>
      mutable.ArrayBuffer.fill[String](columnKeys.length)(zero)
    }

    foreachLocation{ case (a, rown, coln) =>
      rows(rown)(coln) = ShowA.shows(a)
    }

    val cellBoxes = rows.map{ row =>
      row.map(_.box).toList.intersperse(" ┆ ")
    }

    val withRowMarginals = rowMarginals.map { rms =>
      cellBoxes.zip(
        rms.map{ case (rowk, rm) =>
          hjoin(" ┃ ", rm.toString().box)
        }
      ).map{ case (rowCells, rm) => rowCells :+ rm }
    } getOrElse{ cellBoxes }


    val withRowAndColMarginals: Seq[Seq[Box]] = colMarginals.map { cms =>
      val cmBoxes = cms.map(_._2.toString().box).toList.intersperse(" | ")

      withRowMarginals :+  cmBoxes
    } getOrElse{ withRowMarginals }


    val rowLabels = vspace(1) atop vjoins(right, rowKeys.map(_.toString().box))
    val colLabels  = columnKeys.map(_.toString().takeRight(8).mkString.box).intersperse(" ║ ") ++ List(" ┃ ".box, hspace(1))

    val colWise = switchRowsToCols(
      withRowAndColMarginals,
      emptyBox(1, 1)
    )

    val withColLabels = colLabels.zipAll(colWise, hspace(1), List()).map{ case (l, col) => l :: col }

    val cols = hjoins(top,
      withColLabels.map{ col =>
        vjoins(left, col)
      }
    )

    val (rowLabel, colLabel) = axisLabels.map{ axisLabel =>
      val leftLabel = vjoins(left, axisLabel.rowLabel.toList.map(_.toString().box))
      (leftLabel, axisLabel.colLabel.box)
    } getOrElse { (emptyBox(0, 0), emptyBox(0, 0)) }

    vjoinWith(left, vspace(1), List(
      colLabel,
      hjoinWith(top, nullBox, List(
        borderLeftRight("", " ┇ ")(rowLabel),
        hspace(1),
        hjoin(
          borderLeftRight("", " ┃ ")(rowLabels),
          cols
        )
      ))
    ))


  }


}
