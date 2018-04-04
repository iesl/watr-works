package edu.umass.cs.iesl.watr
package utils

import com.google.{common => guava}
import guava.{collect => gcol}
import textboxing.{TextBoxing => TB}, TB._
import scalaz.{@@ => _, Ordering => _, _}, Scalaz._

object GuavaHelpers {
  import scala.collection.JavaConverters._
  import scala.collection.mutable

  case class GuavaTableMatrix[X, Y, A](
    rowKeys: List[X],
    colKeys: List[Y],
    rows: Seq[Seq[A]]
  )

  def guavaTableToLabeledBox[A: Ordering, B: Ordering, C](
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

  def guavaTableToBox[A: Ordering, B: Ordering, C](
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
      // boxedTable :+ sums.map(_ => "-".box).intersperse(" - ".box) // .map(x => TB.borderTB("=")(x))
      boxedTable :+ sums.intersperse(" = ") // .map(x => TB.borderTB("=")(x))
    } getOrElse { boxedTable }


    val rowLabels = vspace(1) atop vjoins(right, rowKeys.map(_.toString().box))
    val colLabels  = columnKeys.map(_.toString().box).intersperse(" ║ ")

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
      // rowLabels,
      // vjoins(left, rowBoxes),
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





  def guavaTableToBoxOld[A: Ordering, B: Ordering, C](
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
      row.map(_.toString().box).toList.intersperse(" ║ ")
    }

    boxedTable = rightMarginalFunc.map{ f =>
      val sums = rowKeys.map{ k =>
        val row = table.row(k).asScala.values.toList
        List(" = ".box, row.reduce(f).toString.box)
      }
      boxedTable.zip(sums).map { case (r, s) => r ++ s }
    } getOrElse { boxedTable }

    boxedTable = bottomMarginalFunc.map{ f =>
      val sums = columnKeys.map{ k =>
        val col = table.column(k).asScala.values.toList
        col.reduce(f).toString.box
      }
      boxedTable :+ sums
    } getOrElse { boxedTable }


    val rowLabels = rowKeys.map(_.toString().box)
    val colLabels  = "  ".box :: " ║ ".box :: (columnKeys.map(_.toString().box).intersperse(" ║ "))

    boxedTable = colLabels +: boxedTable

    val bottomMarginals = bottomMarginalFunc.map{ f =>
        columnKeys.map{ k =>
          val col = table.column(k).asScala.values.toList
          col.reduce(f).toString.box
        }
    } getOrElse {
      columnKeys.map(_ => emptyBox(1, 1))
    }

    val rightMarginals = List.fill(1)("?".box) ++ (rightMarginalFunc.map{ f =>
        rowKeys.map{ k =>
          val row = table.row(k).asScala.values.toList
          " = ".box + row.reduce(f).toString.box
        }
    } getOrElse {
      rowKeys.map(_ => emptyBox(1, 1))
    })


    val grid = Grid.withCols(rowKeys.length, TB.AlignRight)
      .addRow(colLabels:_*)

    val filledGrid =
      rows.zip(rowLabels).zip(rightMarginals)
        .foldLeft(grid){ case (accGrid, ((row, rowLabel), rightMarginal)) =>
          val colBoxes = row.map(c => c.toString().mbox).toList.intersperse(" ┆ ".box)
          accGrid.addRow((rowLabel:: " ┃ ".box :: (colBoxes :+ rightMarginal)):_*)
        }

    // vjoin(TB.AlignLeft,
    //   // hjoins(TB.top, colLabels),
    // )
    filledGrid.addRow(bottomMarginals:_*).toBox()


  }









  def guavaTableToMatrix[A: Ordering, B: Ordering, C](
    table: gcol.Table[A, B, C], zero: C,
    topLabel: Option[String] = None,
    leftLabel: Option[String] = None,
    rightMarginalFunc: Option[(C, C) => C] = None,
    bottomMarginalFunc: Option[(C, C) => C] = None,
  ): GuavaTableMatrix[A, B, C] = {
    val rowKeys = table.rowKeySet().asScala.toList.sorted
    val columnKeys = table.columnKeySet().asScala.toList.sorted
    val matrix = (0 until rowKeys.length).map{ r =>
      mutable.ArrayBuffer.fill[C](columnKeys.length)(zero)
    }


    for {
      (rowk, rown) <- rowKeys.zipWithIndex
      (colk, coln) <- columnKeys.zipWithIndex
    }  {
      val c = table.get(rowk, colk)
      if (c!=null) {
        matrix(rown)(coln) = c
      }
    }
    GuavaTableMatrix(rowKeys, columnKeys, matrix)

  }

  import Grid._

  def tableToGrid[A, B, C](table: GuavaTableMatrix[A, B, C]): TB.Grid = {

    val rowLabels = table.rowKeys.map(_.toString().box)
    val colLabels  = "  ".box :: " ║ ".box :: (table.colKeys.map(_.toString().box).intersperse(" ║ "))

    // val headerSep = List.fill(colLabels.length)("-".box)

    val grid = Grid.withCols(table.rowKeys.length, TB.AlignLeft)
      .addRow(colLabels:_*)

    table.rows.zip(rowLabels).foldLeft(grid){ case (accGrid, (row, rowLabel)) =>
      val colBoxes = row.map(c => c.toString().mbox).toList.intersperse(" ┆ ".box)
      accGrid.addRow((rowLabel:: " ┃ ".box :: colBoxes):_*)
    }

  }

}
