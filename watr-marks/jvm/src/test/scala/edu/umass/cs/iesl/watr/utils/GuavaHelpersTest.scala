package edu.umass.cs.iesl.watr
package utils

import org.scalatest._
import scala.util.Random._
import textboxing.{TextBoxing => TB}, TB._
import GuavaHelpers._
import com.google.{common => guava}, guava.{collect => gcol}
// import scala.collection.JavaConverters._

class GuavaHelpersTest extends FlatSpec with Matchers {

  it should "draw a table" in {
    val table = gcol.HashBasedTable.create[Int, String, String]()
    for {
      row <- (0 until 5)
      col <- (0 until 5)
    } yield {
      if (nextBoolean()) {
        table.put(row, col.toString(), s"${row}, ${col}" )
      }
    }

    val tableDef = guavaTableToMatrix(table, "--")
    val grid = tableToGrid(tableDef)

    println(grid.toBox())

  }

  it should "draw a marginalized table" in {
    val table = gcol.HashBasedTable.create[Int, String, Int]()
    for {
      row <- (0 until 5)
      col <- (0 until 5)
    } yield {
      if (nextBoolean()) {
        table.put(row, col.toString(), row*col)
      }
    }

    val add = (a:Int, b:Int) => a + b

    val box = guavaTableToLabeledBox(table, 0,
      "", "",
      add, add
    )
    println(box.toString())
  }
}
