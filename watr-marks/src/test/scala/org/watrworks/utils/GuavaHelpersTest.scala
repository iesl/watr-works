package org.watrworks
package utils

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random._
import GuavaHelpers._
import scalaz._, Scalaz._

class GuavaHelpersTest extends AnyFlatSpec with Matchers {

  it should "draw a marginalized table" in {
    val table = initTable[Int, String, Int]()
    // val table = gcol.HashBasedTable.create[Int, String, Int]()
    for {
      row <- (0 until 5)
      col <- (0 until 5)
    } yield {
      if (nextBoolean()) {
        table.set(row, col.toString(), row*col)
      }
    }

    val finalTable = table
      .computeColMarginals(1)(_ * _)
      .computeRowMarginals(0)(_ + _)
      .addLabels("Pages", "Whatevs")

    println(finalTable.showBox("?"))
  }
}
