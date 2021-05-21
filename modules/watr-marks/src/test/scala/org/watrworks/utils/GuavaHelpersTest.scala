package org.watrworks
package utils

import scala.util.Random._
import GuavaHelpers._
import scalaz._, Scalaz._

class GuavaHelpersTest extends WatrSpec {

  it should "draw a marginalized table" in {
    val table = initTable[Int, String, String]()
    for {
      row <- (0 until 5)
      col <- (0 until 4)
    } yield {
      if (nextBoolean()) {
        table.set(row, s"col#${col}", s"r${row},c${col}")
      }
    }


    println("done..")
    val finalTable = table
      .computeColMarginals(0)((acc, e) => acc + 1)
      .computeRowMarginals(0)((acc, e) => acc + 1)
      .addTopLabel("Top Label")
      .addLeftLabel("L-Axis")
      // .addLabels("Pages", "Whatevs")

    println("\nShowBox()")
    println(finalTable.showBox().toString())

  }
}
