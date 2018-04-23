package edu.umass.cs.iesl.watr
package utils

import org.scalatest._
import scala.util.Random._
// import textboxing.{TextBoxing => TB}, TB._
import GuavaHelpers._
// import com.google.{common => guava}, guava.{collect => gcol}
// import scala.collection.JavaConverters._
import scalaz._, Scalaz._

class GuavaHelpersTest extends FlatSpec with Matchers {

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
