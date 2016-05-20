package edu.umass.cs.iesl
package watr
package docseg

import java.io.InputStream
import org.scalatest._

import watrmarks._
import ext._
import scalaz.@@


object page {
  val page = (0 to 10).map(PageID(_))
  def apply(i:Int) = page(i)
}

class TextlineSegTest extends FlatSpec with Matchers {
  behavior of "docstrum segmenter"

  val LB = watrmarks.StandardLabels



}
