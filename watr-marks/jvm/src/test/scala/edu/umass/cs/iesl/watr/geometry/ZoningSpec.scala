package edu.umass.cs.iesl.watr
package geometry

import org.scalatest._

import TypeTags._
import watrmarks.{StandardLabels => LB}

class ZoningSpec extends FlatSpec with Matchers {

  import ZoneTrees._

  behavior of "zone trees"

  it should "construct a zone" in   {
    val pageRegion = PageRegion(PageID(1), LTBounds(5d, 4d, 3d, 2d) )

    val l0 = leaf(pageRegion)
    val l01 = role(LB.VisualLine, l0)
    val l02 = ref(ZoneID(32), l0)
    val l03 = role(LB.Authors, l02)
    val n01 = node(List(
      l0, l01, l02
    ))
    val n011 = ref(ZoneID(22), role(LB.Title, n01))

    println(prettyPrintTree(n011))

  }

}
