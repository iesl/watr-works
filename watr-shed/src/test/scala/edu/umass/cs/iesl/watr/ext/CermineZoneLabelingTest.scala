package edu.umass.cs.iesl.watr
package ext

import org.scalatest._

class CermineZoneLabelingTest extends FlatSpec {

  behavior of "cermine zone labeling"

  it should "serialize cermine-derived zone/line/word parsing to SVG" in {

    val asSVG = cermineZoneUtil.cermineZonesToSVG("abc123sha", papers.`6376.pdf`)
    println(asSVG)

  }


}
