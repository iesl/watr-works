package edu.umass.cs.iesl.watr
package extract

import org.scalatest._

import watrmarks._
import scalaz._
import Scalaz._
import spatial._

class DocumentFeatureTest extends FlatSpec {

  behavior of "zone iterator"
  val LB = StandardLabels

  def is = getClass().getResourceAsStream("/spatial/0575.pdf.cermine-zones.json")
  def loadPageIterator = ZoneIterator.load(is)

  it should "load info from json" in new SpatialJsonFormat {
    ZoneIterator.load(is).isDefined
  }


  it should "read in zone desc. from json"  in {

    val pageStream = unfold[Option[PageIterator], PageIterator](
      loadPageIterator
    )(_ match {
      case Some(cur) => Some((cur, cur.nextPage))
      case None => None
    })

    pageStream.foreach { pageIter =>
      val zones = pageIter.getZones(LB.Block)
      zones.foreach { zone =>
        val bboxes = zone.getBoundingBoxes.map(_.bbox.prettyPrint).mkString(", ")
        // println(s"zone = ${zone.getText}   bbox = ${bboxes}")
        println(s"zone = ${zone.getText} ")
        val textDensity = DocumentFeatures.textDensity(zone)
        println(s"    textDensity = ${textDensity}")

        // zone.getTokens.map {case (tokenZone, tokenLabel) =>
        //   println(s"${tokenLabel}:  ${tokenZone.bboxes.map(_.bbox.prettyPrint)}")
        // }
      }

    }
  }
}
