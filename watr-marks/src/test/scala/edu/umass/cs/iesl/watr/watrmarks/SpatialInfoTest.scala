package edu.umass.cs.iesl.watr
package watrmarks

import org.scalatest._

import play.api.libs.json
import json._

class SpatialInfoTest extends FlatSpec {

  behavior of "spatial info"

  it should "parse zones" in new  SpatialJsonFormat {
    // [ 22787, [ [ "page:18999", "301.72", "525.98", "42.85", "7.97" ] ] ],
    val targetedBoundsJson = (
      """|{  "id" : 6130,
         |   "target" : 7,
         |   "bbox" : {
         |     "left" : 407.6588134765625,
         |     "top" : 752.8803634643555,
         |     "width" : 107.7562255859375,
         |     "height" : 10.019996643066406
         |}}
         |""".stripMargin)
    val jsvalue = json.Json.parse(targetedBoundsJson)

    val tb = jsvalue.as[TargetedBounds]

    val zoneExample =(
      s"""|{  "id" : 6130,
          |   "bboxes": [${targetedBoundsJson}]
          |}
          |""".stripMargin

    )
    Json.parse(zoneExample).as[Zone]

    val pageGeometry1 = (
      """|{ "id" : 0,
         |  "bounds" : {
         |    "left" : 0,
         |    "top" : 793.70,
         |    "width" : 595.276,
         |    "height" : 793.70
         |  },
         |  "borders" : {
         |    "bleft" : 0,
         |    "btop" : 793.70,
         |    "bbottom" : 793.70,
         |    "bright" : 595.27
         |  }
         |}
         |""".stripMargin)

    Json.parse(pageGeometry1).as[PageGeometry]

  }



  it should "load info from json" in new SpatialJsonFormat {
    val is = getClass().getResourceAsStream("/spatial/spinfo0.json")
    val jsvalue = json.Json.parse(is).as[ZoneRecords]
    val pspinfo = ZoneIndexer.loadSpatialIndices(jsvalue)
  }
}


