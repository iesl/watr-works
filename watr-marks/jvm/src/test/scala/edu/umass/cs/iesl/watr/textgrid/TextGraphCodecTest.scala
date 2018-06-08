package edu.umass.cs.iesl.watr
package textgrid

import geometry._

import TypeTags._
import LabeledSequenceTreeTransforms._

import _root_.io.circe, circe._
import circe.syntax._

import utils.DoOrDieHandlers._
import LabeledSequenceCodecs._
import TextGridLabelWidget._
import textboxing.{TextBoxing => TB}, TB._

class TextGraphCodecTests extends TextGraphSpec {

  val dummyPageRegion = PageRegion(
    StablePage(DocumentID("docX"), PageNum(0)), LTBounds.empty
  )


  "Behavior of labeled TextGraph serialization to/from Json" in {
    import TextGraphJvm._

    val textGraph = makeBishopClarkSample()
    val asJson = textGraph.asJson
    val roundTripGraph = asJson.decodeOrDie[TextGraphJvm]()
    val rtJson = roundTripGraph.asJson

    // val cmpare = asJson.toString().box besideS rtJson.toString().box
    // println("\n\n\n----------------------------------------------")
    // println(cmpare)
    // println("========================================================")

    assert(asJson.toString() === rtJson.toString())
  }
}
