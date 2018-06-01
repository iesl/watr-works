package edu.umass.cs.iesl.watr
package textgrid

import geometry._

import TypeTags._
import LabeledSequenceTreeTransforms._

import _root_.io.circe
import circe.syntax._

// import utils.DoOrDieHandlers._
import LabeledSequenceCodecs._
import TextGridLabelWidget._

class TextGraphCodecTests extends TextGraphSpec {

  val dummyPageRegion = PageRegion(
    StablePage(DocumentID("docX"), PageNum(0)), LTBounds.empty
  )


  "Behavior of labeled TextGraph serialization to/from Json" in {
    import TextGraphJvm._

    val textGraph = makeBishopClarkSample()
    val asJson = textGraph.asJson
    val roundTripGraph = TextGraph.fromJson(asJson)
    val rtJson = roundTripGraph.asJson

    println("Json Format")
    println(asJson.pretty(JsonPrettyPrinter))

    // val indentedBlock = textGraphToIndentedBox(roundTripGraph)
    // val labelTree = textGraphToLabelTree(roundTripGraph)
    // val expMarginals = labelTreeToMarginals(labelTree, compactMarginals=false)
    // val emarginBlock = marginalGlossToTextBlock(expMarginals)
    // val expBlock = emarginBlock + indentedBlock
    // println("Block Format ")
    // println(expBlock.toString())
    // val cmpare = asJson.toString().box besideS rtJson.toString().box
    // println("\n\n\n----------------------------------------------")
    // println(cmpare)
    // println("========================================================")
    assert(asJson.toString() === rtJson.toString())
  }
}
