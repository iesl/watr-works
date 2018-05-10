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
// import textboxing.{TextBoxing => TB}, TB._
// import utils.{Cursor, Cursors, Window}


class TextGridCodecTests extends TextGridSpec {

  val dummyPageRegion = PageRegion(
    StablePage(DocumentID("docX"), PageNum(0)), LTBounds.empty
  )





  // "Behavior of labeled TextGrid serialization to/from Json" in {

  //   val textGrid = makeBishopClarkTextGrid()
  //   val asJson = textGrid.toJson
  //   val roundTripGrid = TextGrid.fromJson(asJson)
  //   val rtJson = roundTripGrid.toJson()

  //   println("Json Format")
  //   println(asJson.pretty(JsonPrettyPrinter))

  //   val indentedBlock = textGridToIndentedBox(roundTripGrid)
  //   val labelTree = textGridToLabelTree(roundTripGrid)
  //   val expMarginals = labelTreeToMarginals(labelTree, compactMarginals=false)
  //   val emarginBlock = marginalGlossToTextBlock(expMarginals)
  //   val expBlock = emarginBlock + indentedBlock
  //   println("Block Format ")
  //   println(expBlock.toString())
  //   // val cmpare = asJson.toString().box besideS rtJson.toString().box
  //   // println("\n\n\n----------------------------------------------")
  //   // println(cmpare)
  //   // println("========================================================")
  //   assert(asJson.toString() === rtJson.toString())
  // }
}
