package edu.umass.cs.iesl.watr
package formats

import textgrid._
import org.scalatest._
import TextGridLabelWidget._
import utils.ScalazTreeImplicits._

trait TextGridSpec extends FreeSpec with Matchers with TextGridTestExamples

class TextGridOutputFormatsTest extends TextGridSpec {


  "Format of TextGrid serialization" in {
    val textGrid = makeBishopClarkTextGrid()
    val labelTree = textGridToLabelTree(textGrid)
    println(labelTree.drawBox)
    val spanTree = labelTreeToSpanTree(labelTree)
    val offsetTrees = spanTree.drawBox
    println()
    println(offsetTrees)
    println()
    val indentedBlock = textGridToIndentedBox(textGrid)
    println(indentedBlock)
    val bioJson = spanTreeToJson(spanTree)
    println(
      bioJson.pretty(JsonPrettyPrinter)
    )
  }

  "Behavior of labeled TextGrid serialization" in {
    val textGrid = makeBishopClarkTextGrid()
    val asJson = textGrid.toJson // .pretty(jsonPrinter)
    val roundTripGrid = TextGrid.fromJson(asJson)
    val rtJson = roundTripGrid.toJson()
    rtJson.pretty(JsonPrettyPrinter)

    val indentedBlock = textGridToIndentedBox(roundTripGrid)
    // println(asJson.noSpaces)
    // println(indentedBlock)
    // val labelTree = textGridToLabelTree(roundTripGrid)
    // val expMarginals = labelTreeToMarginals(labelTree, compactMarginals=false)
    // val emarginBlock = marginalGlossToTextBlock(expMarginals)
    // val expBlock = emarginBlock + indentedBlock
    // println("post: ")
    // println(expBlock.toString())
    // val cmpare = asJson.toString().mbox besideS rtJson.toString().mbox
    // println("\n\n\n----------------------------------------------")
    // println(cmpare)
    // println("========================================================")
    // assert(asJson.toString() === rtJson.toString())
  }

}
