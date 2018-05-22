package edu.umass.cs.iesl.watr
package textgrid

import TypeTags._
import textboxing.{TextBoxing => TB}, TB._
import org.scalatest._


trait TextGraphSpec extends FreeSpec with Matchers with TextGraphConstruction {

  def infobox(heading: String, b: TB.Box): Unit = {
    info(heading)
    info("\n" + indent(4, b).toString() + "\n")
  }

}
class TextGraphTests extends TextGraphSpec {


  val stableId = DocumentID("SampleDocument")

  info("behavior of textgraph char-level labeling")
  info("behavior of textgraph row-level labeling")
  info("behavior of textgraph block-level labeling")

  val sampleDoc = "abc\n  def\ngh i "


  "construct basic graph" in {
    val textGraph = TextGraphBuilder.create()
    val cells = stringToTextGraphCells(stableId, sampleDoc, PageNum(3))

    cells.foreach { row =>
      textGraph.appendRow(row)
    }

    println(textGraph.toText())
  }

  "apply labels" in {
    val textGraph = TextGraphBuilder.create()
  }

  // val thingCount = 10

  // val things = unlabeledThings(thingCount)

  // things.addBioLabel(Journal)
  // things.addBioLabel(Author)



  // return every labeled span of text within graph

  // it should "support alphabet soup regions" in {}
  // it should "split/join lines" in {}


  // info("behavior of label sequence labeling/unlabeling")
  // "application of labels" in {
  // info("removing labels")
  // "it should find the span of cells that have a particular label" in {
  // "it should find the span of cells that have identical labeling (including unlabeled spans)" in {


}
