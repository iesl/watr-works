package edu.umass.cs.iesl.watr
package labeling

import org.scalatest._

import geometry._
import textreflow.data._
import data._
import utils.EnrichNumerics._
import TypeTags._
import corpora._

class LabelWidgetIndexingSpec extends FlatSpec with Matchers with CorpusTestingUtil {
  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  import LabelWidgetIndex._

  val bbox = LTBounds(0d, 0d, 10d, 10d)
  val tr = TargetRegion(RegionID(0), DocumentID("doc-id-0"), PageNum(23), bbox)

  def stringToReflow(s: String): TextReflow = ???
    // stringToTextReflow(s)(DocumentID("doc-id-0"), PageNum(23))


  "LabelWidgetIndexing" should "have a test" in new FreshDocstore() {

    def reg0: TargetRegion = tr
    def sel0: TargetRegion = tr
    def sel1: TargetRegion = tr

    val reflow0 = stringToReflow("lime _{^{ﬂ}a}vor")
    def range0: RangeInt = RangeInt(1, 3)

    val w1 = LW.targetOverlay(reg0, List(
      LW.labeledTarget(sel0),
      LW.labeledTarget(sel1)
    ))

    val w2 = LW.col(
      LW.reflow(reflow0),
      LW.reflow(reflow0)
    )

    val row1 = LW.row(w1, w2)

    val panel1 = row1

    println("layout")
    println(prettyPrintLabelWidget(panel1))

    println("positioned")
    val abs0 = layoutWidgetPositions(panel1)

    // println(abs0.printTree())


    // val lwIndex = LabelWidgetIndex.create(panel1)
  }
}
