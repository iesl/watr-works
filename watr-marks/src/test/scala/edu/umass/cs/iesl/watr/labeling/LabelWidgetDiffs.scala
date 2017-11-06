
package edu.umass.cs.iesl.watr
package labeling

// import geometry._
// import TypeTags._
// import LabelWidgets._
import corpora._

class LabelWidgetDiffsSpec extends LabelWidgetTestUtil {
  def createEmptyDocumentZoningApi(): DocumentZoningApi = new MemDocZoningApi
  // it should "compute a diff between 2 label widgets" in new CleanDocstore {
  //   val stableId = add4pg_3x3SampleDoc()
  //   val docId = docStore.getDocument(stableId).get

  //   // println(visualizeDocument(stableId))

  //   val overlayBox = figure(getRegionBounds(1, 1, 1, 2))
  //   val overlayBox2 = figure(getRegionBounds(0, 0, 1, 2))
  //   val overlayBox3 = figure(getRegionBounds(0, 0, 1, 3))

  //   val labelWidget1 = col(
  //     row(pageDiv1(page(1), List(overlayBox, overlayBox3)))
  //   )

  //   def visit(lw: LabelWidgetT): LabelWidgetT = lw match {
  //     case l @ RegionOverlay(wid, under, overlays) =>
  //       l.copy(
  //         overlays = List(overlays(0), overlayBox2, overlays(1))
  //       )
  //     case _ => lw
  //   }

  //   val labelWidget2 = labelWidget1.transCata[LabelWidget](visit)

  //   val lwindex1 = LabelWidgetIndex.create(docStore, labelWidget1)
  //   val lwindex2 = LabelWidgetIndex.create(docStore, labelWidget2)

  //   // lwindex2.debugPrint()

  //   val lwDiff = labelWidgetDiff(labelWidget1, labelWidget2)

  //   println(drawLabelWidgetDiff(lwDiff))

  //   val allMods = labelWidgetDiffToMods(lwDiff)

  //   val str = allMods.mkString("\n  ", "\n  ", "\n")
  //   // println(str)

  // }
}
