package edu.umass.cs.iesl.watr
package labeling

// import geometry._
// import textreflow.data._
import labeling.data._

import TypeTags._
import corpora._
import LabelWidgets._
// import LabelWidgetLayoutHelpers._
// import watrmarks.{StandardLabels => LB}
import scalaz.{@@ => _, _}, Scalaz._

sealed trait Mods
final case class AddLw(id: Int@@WidgetID) extends Mods
final case class RmLw(id: Int@@WidgetID) extends Mods

class LabelWidgetsSpec extends LabelWidgetTestUtil { // FlatSpec with Matchers with CorpusTestingUtil with LabelWidgetLayout {
  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  behavior of "label widgets"

  /**

    I'm not totally sure how to write these tests... So far they are really just
    glorified REPL interactions in testing blocks, so that I can step-debug and println
    my way through the results to verify them


    - Ideas for testing:
      - A small visual notation that expresses the desired layout, is easy to visually understand and check against



    */

  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._
  import matryoshka.patterns._
  // import matryoshka.patterns.EnvT


  // it should "create columns/rows" in new CleanDocstore {
  //   add4pg_3x3SampleDoc()

  //   val layout = col(
  //     row(pageDivs3(PageID(1)), pageDivs2(PageID(2))),
  //     row(pageDivs2(PageID(3)), pageDivs3(PageID(4)))
  //   )
  //   // println(prettyPrintLabelWidget(layout))

  //   val lwindex = LabelWidgetIndex.create(docStore, layout)

  //   val finalLayout = (
  //     """|111222
  //        |111
  //        |111222
  //        |333444
  //        |   444
  //        |333444
  //        |""")

  //   // lwindex.debugPrint()
  // }

  // it should "correctly position overlays" in new CleanDocstore  {
  //   add4pg_3x3SampleDoc()

  //   val pageId = PageID(3)
  //   val pageRegion = getRegionBounds(0, 0, 3, 3)
  //   val r0 = getRegionBounds(1, 1, 1, 3)
  //   val layout = targetOverlay(pageId, pageRegion, None, List(
  //     figure(r0)
  //   ))

  //   val lwindex = LabelWidgetIndex.create(docStore, layout)
  //   val finalLayout = (
  //     """|333
  //        |3α3
  //        |3α3
  //        | α
  //        |""")

  //   // lwindex.debugPrint()
  // }

  val page1 = PageID(1)
  val page2 = PageID(2)

  import LabelWidgetF._
  it should "compute a diff between 2 label widgets" in new CleanDocstore {
    val stableId = add4pg_3x3SampleDoc()
    val docId = docStore.getDocument(stableId).get

    // println(visualizeDocument(stableId))

    val overlayBox = figure(getRegionBounds(1, 1, 1, 2))
    val overlayBox2 = figure(getRegionBounds(0, 0, 1, 2))
    val overlayBox3 = figure(getRegionBounds(0, 0, 1, 3))

    val labelWidget1 = col(
      row(pageDiv1(page1, List(overlayBox, overlayBox3)))
    )

    def visit(lw: LabelWidgetT): LabelWidgetT = lw match {
      case l @ RegionOverlay(wid, p, g, c, os) =>
        l.copy(
          overs = List(os(0), overlayBox2, os(1))
        )
      case _ => lw
    }
    val labelWidget2 = labelWidget1.transCata[LabelWidget](visit)


    val lwindex1 = LabelWidgetIndex.create(docStore, labelWidget1)
    val lwindex2 = LabelWidgetIndex.create(docStore, labelWidget2)
    lwindex1.debugPrint()
    lwindex2.debugPrint()


    val lwDiff: Fix[Diff[Fix, LabelWidgetF, ?]] = labelWidget1.paraMerga(labelWidget2)(diff)
    val treeStr = lwDiff.cata(toTree).drawTree
    println(treeStr)
    // 1. Collect Diffs as Add/Remove
    import LabelWidgetF._

    // def positionAttrs: GAlgebra[(LabelWidget, ?), Diff[Fix, LabelWidgetF, ?], Seq[LabelWidget]] = fwa => {
    //   fwa match {
    //     case Same             (ident)        => Seq()
    //     case Similar          (ident)        =>  ident match {
    //       case Row(aAttrs) => aAttrs.flatMap(_._2)
    //       case Col(aAttrs) => aAttrs.flatMap(_._2)
    //       case RegionOverlay(p1, g1, c1, aAttrs) => aAttrs.flatMap(_._2)
    //     }
    //     case Different        (left, right)  => Seq()
    //     case LocallyDifferent (left, right)  => Seq()
    //     case Inserted         (right)        => Seq()
    //     case Deleted          (left)         => Seq()
    //     case Added            (right)        => Seq()
    //     case Removed          (left)         => Seq()
    //   }
    // }


    val qwer: Seq[Mods] = lwDiff.universe.toList
      .flatMap { lwd => lwd.project match {
        case Same             (ident)        => Seq()
        case Similar          (ident: LabelWidgetF[Fix[Diff[Fix, LabelWidgetF, ?]]])        => Seq() //  ident
        case Different        (left: LabelWidget, right: LabelWidget)  =>
          val toRm = left.universe // map {_.id}
          val toAdd = right.universe
          // toRm ++ toAdd
          Seq()
        case LocallyDifferent (left, right)  => Seq()
        case Inserted         (right)        => Seq()
        case Deleted          (left)         => Seq()
        case Added            (right)        => Seq()
        case Removed          (left)         => Seq()
      }
        // println(s"d> ${d}")
      }
    // import Diff._
    // val relativePositioned: Cofree[LabelWidgetF, PosAttr] = lwDiff.cata(attributePara(positionAttrs))
    // val asdf = attributePara(positionAttrs)
    // lwDiff.cata()
    // 2. Run layout on new label widget
    // 3. filter Positioning to add/remove set

    /// Use of diffs for labeling:
    // - When a widget is clicked (or selected)
    //   - run the _.cata(..) via interaction to make changes to the label widget
    //   - do a diff of the resulting widget against the original
    //   - reduce the diff to a data structure suitable to send to the UI,
    //     which contains visual updates and client-state changes


    // On click, add selection indicator



  }


  // it should "include padding" in new CleanDocstore {
  //   add4pg_3x3SampleDoc()
  // }

  // it should "include inserted text (as textbox)" in new CleanDocstore {
  // }

  // it should "include reflows" in new CleanDocstore {
  // }

  // it should "include labeled targets as overlays" in new CleanDocstore {
  //   val stableId = add4pg_3x3SampleDoc()
  //   val docId = docStore.addDocument(stableId)
  //   val pageId = docStore.addPage(docId, PageNum(0))

  //   val pageRegion = mkTargetRegionDbl(pageId, 5d, 4d, 3d, 2d)
  //   val pageRegion2 = mkTargetRegionDbl(pageId, 5.1d, 4.1d, 3.1d, 2.1d)

  //   val subRegion = mkTargetRegionDbl(pageId, 5.5d, 4.5d, 0.5d, 1.5d)
  //   val subRegion2 = mkTargetRegionDbl(pageId, 5.6d, 3.5d, 0.1d, 2.5d)

  //   val widget0 = col(
  //     targetOverlay(pageRegion, List(labeledTarget(subRegion), labeledTarget(subRegion2))),
  //     targetOverlay(pageRegion2, List(labeledTarget(subRegion), labeledTarget(subRegion2)))
  //   )


  //   val widgetLayout = layoutWidgetPositions(widget0)

  //   widgetLayout.positioning
  //     .foreach{ pos =>
  //       println(s"${pos}")
  //       val clipTo = LTBounds(1d, 0d, 1d, 100d)
  //       val clipped = clipPageRegionFromWidgetSpace(pos, clipTo)
  //       println(s"  clipped: ${clipped}")

  //     }

  // }



  // it should "rewrite widgets to include geometric figure overlays for prior labeling" in new CleanDocstore {
  //   add4pg_3x3SampleDoc()

  //   // Create a zone labeling over a few pages
  //   // val stableId = docStore.getDocuments().head
  //   // val docId = docStore.getDocument(stableId).get


  //   // // Create a zone that spans 2 pages
  //   // val page0Lines = docStore.getPageVisualLines(stableId, PageNum(0))
  //   // val page1Lines = docStore.getPageVisualLines(stableId, PageNum(1))
  //   // val lineZoneIds = (page0Lines ++ page1Lines).map(_.getId())
  //   // val newZone = docStore.createZone(lineZoneIds)
  //   // docStore.addZoneLabel(newZone, LB.Authors)



  //   // // Create a labeling widget
  //   // val layout = col(246]

  //   //   row(pageDivs3(1), pageDivs2(2))
  //   // )

  //   // val layout1 = LabelWidgetTransforms.addZoneIndicators(LB.Authors, layout, docStore)

  //   // val lwindex = LabelWidgetIndex.create(docStore, layout1)

  //   // // prove that a widget w/ either of those pages includes the labeled overlay
  //   // lwindex.layout.positioning.foreach { pos =>
  //   //   println(s"${pos}")
  //   // }
  // }



}
