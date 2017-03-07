package edu.umass.cs.iesl.watr
package labeling

import scala.collection.mutable

import textreflow.data._
import geometry._
import LabelWidgetF._
import corpora._
import rindex._

// import TypeTags._
// import spindex._
// import docstore._
// import watrmarks.{StandardLabels => LB}
// import watrmarks._

// Provide a caching wrapper around TextReflow + precomputed page bbox
// Only valid for TextReflow that occupy a single Bbox (e.g., VisualLine)
case class IndexableTextReflow(
  id: Int@@TextReflowID,
  textReflow: TextReflow,
  pageRegion: PageRegion
)

object LabelWidgetIndex extends LabelWidgetLayout {

  implicit object TextReflowIndexable extends SpatialIndexable[IndexableTextReflow] {
    def id(t: IndexableTextReflow): Int = t.id.unwrap
    def ltBounds(t: IndexableTextReflow): LTBounds = t.pageRegion.bbox
  }

  implicit object LabelWidgetIndexable extends SpatialIndexable[WidgetPositioning] {
    def id(t: WidgetPositioning): Int = t.id.unwrap
    def ltBounds(t: WidgetPositioning): LTBounds = t.widgetBounds
  }

  import textreflow.TextReflowJsonCodecs._

  def create(docStore0: DocumentCorpus, lwidget: LabelWidget): LabelWidgetIndex = {
    val lwIndex = SpatialIndex.createFor[WidgetPositioning]()

    val layout0 = layoutWidgetPositions(lwidget)

    layout0.positioning.foreach({pos =>
      lwIndex.add(pos)
    })

    val targetPageRIndexes = mutable.HashMap[Int@@PageID, SpatialIndex[IndexableTextReflow]]()

    layout0.positioning.foreach({pos => pos.widget match {


      case l @ TargetOverlay(under, overs) =>
        val pageId = under.pageId
        // under.regionId
        // under.bbox
        if (!targetPageRIndexes.contains(pageId)) {
          val pageIndex = SpatialIndex.createFor[IndexableTextReflow]()
          targetPageRIndexes.put(pageId, pageIndex)

          for {
            vline <- docStore0.getPageVisualLines(pageId)
            reflow <- docStore0.getModelTextReflowForZone(vline.id)
          } {
            val textReflow = jsonStrToTextReflow(reflow.reflow)
            val indexable = IndexableTextReflow(
              reflow.prKey,
              textReflow,
              PageRegion(
                pageId,
                textReflow.targetRegion.bbox
              )
            )
            pageIndex.add(indexable)
          }
        }

      case _ =>


    }})


    new LabelWidgetIndex {
      def docStore: DocumentCorpus = docStore0
      def layout: WidgetLayout = layout0
      def index: SpatialIndex[WidgetPositioning] = lwIndex
      def pageIndexes: Map[Int@@PageID, SpatialIndex[IndexableTextReflow]] = targetPageRIndexes.toMap
    }
  }
}


trait LabelWidgetIndex {

  def docStore: DocumentCorpus
  def layout: WidgetLayout
  def index: SpatialIndex[WidgetPositioning]
  def pageIndexes: Map[Int@@PageID, SpatialIndex[IndexableTextReflow]]


  def querySelected(bbox: LTBounds): Seq[LabeledTarget] = {
    val positioned: Seq[WidgetPositioning] = index.queryForIntersects(bbox)

    // val selectedTargets: List[(WidgetPositioning, LabeledTarget)]
    val selectedTargets: List[LabeledTarget] = positioned.toList
      .map(p => index.getItem(p.id.unwrap))
      .filter(_.widget.isInstanceOf[LabeledTarget])
      .map(p => p.widget.asInstanceOf[LabeledTarget])

    selectedTargets
  }

  def select(bbox: LTBounds, constraint: Constraint): Seq[Seq[PageRegion]] = {
    val hits: Seq[WidgetPositioning] =
      index.queryForIntersects(bbox)
        .filter(_.widget.isInstanceOf[TargetOverlay[Unit]])

    println(s"hits within ${bbox}")
    val res = hits.mkString("\n  ", "\n  ", "\n")
    println(res)


    Seq()
  }

  def queryForSelectedLines(bbox: LTBounds): Seq[Zone] = {
    // val visualLineZones: Seq[Zone] = for {
    //   selectedTargets <- querySelected(bbox)
    //   labeledTarget <- selectedTargets
    //   zoneId <- docStore.getZoneForTargetRegion(labeledTarget.target.id, LB.VisualLine)
    // } yield { docStore.getZone(zoneId) }
    // visualLineZones
    ???
  }

  def constrainedClipTargetRegions(bbox: LTBounds, constraint: Constraint, targets: Seq[TargetRegion]): Seq[TargetRegion] = {
    constraint match {
      case Constraint.ByLine =>
        // apply label to all lines in selected region


      case Constraint.ByChar =>
      case Constraint.ByRegion =>
        // apply label to region w/o regard to line/chars
    }

    ???
  }




  def runUIRequest(r: UIRequest): UIResponse = {
    val UIRequest(uiState, gesture) = r

    gesture match {
      case SelectRegion(bbox) =>

        // val positioned: Seq[WidgetPositioning] = index.queryForIntersects(bbox)

        val selectedLines = queryForSelectedLines(bbox)
        val selectedTargetLines = selectedLines.flatMap(_.regions)

        val clippedTargets = constrainedClipTargetRegions(bbox, uiState.selectionConstraint, selectedTargetLines)

        if (selectedLines.nonEmpty) {
          uiState.action match {
            case Create =>
              uiState.selectionConstraint match {
                case Constraint.ByLine =>
                  // apply label to all lines in selected region

                case Constraint.ByChar =>
                case Constraint.ByRegion =>
                  // apply label to region w/o regard to line/chars
              }

              def mergedZone: Zone =  ???

              // docStore.getZone(docStore.mergeZones(existingZones.map(_.id)))

              // Add all target regions to merged zone
              // selectedTargets.map(tr => docStore.setZoneTargetRegions(
              //   mergedZone.id,
              //   mergedZone.regions :+ tr.target
              // ))
              // Option(mergedZone)

              // // Create a new Zone with given label
              // val stableId = selectedTargets.head.target.stableId
              // val docId = docStore
              //   .getDocument(stableId)
              //   .getOrElse(sys.error(s"onSelect() document ${stableId} not found"))

              // val targetRegions = selectedTargets.map(_.target)
              // val newZone = docStore.getZone(
              //   docStore.createZone(docId)
              // )
              // docStore.setZoneTargetRegions(newZone.id, targetRegions)
              // docStore.addZoneLabel(newZone.id, targetLabel)

            case Delete =>
          }
        }

    }



    // response: indicate(region, )

    UIResponse(List())
  }

}

// def getWidgetForTargetRegion(targetRegion: TargetRegion): WidgetPositioning = {
//   val stableId = targetRegion.stableId
//   val docId = docStore.getDocument(stableId).get
//   val pageId = docStore.getPage(docId, targetRegion.pageNum).get
//   // Map TargetRegion -> WidgetPositioning
//   layout
//     .collect({
//       case p @ WidgetPositioning(
//         LabeledTarget(bbox, label, score),
//         widgetBounds,
//         pRegionId, _, _
//       ) if targetRegion.id == targetRegion.id => p
//     }).headOption
//     .getOrElse(sys.error(s"getWidgetForTargetRegion: no entry for ${targetRegion}"))
//   ???
// }
