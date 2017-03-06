package edu.umass.cs.iesl.watr
package labeling

import geometry._
// import spindex._
// import docstore._
import LabelWidgetF._
import corpora._
import rindex._
// import watrmarks.{StandardLabels => LB}
// import watrmarks._

object LabelWidgetIndex extends LabelWidgetLayout {

  implicit object LabelWidgetIndexable extends SpatialIndexable[WidgetPositioning] {
    def id(t: WidgetPositioning): Int = t.id.unwrap
    def ltBounds(t: WidgetPositioning): LTBounds = t.widgetBounds
  }

  def create(docStore0: DocumentCorpus, lwidget: LabelWidget): LabelWidgetIndex = {
    val lwIndex = SpatialIndex.createFor[WidgetPositioning]()

    val layout0 = layoutWidgetPositions(lwidget)

    layout0.positioning.foreach({pos =>
      lwIndex.add(pos)
    })

    new LabelWidgetIndex {
      def docStore: DocumentCorpus = docStore0
      def layout: WidgetLayout = layout0
      def index: SpatialIndex[WidgetPositioning] = lwIndex
    }
  }
}


trait LabelWidgetIndex {

  def docStore: DocumentCorpus
  def layout: WidgetLayout
  def index: SpatialIndex[WidgetPositioning]


  def querySelected(bbox: LTBounds): Seq[LabeledTarget] = {
    val positioned: Seq[WidgetPositioning] = index.queryForIntersects(bbox)

    // val selectedTargets: List[(WidgetPositioning, LabeledTarget)]
    val selectedTargets: List[LabeledTarget] = positioned.toList
      .map(p => index.getItem(p.id.unwrap))
      .filter(_.widget.isInstanceOf[LabeledTarget])
      .map(p => p.widget.asInstanceOf[LabeledTarget])

    selectedTargets
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
      case ByLine =>
        // apply label to all lines in selected region


      case ByChar =>
      case ByRegion =>
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
                case ByLine =>
                  // apply label to all lines in selected region


                case ByChar =>
                case ByRegion =>
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
