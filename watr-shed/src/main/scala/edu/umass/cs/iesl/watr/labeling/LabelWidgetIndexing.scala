package edu.umass.cs.iesl.watr
package labeling

import geometry._
// import spindex._
import docstore._
import LabelWidgetF._
import corpora._
import rindex._
import watrmarks.{StandardLabels => LB}
import watrmarks._


object LabelWidgetIndex extends LabelWidgetLayout {

  implicit object LabelWidgetIndexable extends SpatialIndexable[PosAttr] {
    def id(t: PosAttr): Int = t.id.unwrap
    def ltBounds(t: PosAttr): LTBounds = t.widgetBounds
  }

  def create(db: TextReflowDB, lwidget: LabelWidget): LabelWidgetIndex = {
    val lwIndex = SpatialIndex.createFor[PosAttr]()

    val layout0 = layoutWidgetPositions(lwidget)

    layout0.foreach({pos =>
      lwIndex.add(pos)
    })

    new LabelWidgetIndex {
      def docStore: DocumentCorpus = db.docstorage
      def layout: List[PosAttr] = layout0
      def index: SpatialIndex[PosAttr] = lwIndex
    }
  }
}


trait LabelWidgetIndex {

  def docStore: DocumentCorpus
  def layout: List[PosAttr]
  def index: SpatialIndex[PosAttr]

  def getWidgetForTargetRegion(targetRegion: TargetRegion): PosAttr = {
    val stableId = targetRegion.stableId
    val docId = docStore.getDocument(stableId).get
    val pageId = docStore.getPage(docId, targetRegion.pageNum).get
    // Map TargetRegion -> PosAttr
    layout
      .collect({
        case p @ PosAttr(
          LabeledTarget(bbox, label, score),
          widgetBounds,
          pRegionId, _, _
        ) if targetRegion.id == targetRegion.id => p
      }).headOption
      .getOrElse(sys.error(s"getWidgetForTargetRegion: no entry for ${targetRegion}"))

    ???

  }

  def querySelected(bbox: LTBounds): Seq[LabeledTarget] = {
    val positioned: Seq[PosAttr] = index.queryForIntersects(bbox)

    // val selectedTargets: List[(PosAttr, LabeledTarget)]
    val selectedTargets: List[LabeledTarget] = positioned.toList
      .map(p => index.getItem(p.id.unwrap))
      .filter(_.widget.isInstanceOf[LabeledTarget])
      .map(p => p.widget.asInstanceOf[LabeledTarget])

    selectedTargets
  }

  def runUIRequest(r: UIRequest): UIResponse = {
    val UIRequest(uiState, gesture) = r



    // response: indicate(region, )

    UIResponse(List())
  }

  def onSelect(targetLabel: Label, bbox: LTBounds): List[LTBounds] = {

    val positioned: Seq[PosAttr] = index.queryForIntersects(bbox)

    val selectedTargets = querySelected(bbox)

    val updates: Option[Seq[PosAttr]] = if (selectedTargets.nonEmpty) {

      val visualLineZones: Seq[Zone] = for {
        labeledTarget <- selectedTargets
        zoneId <- docStore.getZoneForTargetRegion(labeledTarget.target.id, LB.VisualLine)
      } yield {
        docStore.getZone(zoneId)
      }

      // If any selected regions are already part of a zone...
      val resultZone = if (visualLineZones.nonEmpty) {
        // Merge them..
        def mergedZone: Zone =  ???

        // docStore.getZone(docStore.mergeZones(existingZones.map(_.id)))

        // Add all target regions to merged zone
        selectedTargets.map(tr => docStore.setZoneTargetRegions(
          mergedZone.id,
          mergedZone.regions :+ tr.target
        ))
        Option(mergedZone)

      } else {
        // Create a new Zone with given label
        val stableId = selectedTargets.head.target.stableId
        val docId = docStore
          .getDocument(stableId)
          .getOrElse(sys.error(s"onSelect() document ${stableId} not found"))

        val targetRegions = selectedTargets.map(_.target)
        val newZone = docStore.getZone(
          docStore.createZone(docId)
        )
        docStore.setZoneTargetRegions(newZone.id, targetRegions)
        docStore.addZoneLabel(newZone.id, targetLabel)

        Option(newZone)
      }

      None
    } else {
      None
    }
    ???

  }
}
