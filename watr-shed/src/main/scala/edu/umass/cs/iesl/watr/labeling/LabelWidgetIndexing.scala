package edu.umass.cs.iesl.watr
package labeling

import geometry._
import spindex._
import docstore._
import LabelWidgetF._
import textreflow.ReflowDocstore

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
      def docStore: ReflowDocstore = db.docstorage
      def layout: List[PosAttr] = layout0
      def index: SpatialIndex[PosAttr] = lwIndex
    }
  }
}


// TODO rename this, it's a general LabelWidget interaction API
trait LabelWidgetIndex {

  def docStore: ReflowDocstore
  def layout: List[PosAttr]
  def index: SpatialIndex[PosAttr]

  def onClick(clickPoint: Point): List[LTBounds] = {
    ???
  }


  def getWidgetForTargetRegion(targetRegion: TargetRegion): PosAttr = {
    // Map TargetRegion -> PosAttr
    layout
      .collect({
        case p @ PosAttr(
          LabeledTarget(tr, label, score),
          widgetBounds,
          pRegionId, _, _
        ) if tr.id == targetRegion.id => p
      }).headOption
      .getOrElse(sys.error(s"getWidgetForTargetRegion: no entry for ${targetRegion}"))

    ???

  }

  def onSelect(bbox: LTBounds): List[LTBounds] = {

    val positioned: Seq[PosAttr] = index.queryForIntersects(bbox)

    val selectedTargets: List[(PosAttr, LabeledTarget)] = positioned.toList
      .map(p => index.getItem(p.id.unwrap))
      .filter(_.widget.isInstanceOf[LabeledTarget])
      .map(p => (p, p.widget.asInstanceOf[LabeledTarget]))

    val updates: Option[Seq[PosAttr]] = if (selectedTargets.nonEmpty) {


      val targetLabels = selectedTargets.map(_._2.label).toSet

      val updates0: Option[Seq[PosAttr]] = if (targetLabels.size == 1) {
        // If there is exactly one label present within selected regions..
        // ... use that label for zone
        val label = targetLabels.head.get

        val existingZones: Seq[Zone] = for {
          (posAttr, target) <- selectedTargets
          zoneId <- docStore.getZonesForTargetRegion(target.target.id)
        } yield {
          docStore.getZone(zoneId)
        }

        // If any selected regions are already part of a zone...
        val resultZone = if (existingZones.nonEmpty) {
          // Merge them..
          val mergedZone: Zone =  ???
            // docStore.getZone(docStore.mergeZones(existingZones.map(_.id)))

          // Add all target regions to merged zone
          selectedTargets.map(tr => docStore.setZoneTargetRegions(
            mergedZone.id,
            mergedZone.regions :+ tr._2.target
          ))
          Option(mergedZone)

        } else {
          // Create a new Zone with given label
          val stableId = selectedTargets.head._2.target.stableId
          val docId = docStore
            .getDocument(stableId)
            .getOrElse(sys.error(s"onSelect() document ${stableId} not found"))

          val targetRegions = selectedTargets.map(_._2.target)
          val newZone = docStore.getZone(
            docStore.createZone(docId)
          )
          docStore.setZoneTargetRegions(newZone.id, targetRegions)

          Option(newZone)
        }

        resultZone.map({zone =>
          zone.regions
            .map(getWidgetForTargetRegion(_))

        })
      } else if (targetLabels.nonEmpty) {
        // Add all selected targets to whatever zone they are presumed to be
        None

      } else {
        // Error: No targets w/labels were selected
        None
      }
      updates0
    } else {
      None
    }

    updates
      .toList.flatten
      .map(p => p.widgetBounds)

  }
}
