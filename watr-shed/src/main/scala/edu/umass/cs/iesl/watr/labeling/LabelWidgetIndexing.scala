package edu.umass.cs.iesl.watr
package labeling

import geometry._
import spindex._
import docstore._
import LabelWidgetF._

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
      def reflowDB: TextReflowDB = db
      def layout: List[PosAttr] = layout0
      def index: SpatialIndex[PosAttr] = lwIndex
    }
  }
}

import scalaz.syntax.std.list._

// TODO rename this, it's a general LabelWidget interaction API
trait LabelWidgetIndex {

  def reflowDB: TextReflowDB
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

        val existingZones = selectedTargets
          .map(p => (p, reflowDB.selectZones(p._2.target.docId, p._2.target.pageNum, label)))

        // If any selected regions are already part of a zone...
        val resultZone = if (existingZones.nonEmpty) {
          // Merge them..
          val mergedZone = reflowDB.mergeZones(existingZones.map(_._2).flatten)

          // Add all target regions to merged zone
          selectedTargets
            .map(tr => reflowDB.addZoneTargetRegion(mergedZone, tr._2.target))
          Option(mergedZone)

        } else {
          // Create a new Zone with given label
          val docId = selectedTargets.head._2.target.docId

          val trs = selectedTargets.map(_._2.target)

          Option(reflowDB.createZone(docId, trs))
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
