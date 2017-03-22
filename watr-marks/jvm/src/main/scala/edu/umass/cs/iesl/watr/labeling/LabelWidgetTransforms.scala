package edu.umass.cs.iesl.watr
package labeling

import geometry._
import PageComponentImplicits._

import LabelWidgetF._
import LabelWidgets._
import corpora._

import matryoshka._
import matryoshka.implicits._

import watrmarks.{StandardLabels => LB}

object LabelWidgetTransforms {

  def addZoneIndicators(lwidget: LabelWidget, docStore: DocumentCorpus): LabelWidget = {

    // append rectangular overlays which respond to user clicks to select/deselect zones
    def addIndicator(lw0: LabelWidgetT): LabelWidgetT = {
      lw0 match {
        case RegionOverlay(under, overlays) =>
          val pageDef = docStore.getPageDef(under.pageId).getOrElse {
            sys.error(s"addIndicator(): no page found for ${under}")
          }

          val zoneLineOverlays: Seq[Option[LabelWidget]] = for {
            zoneId <- docStore.getZonesForDocument(pageDef.document)
          } yield {
            val zone = docStore.getZone(zoneId)

            if (zone.labels.contains(LB.VisualLine)) None else {

              val filteredRegionsToTargetRegion = zone.regions.filter({zoneRegion =>
                val docId = docStore.getDocument(zoneRegion.stableId).get
                val zonePageId = docStore.getPage(docId, zoneRegion.pageNum).get
                val zonePageRegion = PageRegion(
                  zonePageId,
                  zoneRegion.bbox
                )
                zonePageRegion.intersects(under)
              })

              // TODO: maybe make each line region clickable??
              // clip zone to under's target region
              val intersectingBboxes:List[GeometricFigure] = filteredRegionsToTargetRegion.flatMap {fr =>
                fr.intersection(under.bbox).map(_.bbox)
              }.toList

              if (intersectingBboxes.isEmpty) None else {
                Some(panel(
                  figure(GeometricGroup(intersectingBboxes)),
                  LabelAction.clickToSelectZone(zoneId)
                ))
              }
            }
          }

          RegionOverlay(
            under,
            overlays ++ zoneLineOverlays.flatten
          )

        case  _ => lw0
      }
    }

    lwidget.transCata[LabelWidget](addIndicator)
  }
}
