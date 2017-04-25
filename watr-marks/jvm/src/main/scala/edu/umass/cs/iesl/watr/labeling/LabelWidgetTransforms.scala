package edu.umass.cs.iesl.watr
package labeling

import geometry._
import geometry.syntax._
import PageComponentImplicits._

import LabelWidgetF._
import LabelWidgets._
import corpora._

import matryoshka._
import matryoshka.implicits._

import scala.reflect._

import watrmarks.Label

object LabelWidgetTransforms {

  def hasId(id: Int, cls: String): LabelWidgetT => Boolean = _ match {
    case Identified(wid, _, id0, cls0)
        if id==id0  && cls==cls0 => true
    case _ => false
  }

  def atEveryId[T: ClassTag](
    id: Int@@T,
    widget: LabelWidget,
    f: LabelWidget => LabelWidget
  ): LabelWidget = {
    val cls = implicitly[ClassTag[T]].runtimeClass.getSimpleName

    def visit(lw: LabelWidgetT): LabelWidgetT = lw match {
      case Identified(wid, _, id0, cls0) if id==id0  && cls==cls0 =>
        holes(lw) match {
          case Identified(wid, (a, fWhole), id0, cls0) => fWhole(f(a))
          case x => sys.error(s"atEveryId ${lw}: ${x}")
        }
      case _ => lw
    }

    widget.transCata[LabelWidget](visit)
  }

  def everywhere(r: LabelWidget)(f: LabelWidgetT => LabelWidgetT): LabelWidget = {
    r.transCata[LabelWidget](f)
  }

  def addZoneIndicator(zoneId: Int@@ZoneID, lwidget: LabelWidget, docStore: DocumentCorpus): LabelWidget = {

    // append rectangular overlays which respond to user clicks to select/deselect zones
    def addIndicator(lw0: LabelWidgetT): LabelWidgetT = {
      lw0 match {

        case l @ RegionOverlay(wid1, under, overlays) =>
          val pageId = under.page.pageId
          val pageDef = docStore.getPageDef(pageId).getOrElse {
            sys.error(s"addIndicator(): no page found for ${pageId}")
          }

          val clipBox = under.bbox

          val zone = docStore.getZone(zoneId)

          val filteredRegionsToTargetRegion = zone.regions.filter({ targetRegion =>
            val zoneTargetRegion = docStore.getTargetRegion(targetRegion.id)
            zoneTargetRegion.intersects(pageId, clipBox)
          })

          // clip zone target regions to clipped page region
          val intersectingBboxes: List[GeometricFigure] =
            filteredRegionsToTargetRegion
              .flatMap { targetRegion =>
                targetRegion
                  .intersection(clipBox)
                  .map(_.bbox)
              }.toList

          val overlayWidgets = if (intersectingBboxes.isEmpty) None else {
            val groupBbox = intersectingBboxes.map(totalBounds(_)).reduce(_ union _)
            Some(panel(
              withId(zoneId, figure(GeometricGroup(groupBbox, intersectingBboxes))),
              LabelAction.toggleZoneSelection(zoneId)
            ))
          }

          l.copy(
            overlays = l.overlays ++ overlayWidgets.toList
          )

        case  _ => lw0
      }
    }

    lwidget.transCata[LabelWidget](addIndicator)
  }

  def addAllZoneIndicators(labels: Seq[Label], lwidget: LabelWidget, docStore: DocumentCorpus): LabelWidget = {
    labels.foldLeft(lwidget) {
      case (acc, elemLabel) =>
        LabelWidgetTransforms.addZoneIndicators(elemLabel, acc, docStore)
      }
  }

  def addZoneIndicators(label: Label, lwidget: LabelWidget, docStore: DocumentCorpus): LabelWidget = {

    val labelId = docStore.ensureLabel(label)

    // append rectangular overlays which respond to user clicks to select/deselect zones
    def addIndicator(lw0: LabelWidgetT): LabelWidgetT = {
      lw0 match {

        case l @ RegionOverlay(wid1, under, overlays) =>
          val pageId = under.page.pageId
          val pageDef = docStore.getPageDef(pageId).getOrElse {
            sys.error(s"addIndicator(): no page found for ${pageId}")
          }

          val clipBox = under.bbox

          val zoneLineOverlays: Seq[Option[LabelWidget]] = for {
            zoneId <- docStore.getZonesForDocument(pageDef.document, labelId)
          } yield {
            val zone = docStore.getZone(zoneId)

            val filteredRegionsToTargetRegion = zone.regions.filter({ targetRegion =>

              val zoneTargetRegion = docStore.getTargetRegion(targetRegion.id)
              zoneTargetRegion.intersects(pageId, clipBox)
            })

            // clip zone target regions to clipped page region
            val intersectingBboxes:List[GeometricFigure] =
              filteredRegionsToTargetRegion
                .flatMap { targetRegion =>
                  targetRegion
                    .intersection(clipBox)
                    .map(_.bbox)
                }.toList

            if (intersectingBboxes.isEmpty) None else {
              val groupBbox = intersectingBboxes.map(totalBounds(_)).reduce(_ union _)
              Some(panel(
                withId(zoneId, figure(GeometricGroup(groupBbox, intersectingBboxes))),
                LabelAction.toggleZoneSelection(zoneId)
              ))
            }
          }

          l.copy(
            overlays = overlays ++ zoneLineOverlays.flatten
          )

        case  _ => lw0
      }
    }

    lwidget.transCata[LabelWidget](addIndicator)
  }
}
