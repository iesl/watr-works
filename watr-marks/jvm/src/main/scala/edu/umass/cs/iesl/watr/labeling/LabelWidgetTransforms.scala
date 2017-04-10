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
    case Identified(_, id0, cls0)
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
      case Identified(_, id0, cls0) if id==id0  && cls==cls0 =>
        holes(lw) match {
          case Identified((a, fWhole), id0, cls0) => fWhole(f(a))
          case x => sys.error(s"atEveryId ${lw}: ${x}")
        }
      case _ => lw
    }

    widget.transCata[LabelWidget](visit)
  }

  def everywhere(r: LabelWidget)(f: LabelWidgetT => LabelWidgetT): LabelWidget = {
    r.transCata[LabelWidget](f)
  }

  def addZoneIndicators(label: Label, lwidget: LabelWidget, docStore: DocumentCorpus): LabelWidget = {

    val labelId = docStore.ensureLabel(label)

    // append rectangular overlays which respond to user clicks to select/deselect zones
    def addIndicator(lw0: LabelWidgetT): LabelWidgetT = {
      lw0 match {
        case RegionOverlay(under, overlays) =>
          val pageDef = docStore.getPageDef(under.page.pageId).getOrElse {
            sys.error(s"addIndicator(): no page found for ${under}")
          }

          val zoneLineOverlays: Seq[Option[LabelWidget]] = for {
            zoneId <- docStore.getZonesForDocument(pageDef.document, labelId)
          } yield {
            val zone = docStore.getZone(zoneId)

            val filteredRegionsToTargetRegion = zone.regions.filter({ targetRegion =>

              val zoneTargetRegion = docStore.getTargetRegion(targetRegion.id)
              zoneTargetRegion.intersects(under)
            })

            // clip zone to under's target region
            val intersectingBboxes:List[GeometricFigure] =
              filteredRegionsToTargetRegion
                .flatMap { targetRegion =>
                  targetRegion
                    .intersection(under.bbox)
                    .map(_.bbox)
                }.toList

            if (intersectingBboxes.isEmpty) None else {
              val groupBbox = intersectingBboxes.map(totalBounds(_)).reduce(_ union _)
              Some(panel(
                withId(zoneId, figure(GeometricGroup(groupBbox, intersectingBboxes))),
                LabelAction.clickToSelectZone(zoneId)
              ))
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
