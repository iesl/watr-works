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
import utils.Colors
import utils.Color

import watrmarks.Label
// import scalaz.std.list._

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

  def regenerateIds(lw: LabelWidget): LabelWidget = {
    everywhere(lw){ fa =>
      regenerateId(fa)
    }
  }

  def makeZoneHighlightFigure(
    label: Label,
    intersectingBboxes: List[GeometricFigure],
    zoneColor: Color
  ): LabelWidget = {
    val groupBbox = intersectingBboxes.map(totalBounds(_)).reduce(_ union _)
    withLabel("tooltip", label.key,
      figure(
        Colorized(
          GeometricGroup(groupBbox, intersectingBboxes),
          fg=zoneColor, bg=zoneColor,
          fgOpacity=0.1f, bgOpacity=0.2f
        )
      )
    )
  }

  def makeZoneHighlight(
    zoneId: Int@@ZoneID,
    label: Label,
    intersectingBboxes: List[GeometricFigure],
    zoneColor: Color
  ): LabelWidget = {
    // val groupBbox = intersectingBboxes.map(totalBounds(_)).reduce(_ union _)
    panel(
      withId(zoneId,
        makeZoneHighlightFigure(label, intersectingBboxes, zoneColor)
      ),
      LabelAction.toggleZoneSelection(zoneId)
    )
  }

  def selectionFringeToggle(
    labelWidget: LabelWidget,
    toggleOn: Boolean
  ): LabelWidget = {
    println(s"selectionFringeToggle(toggleOn = ${toggleOn})")

    if (toggleOn) {
      labelWidget match {
        case PatLabeled(lid,
          Embed(Figure(fid, fig)),
          key, value
        ) =>
          val fringe = figure(
            Colorized(
              makeFringe(fig, Padding.Ints(4)),
              fg=Colors.Yellow, bg=Colors.Yellow,
              fgOpacity=0.0f, bgOpacity=0.1f
            ))

          zstack(regenerateIds(labelWidget), fringe)

        case x =>
          println(s"selectionFringeToggle: match error ${x}")
          x

      }
    } else {
      labelWidget match {
        case PatZStack(lid, as) =>
          regenerateIds(as.head)

        case x =>
          println(s"selectionFringeToggle: match error ${x}")
          x
      }
    }
  }

  def addZoneIndicator(zoneId: Int@@ZoneID, labelWidget: LabelWidget, labelColors: Map[Label, Color], docStore: DocumentZoningApi): LabelWidget = {
    // append rectangular overlays which respond to user clicks to select/deselect zones
    def addIndicator(lw0: LabelWidgetT): LabelWidgetT = {
      lw0 match {

        case l @ RegionOverlay(wid1, under, overlays) =>
          val pageId = under.page.pageId
          val clipBox = under.bbox

          val zone = docStore.getZone(zoneId)
          val zoneColor = labelColors(zone.label)

          // clip zone target regions to clipped page region
          val intersectingBboxes = for {
            region <- zone.regions.toList
            if region.intersects(pageId, clipBox)

            intersect <- region.intersection(clipBox)

          } yield intersect.bbox

          if (intersectingBboxes.isEmpty) {
            l
          } else {
            val fringeOverlay = makeZoneHighlight(zoneId, zone.label, intersectingBboxes, zoneColor)
            l.copy(overlays = overlays :+ fringeOverlay)
          }

        case  _ => lw0
      }
    }

    labelWidget.transCata[LabelWidget](addIndicator)
  }


  // def addAllZoneIndicators(labelWidget: LabelWidget, labelerIdentifier: LabelerIdentifier, docStore: DocumentZoningApi): LabelWidget = {
  def addAllZoneIndicators(labelWidget: LabelWidget, labelColors: Map[Label, Color], docStore: DocumentZoningApi): LabelWidget = {

    labelColors.toList.foldLeft(labelWidget) {
      case (acc, (elemLabel, _)) =>
        addZoneIndicators(elemLabel, acc, labelColors, docStore)
      }
  }


  def addZoneIndicators(label: Label, labelWidget: LabelWidget, labelColors: Map[Label, Color], docStore: DocumentZoningApi): LabelWidget = {

    println(s"0---")
    val labelId = docStore.ensureLabel(label)

    val pageIds = labelWidget.universe.list
      .toList.collect{
        case l @ Embed(RegionOverlay(wid1, under, overlays)) => under.page.pageId
      }.toSet.toList

    val documentIds: Map[Int@@PageID, Int@@DocumentID] =
      pageIds.foldLeft(Map[Int@@PageID, Int@@DocumentID]()){
        case (docIds, pageId) =>
          if (docIds.contains(pageId)){
            docIds
          } else {
            val pageDef = docStore.getPageDef(pageId).getOrElse {
              sys.error(s"addIndicator(): no page found for ${pageId}")
            }
            docIds + (pageId -> pageDef.document)
          }
      }

    val documentZoneMap = documentIds.values.toSet
      .foldLeft(Map[Int@@DocumentID, List[Zone]]()){
        case (acc, docId)  =>

          if (acc.contains(docId)) {
            acc
          } else {
            val zones = docStore.getZonesForDocument(docId,labelId)
              .map(docStore.getZone(_))
              .toList

            acc + (docId -> zones)
          }
      }


    // append rectangular overlays which respond to user clicks to select/deselect zones
    def addIndicator(lw0: LabelWidgetT): LabelWidgetT = {
      lw0 match {

        case l @ RegionOverlay(wid1, under, overlays) =>
          val pageId = under.page.pageId
          val docId = documentIds(pageId)
          val clipBox = under.bbox

          val zoneIndicators: Seq[Option[LabelWidget]] = for {
            zone <- documentZoneMap(docId)
          } yield {
            val zoneColor = labelColors(label)

            val intersectingBboxes = for {
              region <- zone.regions.toList
              if region.intersects(pageId, clipBox)

              intersect <- region.intersection(clipBox)

            } yield intersect.bbox


            if (intersectingBboxes.isEmpty) None else {
              Some(makeZoneHighlight(zone.id, zone.label, intersectingBboxes, zoneColor))
            }
          }

          l.copy(overlays = overlays ++ zoneIndicators.flatten.toList)


        case  _ => lw0
      }
    }

    labelWidget.transCata[LabelWidget](addIndicator)
  }
}
