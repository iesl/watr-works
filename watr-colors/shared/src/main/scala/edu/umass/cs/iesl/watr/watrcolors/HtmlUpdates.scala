package edu.umass.cs.iesl.watr
package watrcolors

import boopickle.DefaultBasic._


sealed trait HtmlUpdate

final case class HtmlPrepend(css: String, content: String) extends HtmlUpdate
final case class HtmlAppend(css: String, content: String) extends HtmlUpdate
final case class HtmlReplace(css: String, content: String) extends HtmlUpdate
final case class HtmlReplaceInner(css: String, content: String) extends HtmlUpdate
final case class HtmlRemove(css: String) extends HtmlUpdate

sealed trait Overlay
sealed trait OverlayInfo

case class CharInfo(
  info: String
) extends OverlayInfo

case class LabelInfo(
  label: String
) extends OverlayInfo

case class BBox(
  x: Double, y: Double, width: Double, height: Double
  // info: Option[OverlayInfo] = None
) extends Overlay

object Picklers {
  implicit val pickler0 = compositePickler[HtmlUpdate]

  implicit val p1: Pickler[HtmlAppend] = PicklerGenerator.generatePickler[HtmlAppend]
  implicit val p2: Pickler[HtmlPrepend] = PicklerGenerator.generatePickler[HtmlPrepend]
  implicit val p3: Pickler[HtmlRemove] = PicklerGenerator.generatePickler[HtmlRemove]
  implicit val p4: Pickler[HtmlReplaceInner] = PicklerGenerator.generatePickler[HtmlReplaceInner]
  implicit val p5: Pickler[HtmlReplace] = PicklerGenerator.generatePickler[HtmlReplace]

  pickler0
    .addConcreteType[HtmlPrepend]
    .addConcreteType[HtmlAppend]
    .addConcreteType[HtmlRemove]
    .addConcreteType[HtmlReplaceInner]
    .addConcreteType[HtmlReplace]

  implicit val pickleOverlayInfo = compositePickler[OverlayInfo]

  implicit val q1 = PicklerGenerator.generatePickler[CharInfo]
  implicit val q2 = PicklerGenerator.generatePickler[LabelInfo]

  pickleOverlayInfo
    .addConcreteType[CharInfo]
    .addConcreteType[LabelInfo]




  implicit val pOverlay = compositePickler[Overlay]
  implicit val pBBox = PicklerGenerator.generatePickler[BBox]

  pOverlay.addConcreteType[BBox]
}


