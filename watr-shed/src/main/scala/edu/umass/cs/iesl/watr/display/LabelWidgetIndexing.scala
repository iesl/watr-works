package edu.umass.cs.iesl.watr
package display


import geometry._
// import display.data._
import spindex._

// import utils.{CompassDirection => CDir}
// import textreflow._
// import GeometryImplicits._
// import PageComponentImplicits._

import matryoshka._
// import matryoshka.data._
import matryoshka.implicits._

import LabelWidgetF._
// import LabelWidgets._

object LabelWidgetIndex {

  // type PositionedT = Positioned[LabelWidget]
  type PositionedT = Positioned[Unit]

  implicit object LabelWidget extends SpatialIndexable[PositionedT] {
    def id(t: PositionedT): Int = t.id.unwrap
    def ltBounds(t: PositionedT): LTBounds = t.widgetBbox
  }
}

import LabelWidgetIndex._

case class LabelWidgetIndex(
  lwIndex: SpatialIndex[PositionedT],
  lwidget: LabelWidget,
  positioned: LabelWidget
) {

}

object LabelWidgetIndexing extends LabelWidgetBasics {

  def indexLabelWidget(lwidget: LabelWidget): LabelWidgetIndex = {
    val lwIndex = SpatialIndex.createFor[PositionedT]()

    val positionedLabelWidget = absPositionLabelWidget(lwidget)

    def visit(t: LabelWidgetF[Unit]): Unit = t match {
      case p @ Positioned(a, pvec, wbbox, tbbox, id) =>
        lwIndex.add(p)

      case _ => ()
    }

    positionedLabelWidget.cata(visit)

    new LabelWidgetIndex(lwIndex, lwidget, positionedLabelWidget)
  }
}
