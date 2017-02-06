package edu.umass.cs.iesl.watr
package labeling

import geometry._
import spindex._
import LabelWidgetF._

object LabelWidgetIndex extends LabelWidgetLayout {

  implicit object LabelWidget extends SpatialIndexable[PosAttr] {
    def id(t: PosAttr): Int = t.id.unwrap
    def ltBounds(t: PosAttr): LTBounds = t.widgetBounds
  }

  def create(lwidget: LabelWidget): LabelWidgetIndex = {
    val lwIndex = SpatialIndex.createFor[PosAttr]()

    val layout = layoutWidgetPositions(lwidget)

    layout.foreach({pos =>
      lwIndex.add(pos)
    })

    LabelWidgetIndex(layout, lwIndex)
  }
}

case class LabelWidgetIndex(
  layout: List[PosAttr],
  index: SpatialIndex[PosAttr]
)
