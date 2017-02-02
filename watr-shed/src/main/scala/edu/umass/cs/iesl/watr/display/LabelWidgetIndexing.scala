package edu.umass.cs.iesl.watr
package display


import geometry._
import spindex._


import LabelWidgetF._

object LabelWidgetIndex extends LabelWidgetLayout {

  implicit object LabelWidget extends SpatialIndexable[Position] {
    def id(t: Position): Int = t.id.unwrap
    def ltBounds(t: Position): LTBounds = t.totalBounds
  }
  def indexLabelWidget(lwidget: LabelWidget): LabelWidgetIndex = {
    val lwIndex = SpatialIndex.createFor[Position]()

    val positionedLabelWidget = layoutWidgetPositions(lwidget)

    def visit(t: Position): Unit = {
      lwIndex.add(t)
      t.children.map(visit)
    }

    visit(positionedLabelWidget)

    new LabelWidgetIndex(lwIndex, lwidget, positionedLabelWidget)
  }
}

// import LabelWidgetIndex._

case class LabelWidgetIndex(
  lwIndex: SpatialIndex[Position],
  lwidget: LabelWidget,
  position: Position
)

// object LabelWidgetIndexing extends LabelWidgetLayout {}
