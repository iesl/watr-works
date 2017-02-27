package edu.umass.cs.iesl.watr
package labeling


import data._
import geometry._

trait LabelWidgetUtils {

  def pageControls = makeButtons(
    "Next Labeling Tasks",
    "Finished"
  )


  def makeButtons(actions: String*): LabelWidget = {
    val bpad = Padding(4d, 4d, 4d, 4d)
    LW.row(
      actions.toList.map(action =>
        LW.pad(LW.button(action), bpad)
      ):_*
    )
  }

  def makeRadios(labels: String*): LabelWidget = {
    val bpad = Padding(4d, 4d, 4d, 4d)
    LW.row(
      labels.toList.map(action =>
        LW.pad(LW.button(action), bpad)
      ):_*
    )

  }

  def makePagePanel(body: LabelWidget): LabelWidget = {
    LW.panel(
      LW.col(
        pageControls,
        body
      )
    )

  }
}

object LabelWidgetUtils extends LabelWidgetUtils
