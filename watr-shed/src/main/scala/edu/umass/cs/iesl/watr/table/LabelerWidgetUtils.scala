package edu.umass.cs.iesl.watr
package table


// import BioArxiv._
// import AlignBioArxiv._
// import display._
import display.data._
import geometry._
// import PageComponentImplicits._

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

}

object LabelWidgetUtils extends LabelWidgetUtils
