package edu.umass.cs.iesl.watr
package labeling

import scalaz._, Scalaz._
import matryoshka._
import matryoshka.implicits._

trait LabelWidgetBasics {

  import utils.ScalazTreeImplicits._
  import textboxing.{TextBoxing => TB}

  import LabelWidgetF._


  def prettyPrintLabelWidget(lwidget: LabelWidget): TB.Box = {
    lwidget.cata(toTree).drawBox
  }
}
