package edu.umass.cs.iesl.watr
package labeling

import scalaz.std.anyVal._
// import scalaz._, Scalaz._
import matryoshka._
import matryoshka.implicits._
import textboxing.{TextBoxing => TB}

import LabelWidgetF._

trait LabelWidgetBasics {

  import utils.ScalazTreeImplicits._


  def prettyPrintLabelWidget(lwidget: LabelWidget): TB.Box = {
    lwidget.cata(toTree).drawBox
  }


}
