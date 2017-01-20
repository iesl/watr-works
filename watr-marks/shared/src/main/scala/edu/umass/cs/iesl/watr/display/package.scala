package edu.umass.cs.iesl.watr
package display

import matryoshka.data._

object `package` {

  type LabelWidget = Fix[LabelWidgetF]

  type LabelWidgetT = LabelWidgetF[Fix[LabelWidgetF]]

}
