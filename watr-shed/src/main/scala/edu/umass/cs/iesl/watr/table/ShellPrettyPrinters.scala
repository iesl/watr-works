package edu.umass.cs.iesl.watr
package table

import pprint._
import spindex._
// import textreflow.data._
import textboxing.{TextBoxing => TB}
// import labeling._
// import labeling.data._
// import scala.{Iterator => Iter}

object ShellPrettyPrinters extends ComponentEnrichments {


  def additionalHandlers: PartialFunction[Any, Tree] = {
    case c: Component => Tree.Literal(c.show.toString())
    case c: TB.Box    => Tree.Literal(c.toString())
  }

  // val pprintTextReflow: PPrinter = PPrinter.apply[TextReflow]({(textReflow, config) =>
  //   val text = textReflow.toText()
  //   Iterator(text)
  // })

  // val pprintLabelWidget: PPrinter = PPrinter.apply[LabelWidget]({(lwidget, config) =>
  //   val pp = LabelWidgetIndex.prettyPrintLabelWidget(lwidget)
  //   Iterator(pp.toString)
  // })
}
