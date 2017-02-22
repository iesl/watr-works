package edu.umass.cs.iesl.watr
package table

import pprint.PPrinter
import spindex._
import textreflow.data._
import textboxing.{TextBoxing => TB}
import labeling._
import labeling.data._

object ShellPrettyPrinters extends ComponentEnrichments {

  val pprintComponent: PPrinter[Component] = PPrinter({(component, config) =>
    val box = component.show
    Iterator(box.toString())
  })

  val pprintBox: PPrinter[TB.Box] = PPrinter({(box, config) =>
    Iterator(box.toString())
  })

  val pprintTextReflow: PPrinter[TextReflow] = PPrinter({(textReflow, config) =>
    val text = textReflow.toText()
    Iterator(text)
  })

  val pprintLabelWidget: PPrinter[LabelWidget] = PPrinter({(lwidget, config) =>
    val pp = LabelWidgetIndex.prettyPrintLabelWidget(lwidget)
    Iterator(pp.toString)
  })
}
