package edu.umass.cs.iesl.watr
package spindex

import watrmarks._
import geometry.CharAtom
import ComponentOperations._

object TextReflowConversion {
  import TB._
  import textreflow._


  def childSpansOrAtoms(cc: Component): Seq[Component] = {
    if (cc.hasChildren(LB.TextSpan)) {
      cc.getChildren(LB.TextSpan)
    } else {
      cc.queryAtoms()
    }
  }

  def hasLabel(cc: Component, l: Label) = cc.getLabels.contains(l)
  def isTokenized(cc: Component) = hasLabel(cc, LB.Tokenized)

  def transferLabel(l: Label, cc: Component)(t: TextReflow): TextReflow = {
    cc.getLabel(l)
      .map(l => addLabel(l)(t))
      .getOrElse(t)
  }

  def toTextReflow(cc: Component): Option[TextReflow] = {
    cc.roleLabel match {
      case LB.VisualLine
         | LB.TextSpan =>
        val children = childSpansOrAtoms(cc)

        val childReflows = children.map(toTextReflow(_))
        val joined = if (isTokenized(cc)) {
          joins(" ")(childReflows.flatten)
        } else {
          concat(childReflows.flatten)
        }

        val labeled = (joined
          |> transferLabel(LB.Sup, cc)
          |> transferLabel(LB.Sub, cc)
          |> transferLabel(LB.VisualLine, cc)
          |> transferLabel(LB.TextBlock, cc)
        )

        Some(labeled)

      case LB.PageAtom =>
        val ac = cc.asInstanceOf[AtomicComponent]
        atom(ac.pageAtom.asInstanceOf[CharAtom]).some

      case _ => sys.error(s"renderCC(${cc}): unmatched roleLabel ${cc.roleLabel}")
    }
  }

}
