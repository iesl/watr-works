package edu.umass.cs.iesl.watr
package spindex

import textboxing.{TextBoxing => TB}
import watrmarks._
import watrmarks.{StandardLabels => LB}

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

  def dquote(b: TextReflow): TextReflow = bracket('"', '"', b)
  def squareBracket(b: TextReflow): TextReflow = bracket('[', ']', b)
  def curlyBrace(b: TextReflow): TextReflow = bracket('{', '}', b)


  def getDescendantLabels(cc: Component): Set[Label] = {
    cc.getLabels ++ (
      cc.getDescendants(LB.TextSpan)
        .map(_.getLabels)
        .reduce(_ ++ _)
    )
  }

  def transferLabel(l: Label, cc: Component)(t: TextReflow): TextReflow = {
    if (cc.hasLabel(l)) {
      addLabel(l)(t)
    } else t
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
        )

        Some(labeled)

      case LB.PageAtom =>
        val ac = cc.asInstanceOf[AtomicComponent]
        val ops = new textreflow.TextReflowAtomOps(ac.char)
        atom(ac.pageAtom, ops).some

      case _ => sys.error(s"renderCC(${cc}): unmatched roleLabel ${cc.roleLabel}")
    }
  }

}
