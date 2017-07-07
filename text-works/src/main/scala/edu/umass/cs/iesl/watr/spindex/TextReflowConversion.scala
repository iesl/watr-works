package edu.umass.cs.iesl.watr
package spindex

import watrmarks._

object TextReflowConversion {
  import textboxing.{TextBoxing => TB}
  import watrmarks.{StandardLabels => LB}
  import TB._
  import textreflow.data._
  import ComponentOperations._


  def childSpansOrAtoms(cc: Component): Seq[Component] = {
    if (cc.hasChildren(LB.TextSpan)) {
      cc.getChildren(LB.TextSpan)
    } else {
      cc.queryAtoms()
    }
  }

  def hasLabel(cc: Component, l: Label) = cc.getLabels.contains(l)

  def transferLabel(l: Label, cc: Component)(t: TextReflow): TextReflow = {
    cc.getLabel(l)
      .map(l => addLabel(l)(t))
      .getOrElse(t)
  }

  var __debug = false

  def toTextReflow(cc: Component): Option[TextReflow] = {
    _toTextReflow(cc, 0).map{ reflow =>

      reflow

    }
  }

  def _toTextReflow(cc: Component, level: Int=0): Option[TextReflow] = {
    if (__debug) {
      println(indent(level*4)("toTextReflow: Enter"))
      println(indent(level*4)(renderRoleTree(cc)))
    }
    cc.roleLabel match {
      case LB.VisualLine
         | LB.TextSpan =>
        val children = childSpansOrAtoms(cc)

        val childReflows = children.map(_toTextReflow(_, level+1))

        val withSpaces = (children.zip(childReflows))
          .foldLeft(List[TextReflow]()) {
          case (acc, (childCC, maybeChildReflow)) =>
            var acc0 =  acc
            maybeChildReflow.foreach { childReflow  =>
              if (hasLabel(childCC, LB.WhitespaceAfter)) {
                acc0 = space() :: acc0
              }
              acc0 = childReflow :: acc0
            }
            acc0
        }
        val joined = concat(withSpaces.reverse)

        val labeled = (joined
          |> transferLabel(LB.Sup, cc)
          |> transferLabel(LB.Sub, cc)
          |> transferLabel(LB.Token, cc)
          |> transferLabel(LB.VisualLine, cc)
          |> transferLabel(LB.TextBlock, cc)
        )

        if (__debug) {
          println(indent(level*4)("Exit: "))
          println(indent(level*4)(prettyPrintTree(labeled)))
        }
        Some(labeled)

      case LB.PageAtom =>
        val ac = cc.asInstanceOf[AtomicComponent]
        atom(ac.charAtom).some

      case _ => sys.error(s"renderCC(${cc}): unmatched roleLabel ${cc.roleLabel}")
    }
  }

}
