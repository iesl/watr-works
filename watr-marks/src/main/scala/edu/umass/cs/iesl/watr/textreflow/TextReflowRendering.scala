package edu.umass.cs.iesl.watr
package textreflow

object TextReflowRendering {
  import TextReflowF._
  import watrmarks.{StandardLabels => LB}

  def escapeLineFormatting: TextReflowT => TextReflowT = {
    case l @ Labeled (labels, a)     =>
      if (l.hasLabel(LB.Sup))      { Bracket("^{", "}",  a) }
      else if (l.hasLabel(LB.Sub)) { Bracket("_{", "}",  a) }
      else                         { l }

    case t      => t
  }

  def renderText(t: TextReflowF[(TextReflow, String)]): String = t match {
    case Atom    (ac, ops)               => ops.toString
    case Insert  (value)                 => value
    case Rewrite ((from, attr), to)      => to
    case Bracket (pre, post, (a, attr))  => s"$pre${attr}$post"
    case Mask    (mL, mR, (a, attr))     => attr.drop(mL).dropRight(mR).mkString
    case Flow    (atomsAndattrs)         => atomsAndattrs.map(_._2).mkString
    case Labeled (labels, (a, attr))     => attr
    case CachedText((a, attr), text)     => text
  }

  // object VisualLine {

  //   def escapeString(s: TextReflow, subs: Seq[(Char, String)]): TextReflow = {
  //     ???
  //   }

  //   def escapeTex(s: TextReflow): TextReflow = {
  //     val subs = Seq(
  //       ('_' -> "\\_"),
  //       ('^' -> "\\^"),
  //       ('{' -> "\\{"),
  //       ('}' -> "\\}")
  //     )
  //     escapeString(s, subs)
  //   }

  //   def escapeJson(s: TextReflow): TextReflow = {
  //     val subs = Seq(
  //       ('"' -> "\\\""),
  //       ('\\' -> "\\\\")
  //     )
  //     escapeString(s, subs)
  //   }

  //   def getDescendantLabels(cc: Component): Set[Label] = {
  //     cc.getLabels ++ (
  //       cc.getDescendants(LB.TextSpan)
  //         .map(_.getLabels)
  //         .reduce(_ ++ _)
  //     )
  //   }

  //   def hasTex(cc: Component): Boolean = {
  //     val lls = getDescendantLabels(cc)
  //     !(lls intersect Set(LB.Sup, LB.Sub)).isEmpty
  //   }

  //   def childSpansOrAtoms(cc: Component): Seq[Component] = {
  //     if (cc.hasChildren(LB.TextSpan)) {
  //       cc.getChildren(LB.TextSpan)
  //     } else {
  //       cc.queryAtoms()
  //     }
  //   }

  //   def hasLabel(cc: Component, l: Label) = cc.getLabels.contains(l)
  //   def isSup(cc: Component) = hasLabel(cc, LB.Sup)
  //   def isSub(cc: Component) = hasLabel(cc, LB.Sub)
  //   // def isToken(cc: Component) = hasLabel(cc, LB.Token)
  //   def isTokenized(cc: Component) = hasLabel(cc, LB.Tokenized)

  //   def surroundCC(cc: Component, b: TextReflow): TextReflow = {
  //     if (isSup(cc)) {
  //       bracket("^{", "}", b)
  //     }
  //     else if (isSub(cc)) {
  //       bracket("_{", "}", b)
  //     }
  //     else b
  //   }
  // }


  // object PageAtom {
  //   import EnrichGeometricFigures._
  //   import utils.EnrichNumerics._


  //   def boundsBox(c: Component): TB.Box = {
  //     vjoin()(
  //       c.chars.box,
  //       vcat(center1)(Seq(
  //         c.bounds.top.pp,
  //         c.bounds.left.prettyPrint +| c.bounds.right.prettyPrint,
  //         c.bounds.bottom.prettyPrint
  //       )),
  //       s"(w=${c.bounds.width.prettyPrint}, ctr:${c.bounds.toCenterPoint})"
  //     )
  //   }

  // }

}
