package edu.umass.cs.iesl.watr
package spindex

import textboxing.{TextBoxing => TB}
import watrmarks._
import watrmarks.{StandardLabels => LB}


object ComponentRendering {
  import TB._
  import utils.ScalazTreeImplicits._
  // import textflow.GeneralizedReflow.componentReflow._
  import textflow.TextReflow._


  object VisualLine {
    import scalaz.std.string._

    def renderRoleTree(c: Component): TB.Box = {
      c.toRoleTree(LB.VisualLine, LB.TextSpan, LB.PageAtom)
        .map(_.toString())
        .draw
    }


    def dquote(b: TextReflow): TextReflow = bracket('"', '"', b)
    def squareBracket(b: TextReflow): TextReflow = bracket('[', ']', b)
    def curlyBrace(b: TextReflow): TextReflow = bracket('{', '}', b)


    def escapeString(s: TextReflow, subs: Seq[(Char, String)]): TextReflow = {
      ???
      // val submap = subs.map(kv => (kv._1.toString(), kv._2)).toMap
      // foldMapTextReflow(s, {case (textUnit, flowUnit) =>
      //   val tsub = submap.get(textUnit).getOrElse(textUnit)
      //   (tsub, flowUnit)
      // })
    }

    def escapeTex(s: TextReflow): TextReflow = {
      val subs = Seq(
        ('_' -> "\\_"),
        ('^' -> "\\^"),
        ('{' -> "\\{"),
        ('}' -> "\\}")
      )
      escapeString(s, subs)
    }

    def escapeJson(s: TextReflow): TextReflow = {
      val subs = Seq(
        ('"' -> "\\\""),
        ('\\' -> "\\\\")
      )
      escapeString(s, subs)
    }

    def getDescendantLabels(cc: Component): Set[Label] = {
      cc.getLabels ++ (
        cc.getDescendants(LB.TextSpan)
          .map(_.getLabels)
          .reduce(_ ++ _)
      )
    }

    def hasTex(cc: Component): Boolean = {
      val lls = getDescendantLabels(cc)
      !(lls intersect Set(LB.Sup, LB.Sub)).isEmpty
    }

    def doEscapeAndQuote(cc: Component, s: TextReflow): TextReflow = {
      dquote(escapeJson(s))
    }

    def renderWithIDs(cc: Component): TB.Box = {
      // TODO assert that cc is role VisualLine
      val textAndIds = for {
        textSpan  <- cc.getChildren(LB.TextSpan)
        textFlow <- toTextReflow(textSpan)
      } yield {
        val tokenId = textSpan.id
        (escapeJson(textFlow), tokenId)
      }
      // val lineText = hsepb(textAndIds.map(_._1.text.box), ",")
      val lineIDs = hsepb(textAndIds.map(_._2.toString.box), ",")
      val visualLineID = cc.id.toString

      // OneRow.squareBracket(
      //   hsepb(
      //     Seq(OneRow.squareBracket(lineText), OneRow.squareBracket(lineIDs), visualLineID),
      //     ","
      //   )
      // )
      ???
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

          Some(joined)

        case LB.PageAtom =>
          val ac = cc.asInstanceOf[AtomicComponent]
          atom(ac).some

        case _ => sys.error(s"renderCC(${cc}): unmatched roleLabel ${cc.roleLabel}")
      }
    }

    // def render(cc: Component): Option[TextReflow] = {
    //   cc.roleLabel match {
    //     case LB.VisualLine
    //        | LB.TextSpan =>
    //       val children = childSpansOrAtoms(cc)
    //       val childTextFlows = children.map(render(_))
    //       val joined = if (isTokenized(cc)) {
    //         joins(" ")(childTextFlows.flatten)
    //       } else {
    //         concat(childTextFlows.flatten)
    //       }

    //       Some(surroundCC(cc, joined))

    //     // case LB.TextSpan =>
    //     //   //
    //     //   if (cc.hasLabel(LB.Invisible)) {
    //     //     None
    //     //   } else {
    //     //     val ccBox = cc.getLabels
    //     //       .find(_ == LB.LineBreakToken)
    //     //       .map({ label =>
    //     //         surroundCC(cc, label.value.get)
    //     //       })
    //     //       .getOrElse({
    //     //         val children = childSpansOrAtoms(cc)
    //     //         val rc = children.map(render(_))
    //     //         val hsep = if (isTokenized(cc)) " " else ""
    //     //         val joined = hjoins(sep=hsep)(rc.flatten)

    //     //         surroundCC(cc, joined)
    //     //       })

    //     //     Some(ccBox)
    //     //   }


    //     case LB.PageAtom =>
    //       // val textFlow = TextReflow(
    //       //   Seq(FlowUnit.Atom(cc.asInstanceOf[AtomicComponent]))
    //       // )
    //       // val esc = escapeTex(textFlow)
    //       // Some(surroundCC(cc, esc))
    //       ???

    //     case _ => sys.error(s"renderCC(${cc}): unmatched roleLabel ${cc.roleLabel}")
    //   }

    //   // None
    // }

    def childSpansOrAtoms(cc: Component): Seq[Component] = {
      if (cc.hasChildren(LB.TextSpan)) {
        cc.getChildren(LB.TextSpan)
      } else {
        cc.queryAtoms()
      }
    }

    def hasLabel(cc: Component, l: Label) = cc.getLabels.contains(l)
    def isSup(cc: Component) = hasLabel(cc, LB.Sup)
    def isSub(cc: Component) = hasLabel(cc, LB.Sub)
    // def isToken(cc: Component) = hasLabel(cc, LB.Token)
    def isTokenized(cc: Component) = hasLabel(cc, LB.Tokenized)

    def surroundCC(cc: Component, b: TextReflow): TextReflow = {
      if (isSup(cc)) {
        bracket("^{", "}", b)
      }
      else if (isSub(cc)) {
        bracket("_{", "}", b)
      }
      else b
    }
  }


  object PageAtom {
    import EnrichGeometricFigures._
    import utils.EnrichNumerics._


    def boundsBox(c: Component): TB.Box = {
      vjoin()(
        c.chars.box,
        vcat(center1)(Seq(
          c.bounds.top.pp,
          c.bounds.left.prettyPrint +| c.bounds.right.prettyPrint,
          c.bounds.bottom.prettyPrint
        )),
        s"(w=${c.bounds.width.prettyPrint}, ctr:${c.bounds.toCenterPoint})"
      )
    }

  }
}
