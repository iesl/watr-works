package edu.umass.cs.iesl.watr
package textreflow

import spindex._

import textboxing.{TextBoxing => TB}
import TB._


object TextReflowRendering {
  import utils.ScalazTreeImplicits._
  import TextReflow._
  import TextReflowOps._
  import TextReflowF._
  val TR = TextReflow
  import watrmarks.{StandardLabels => LB, _}


  import TR._

  val BracketTexFormatting: TextReflowU => TextReflowU = {
    case l @ Labeled (labels, a)     =>
      if (l.hasLabel(LB.Sup))      { Bracket("^{", "}",  a) }
      else if (l.hasLabel(LB.Sub)) { Bracket("_{", "}",  a) }
      else                         { l }

    case t      => t

  }

  def evalFlatText(t: TextReflowF[(TextReflow, String)]): String = {
    t match {
      case Atom    (ac, ops)               => ops.toString
      case Insert  (value)                 => s"$value"
      case Rewrite ((from, attr), to)      => s"$to"
      case Bracket (pre, post, (a, attr))  => s"$pre${attr}$post"
      case Flow    (labels, atomsAndattrs) => s"""${atomsAndattrs.map(_._2).mkString}"""
      case Labeled (labels, (a, attr))     => s"${attr}"
    }
  }

  implicit class RicherTextReflow(val theReflow: TR.TextReflow) extends AnyVal  {
    import matryoshka._
    import matryoshka.data._
    import matryoshka.implicits._

    def toText(): String = {
      val res = theReflow.cata(attributePara(evalFlatText))
      res.toPair._1
    }

    def toFormattedText(): String = {
      val res = theReflow.transCata(BracketTexFormatting)
      res.toText
    }
  }

  object VisualLine {
    import scalaz.std.string._

    def renderRoleTree(c: Component): TB.Box = {
      c.toRoleTree(LB.VisualLine, LB.TextSpan, LB.PageAtom)
        .map(_.toString())
        .drawBox
    }


    // def dquote(b: TextReflow): TextReflow = bracket('"', '"', b)
    // def squareBracket(b: TextReflow): TextReflow = bracket('[', ']', b)
    // def curlyBrace(b: TextReflow): TextReflow = bracket('{', '}', b)


    def escapeString(s: TextReflow, subs: Seq[(Char, String)]): TextReflow = {
      // val submap = subs.map(kv => (kv._1.toString(), kv._2)).toMap
      // foldMapTextReflow(s, {case (textUnit, flowUnit) =>
      //                   val tsub = submap.get(textUnit).getOrElse(textUnit)
      //                   (tsub, flowUnit)
      //                 })
      ???
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

    // def doEscapeAndQuote(cc: Component, s: TextReflow): TextReflow = {
    //   dquote(escapeJson(s))
    // }

    // def renderWithIDs(cc: Component): TB.Box = {
    //   // TODO asser that cc is role VisualLine
    //   val textAndIds = for {
    //     textSpan  <- cc.getChildren(LB.TextSpan)
    //     textFlow <- render(textSpan)
    //   } yield {
    //     val tokenId = textSpan.id
    //     // (doEscapeAndQuote(cc, tokenBox), tokenId)
    //     (escapeJson(textFlow), tokenId)
    //   }
    //   val lineText = hsepb(textAndIds.map(_._1.text.box), ",")
    //   val lineIDs = hsepb(textAndIds.map(_._2.toString.box), ",")
    //   val visualLineID = cc.id.toString

    //   BX.squareBracket(
    //     hsepb(
    //       Seq(BX.squareBracket(lineText), BX.squareBracket(lineIDs), visualLineID),
    //       ","
    //     )
    //   )
    // }


    // def render(cc: Component): Option[TextReflow] = {
    //   cc.roleLabel match {
    //     case LB.VisualLine
    //        | LB.TextSpan =>
    //       val children = childSpansOrAtoms(cc)
    //       val childReflows = children.map(render(_))
    //       val joined = if (isTokenized(cc)) {
    //         joins(" ")(childReflows.flatten)
    //       } else {
    //         concat(childReflows.flatten)
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
    //       val ac = cc.asInstanceOf[AtomicComponent]
    //       val ops = new textreflow.TextReflowAtomOps(ac.chars)
    //       val textFlow = atom(ac, ops)

    //       val esc = escapeTex(textFlow)
    //       Some(surroundCC(cc, esc))

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
