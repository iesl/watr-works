package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable
import textboxing.{TextBoxing => TB}
import watrmarks._
import textflow._
import TypeTags._
import scalaz.@@

import watrmarks.{StandardLabels => LB}

import ComponentOperations._
import ComponentTypeEnrichments._
import GeometricFigure._
import ComponentReflow._
import TextFlow._

// import acyclic.file

object ComponentRendering {
  import TB._
  import utils.ScalazTreeImplicits._

  object BX {
    def bracket(l:Char, r:Char, b: Box): Box = {
      val lb = l.toString.box
      val rb = r.toString.box
      lb + b + rb
    }

    def dquote(b: Box): Box = bracket('"', '"', b)
    def squareBracket(b: Box): Box = bracket('[', ']', b)
    def curlyBrace(b: Box): Box = bracket('{', '}', b)

  }

  object VisualLine {
    import scalaz.std.string._

    def renderRoleTree(c: Component): TB.Box = {
      c.toRoleTree(LB.VisualLine, LB.TextSpan, LB.PageAtom)
        .map(_.toString())
        .draw
    }


    def dquote(b: TextFlow): TextFlow = bracket('"', '"', b)
    def squareBracket(b: TextFlow): TextFlow = bracket('[', ']', b)
    def curlyBrace(b: TextFlow): TextFlow = bracket('{', '}', b)


    def escapeString(s: TextFlow, subs: Seq[(Char, String)]): TextFlow = {
      val submap = subs.map(kv => (kv._1.toString(), kv._2)).toMap
      foldMapTextFlow(s, {case (textUnit, flowUnit) =>
                        val tsub = submap.get(textUnit).getOrElse(textUnit)
                        (tsub, flowUnit)
                      })
    }

    def escapeTex(s: TextFlow): TextFlow = {
      val subs = Seq(
        ('_' -> "\\_"),
        ('^' -> "\\^"),
        ('{' -> "\\{"),
        ('}' -> "\\}")
      )
      escapeString(s, subs)
    }

    def escapeJson(s: TextFlow): TextFlow = {
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

    def doEscapeAndQuote(cc: Component, s: TextFlow): TextFlow = {
      dquote(escapeJson(s))
    }

    def renderWithIDs(cc: Component): TB.Box = {
      // TODO asser that cc is role VisualLine
      val textAndIds = for {
        textSpan  <- cc.getChildren(LB.TextSpan)
        textFlow <- render(textSpan)
      } yield {
        val tokenId = textSpan.id
        (escapeJson(textFlow), tokenId)
      }
      val lineText = hsepb(textAndIds.map(_._1.text.box), ",")
      val lineIDs = hsepb(textAndIds.map(_._2.toString.box), ",")
      val visualLineID = cc.id.toString

      BX.squareBracket(
        hsepb(
          Seq(BX.squareBracket(lineText), BX.squareBracket(lineIDs), visualLineID),
          ","
        )
      )
    }


    def renderWithoutFormatting(cc: Component): Option[TextFlow] = {
      cc.roleLabel match {
        case LB.VisualLine
           | LB.TextSpan =>
          val children = childSpansOrAtoms(cc)
          val childTextFlows = children.map(renderWithoutFormatting(_))
          val joined = if (isTokenized(cc)) {
            joins(" ")(childTextFlows.flatten)
          } else {
            concat(childTextFlows.flatten)
          }

          Some(joined)

        case LB.PageAtom =>
          val ac = cc.asInstanceOf[AtomicComponent]
          val charAtom = ac.pageAtom.asInstanceOf[CharAtom]

          val textFlow = charAtom.wonkyCharCode
            .map({_ =>
              TextFlow(Seq(FlowUnit.Rewrite(FlowUnit.Atom(ac), "")))
            })
            .getOrElse({
              TextFlow(Seq(FlowUnit.Atom(ac)))
            })

          Some(textFlow)

        case _ => sys.error(s"renderCC(${cc}): unmatched roleLabel ${cc.roleLabel}")
      }
    }

    def render(cc: Component): Option[TextFlow] = {
      cc.roleLabel match {
        case LB.VisualLine
           | LB.TextSpan =>
          val children = childSpansOrAtoms(cc)
          val childTextFlows = children.map(render(_))
          val joined = if (isTokenized(cc)) {
            joins(" ")(childTextFlows.flatten)
          } else {
            concat(childTextFlows.flatten)
          }

          Some(surroundCC(cc, joined))

        // case LB.TextSpan =>
        //   //
        //   if (cc.hasLabel(LB.Invisible)) {
        //     None
        //   } else {
        //     val ccBox = cc.getLabels
        //       .find(_ == LB.LineBreakToken)
        //       .map({ label =>
        //         surroundCC(cc, label.value.get)
        //       })
        //       .getOrElse({
        //         val children = childSpansOrAtoms(cc)
        //         val rc = children.map(render(_))
        //         val hsep = if (isTokenized(cc)) " " else ""
        //         val joined = hjoins(sep=hsep)(rc.flatten)

        //         surroundCC(cc, joined)
        //       })

        //     Some(ccBox)
        //   }


        case LB.PageAtom =>
          val textFlow = TextFlow(
            Seq(FlowUnit.Atom(cc.asInstanceOf[AtomicComponent]))
          )


          val esc = escapeTex(textFlow)
          Some(surroundCC(cc, esc))

        case _ => sys.error(s"renderCC(${cc}): unmatched roleLabel ${cc.roleLabel}")
      }

      // None
    }

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

    def surroundCC(cc: Component, b: TextFlow): TextFlow = {
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
