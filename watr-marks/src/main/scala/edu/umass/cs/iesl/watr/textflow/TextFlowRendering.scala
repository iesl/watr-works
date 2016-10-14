package edu.umass.cs.iesl.watr
package textflow

// import acyclic.file

// import scala.collection.mutable
import textboxing.{TextBoxing => TB}
// import TypeTags._

import scalaz.@@

import spindex._
import watrmarks.{StandardLabels => LB}

import watrmarks._
// import ComponentOperations._
// import ComponentTypeEnrichments._
// import GeometricFigure._
// import ComponentReflow._
// import TextFlow._
import GeneralizedReflow._

object TextFlowRendering {
  import TB._
  import utils.ScalazTreeImplicits._

  object Tx {
    import Reflow._

    def append(l:String, b: Reflow): Reflow = {
      flows(l.map(atom(_)))
    }
    def prepend(l:String, b: Reflow): Reflow = {
      // val lpad = FlowUnit.Insert(l)
      // Reflow(lpad +: b.flow)
      ???
    }

    def bracket(l:Char, r:Char, b: Reflow): Reflow = {
      bracket(l.toString(), r.toString(), b)
    }

    def bracket(l:String, r:String, b: Reflow): Reflow = {
      append(r, prepend(l, b))
    }

    private def mkPad(s: String): Reflow = ??? // Reflow(Seq(FlowUnit.Insert(s)))

    def join(sep:String)(bs:Reflow*): Reflow =
      joins(sep)(bs.toSeq)

    def joins(sep:String)(bs:Seq[Reflow]): Reflow =
      concat(bs.toList intersperse mkPad(sep))

    def concat(bs: Seq[Reflow]): Reflow = {
      // val flowUnits = bs.map(unzipReflow(_)).flatten
      // Reflow(flowUnits.map(_._2))
      ???
    }
  }

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


    // def dquote(b: Reflow): Reflow = bracket('"', '"', b)
    // def squareBracket(b: Reflow): Reflow = bracket('[', ']', b)
    // def curlyBrace(b: Reflow): Reflow = bracket('{', '}', b)


    def escapeString(s: Reflow, subs: Seq[(Char, String)]): Reflow = {
      // val submap = subs.map(kv => (kv._1.toString(), kv._2)).toMap
      // foldMapReflow(s, {case (textUnit, flowUnit) =>
      //                   val tsub = submap.get(textUnit).getOrElse(textUnit)
      //                   (tsub, flowUnit)
      //                 })
      ???
    }

    def escapeTex(s: Reflow): Reflow = {
      val subs = Seq(
        ('_' -> "\\_"),
        ('^' -> "\\^"),
        ('{' -> "\\{"),
        ('}' -> "\\}")
      )
      escapeString(s, subs)
    }

    def escapeJson(s: Reflow): Reflow = {
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

    // def doEscapeAndQuote(cc: Component, s: Reflow): Reflow = {
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


    def render(cc: Component): Option[Reflow] = {
      cc.roleLabel match {
        case LB.VisualLine
           | LB.TextSpan =>
          val children = childSpansOrAtoms(cc)
          val childReflows = children.map(render(_))
          val joined = if (isTokenized(cc)) {
            Tx.joins(" ")(childReflows.flatten)
          } else {
            Tx.concat(childReflows.flatten)
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
          val textFlow = element[Component](cc, _.chars)

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

    def surroundCC(cc: Component, b: Reflow): Reflow = {
      if (isSup(cc)) {
        Tx.bracket("^{", "}", b)
      }
      else if (isSub(cc)) {
        Tx.bracket("_{", "}", b)
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
