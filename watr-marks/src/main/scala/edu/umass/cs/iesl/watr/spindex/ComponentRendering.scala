package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable
import textboxing.{TextBoxing => TB}
import watrmarks._
import TypeTags._
import scalaz.@@

import watrmarks.{StandardLabels => LB}

import ComponentOperations._
import ComponentTypeEnrichments._
import GeometricFigure._

import acyclic.file

object ComponentRendering {
  import TB._

  object VisualLine {
    import scalaz.std.string._

    def renderRoleTree(c: Component): TB.Box = {
      c.toRoleTree(LB.VisualLine, LB.TextSpan, LB.PageAtom)
        .map(_.toString())
        .drawTree
    }

    def bracket(l:Char, r:Char, b: Box): Box = {
      val lb = l.toString.box
      val rb = r.toString.box
       lb + b + rb
    }
    def dquote(b: Box): Box = bracket('"', '"', b)
    def squareBracket(b: Box): Box = bracket('[', ']', b)
    def curlyBrace(b: Box): Box = bracket('{', '}', b)


    def escapeString(s: String, subs: Seq[(Char, String)]): String = {
      val submap = subs.toMap
      s.map({ch => submap.get(ch).getOrElse(ch.toString) })
        .mkString
    }
    def escapeTex(s: String): String = {
      val subs = Seq(
        ('_' -> "\\_"),
        ('^' -> "\\^"),
        ('{' -> "\\{"),
        ('}' -> "\\}")
      )
      escapeString(s, subs)
    }
    def escapeJson(s: String): String = {
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

    def doEscapeAndQuote(cc: Component, s: TB.Box): TB.Box = {
      dquote(escapeJson(s.toString()))
    }

    def renderWithIDs(cc: Component): TB.Box = {
      val textAndIds = for {
        tokenizedChild <- cc.getDescendants(LB.TextSpan).find(_.hasLabel(LB.Tokenized)).toSeq
        textSpan <- tokenizedChild.getChildren(LB.TextSpan)
        tokenBox <- render(textSpan)
      } yield {
        val tokenId = textSpan.id
        (doEscapeAndQuote(cc, tokenBox), tokenId)
      }
      val lineText = hsepb(textAndIds.map(_._1), ",")
      val lineIDs = hsepb(textAndIds.map(_._2.toString.box), ",")
      val visualLineID = cc.id.toString

      squareBracket(
        hsepb(
          Seq(squareBracket(lineText), squareBracket(lineIDs), visualLineID),
          ","
        )
      )
    }

    def renderWords(cc: Component): Seq[TB.Box] = {
      cc.roleLabel match {
        case LB.VisualLine =>
          val children = childSpansOrAtoms(cc)
          val rc = children.map(render(_))
          rc.flatten

        case _ => sys.error(s"renderCC(${cc}): unmatched roleLabel ${cc.roleLabel}")
      }
    }
    def render(cc: Component): Option[TB.Box] = {
      cc.roleLabel match {
        case LB.VisualLine =>
          val children = childSpansOrAtoms(cc)
          val rc = children.map(render(_))
          val hsep = if (isTokenized(cc)) " " else ""
          val joined = hjoins(sep=hsep)(rc.flatten)

          Some(surroundCC(cc, joined))

        case LB.TextSpan =>
          if (cc.hasLabel(LB.Invisible)) {
            None
          } else {
            val ccBox = cc.getLabels
              .find(_ == LB.LineBreakToken)
              .map({ label =>
                surroundCC(cc, label.value.get)
              })
              .getOrElse({
                val children = childSpansOrAtoms(cc)
                val rc = children.map(render(_))
                val hsep = if (isTokenized(cc)) " " else ""
                val joined = hjoins(sep=hsep)(rc.flatten)

                surroundCC(cc, joined)
              })

            Some(ccBox)
          }


        case LB.PageAtom =>
          val esc = escapeTex(cc.chars).box
          Some(surroundCC(cc, esc))

        case _ => sys.error(s"renderCC(${cc}): unmatched roleLabel ${cc.roleLabel}")
      }
    }

    def childSpansOrAtoms(cc: Component): Seq[Component] = {
      val sub = cc.getChildren(LB.TextSpan)
      if (sub.isEmpty) cc.queryAtoms()
      else sub
    }

    def hasLabel(cc: Component, l: Label) = cc.getLabels.contains(l)
    def isSup(cc: Component) = hasLabel(cc, LB.Sup)
    def isSub(cc: Component) = hasLabel(cc, LB.Sub)
    // def isToken(cc: Component) = hasLabel(cc, LB.Token)
    def isTokenized(cc: Component) = hasLabel(cc, LB.Tokenized)

    def surroundCC(cc: Component, b: TB.Box): TB.Box = {
      if (isSup(cc)) "^{".box + b + "}"
      else if (isSub(cc)) "_{".box + b + "}"
      else b
    }

  }

  def renderConnectedComponents(cc: Component): Seq[TB.Box] = {

    // val subrender = _cc match {
    //   // case cc: ConnectedComponents =>
    //   case cc: RegionComponent =>
    //     val labels = cc.getLabels
    //     if (labels.contains(LB.Invisible)) {
    //       // skip
    //       Seq()
    //     } else if (labels.contains(LB.VisualLine)) {
    //       cc.tokenizeLine()
    //       // val children = cc.queryFor(Query.Contains, Quantifier.All, LB.Token)
    //       val children = cc.getChildTree(LB.Token)

    //       ostate.map{ state =>
    //         val currPage = state.currentPage

    //         val vs = children.map({ c=>
    //           val tokenId = c.id
    //           state.tokens.append((currPage, tokenId, c.bounds))
    //           val trend = renderConnectedComponents(c)
    //           val token = hcat(trend)

    //           val quoted = "\"".box+token+"\""
    //           (quoted,  tokenId.toString.box)
    //         })

    //         Seq(
    //           "[[".box + hsepb(vs.map(_._1), ",") +"]" + ",     " + "[" + hsepb(vs.map(_._2), ",") + s"], ${cc.id}]"
    //         )

    //       } getOrElse {
    //         val vs = children.map({ c=>
    //           val trend = renderConnectedComponents(c)
    //           hcat(trend)
    //         })

    //         Seq(hsep(vs))
    //       }

    //     } else if (labels.contains(LB.Token)) {
    //       if (labels.contains(LB.LineBreakToken)) {
    //         val lineBreak = labels.find(_ ==LB.LineBreakToken).get
    //         val joinedText = lineBreak.value.get

    //         Seq(joinedText.box)
    //       } else {

    //         val clabels:Set[Label] = cc.components.map(_.containedLabels).reduce(_++_)
    //         val texFmtLabels = (clabels intersect Set(LB.Sup, LB.Sub))

    //         val subs = Seq(
    //           ('"' -> "\\\""),
    //           ('\\' -> "\\\\"),
    //           ('{' -> "\\\\{"),
    //           ('}' -> "\\\\}")
    //         ) ++ (if (!texFmtLabels.isEmpty) Seq( // add extra escapes inside tex-formatted tokens
    //           ('_' -> "\\\\_"),
    //           ('^' -> "\\\\^")
    //         ) else Seq())


    //         val mapped = cc.components.map({c =>
    //           hcat(
    //             renderConnectedComponents(
    //               c.mapChars(subs)))
    //         })


    //         if (texFmtLabels.isEmpty) {
    //           mapped
    //         } else {
    //           "{".box +: mapped :+ "}".box
    //         }
    //       }

    //     } else if (labels.contains(LB.Sub)) {
    //       val vs = cc.components.flatMap(c =>
    //         renderConnectedComponents(c)
    //       )

    //       Seq("_{".box + hcat(vs) + "}")

    //     } else if (labels.contains(LB.Sup)) {
    //       val vs = cc.components.flatMap(c =>
    //         renderConnectedComponents(c)
    //       )

    //       Seq("^{".box + hcat(vs) + "}")
    //     } else {
    //       val vs = cc.components.flatMap(c =>
    //         renderConnectedComponents(c)
    //       )
    //       Seq(hcat(vs))
    //     }



    //   case comp: AtomicComponent => comp.component match {
    //     case b: CharAtom =>
    //       Seq(b.bestGuessChar.box)
    //     case b: ImgAtom =>
    //       Seq()
    //   }
    // }

    // val blockLabels = _cc.getLabels.intersect(Set(LB.Para, LB.SectionHeadingLine))

    // if (blockLabels.isEmpty) {
    //   subrender
    // } else {
    //   val cclabels = hsep(_cc.getLabels.map(_.key.box).toSeq)

    //   s"""{"labels": [${cclabels}] "lines": [""".box +: subrender :+ "]}".box

    // }

    Seq()
  }


  object PageAtom {
    import EnrichGeometricFigures._
    import utils.EnrichNumerics._


    def boundsBox(c: Component): TB.Box = {
      vcat(center1)(Seq(
        c.chars,
        c.bounds.top.pp,
        c.bounds.left.prettyPrint +| c.bounds.right.prettyPrint,
        c.bounds.bottom.prettyPrint,
        "(w=" + c.bounds.width.prettyPrint + ")"
      ))
    }

  }
}
