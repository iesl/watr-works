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
case class CCRenderState(
  numOfPages: Int,
  startingPage: Int@@PageID = PageID(0),
  // idgen: IdGenerator[TokenID] = IdGenerator[TokenID](),
  tokens:mutable.ArrayBuffer[(Int@@PageID, Int@@ComponentID, LTBounds)] = mutable.ArrayBuffer()
    // tokens:mutable.ArrayBuffer[(Int@@PageID, Int@@TokenID, LTBounds)] = mutable.ArrayBuffer()
) {
  private var currPage: Int@@PageID = startingPage

  def currentPage: Int@@PageID = currPage

  def advancePage(): Int@@PageID = {
    currPage = PageID(PageID.unwrap(currPage)+1)
    currentPage
  }
}


object ComponentRendering {
  import TB._

  def renderConnectedComponents(_cc: Component)(implicit ostate: Option[CCRenderState] = None): Seq[TB.Box] = {

    val subrender = _cc match {
      case cc: ConnectedComponents =>
        val labels = cc.getLabels
        if (labels.contains(LB.VisualLine)) {
          cc.tokenizeLine()

          ostate.map{ state =>
            val currPage = state.currentPage

            val vs = cc.components.map({ c=>
              val tokenId = c.id
              state.tokens.append((currPage, tokenId, c.bounds))
              val trend = renderConnectedComponents(c)
              val token = hcat(trend)

              val quoted = "\"".box+token+"\""
              (quoted,  tokenId.toString.box)
            })

            Seq(
              "[[".box + hsepb(vs.map(_._1), ",") +"]" + ",     " + "[" + hsepb(vs.map(_._2), ",") + s"], ${cc.id}]"
            )

          } getOrElse {
            val vs = cc.components.map({ c=>
              val trend = renderConnectedComponents(c)
              hcat(trend)
            })

            Seq(hsep(vs))
          }


        } else if (labels.contains(LB.Token)) {
          val clabelsx = cc.containedLabels()
          val clabels:Set[Label] = cc.components.map(_.containedLabels).reduce(_++_)
          val texFmtLabels = (clabels intersect Set(LB.Sup, LB.Sub))

          val subs = Seq(
            ('"' -> "\\\""),
            ('\\' -> "\\\\"),
            ('{' -> "\\\\{"),
            ('}' -> "\\\\}")
          ) ++ (if (!texFmtLabels.isEmpty) Seq( // add extra escapes inside tex-formatted tokens
            ('_' -> "\\\\_"),
            ('^' -> "\\\\^")
          ) else Seq())

          val mapped = cc.components.map({c =>
            hcat(
              renderConnectedComponents(
                c.mapChars(subs)))
          })

          if (texFmtLabels.isEmpty) {
            mapped
          } else {
            "{".box +: mapped :+ "}".box
          }

        } else if (labels.contains(LB.Sub)) {
          // println(s"   ${cc.blockRole}")
          val vs = cc.components.flatMap(c =>
            renderConnectedComponents(c)
          )

          Seq("^{".box + hcat(vs) + "}")

        } else if (labels.contains(LB.Sup)) {
          // println(s"   ${cc.blockRole}")
          val vs = cc.components.flatMap(c =>
            renderConnectedComponents(c)
          )

          Seq("_{".box + hcat(vs) + "}")
        } else {
          val vs = cc.components.flatMap(c =>
            renderConnectedComponents(c)
          )
          Seq(hcat(vs))
        }



      case comp: PageComponent => comp.component match {
        case b: CharAtom =>
          Seq(b.bestGuessChar.box)
        case b: ImgAtom =>
          Seq()
      }
    }

    val blockLabels = _cc.getLabels.intersect(Set(LB.Para, LB.SectionHeadingLine))

    if (blockLabels.isEmpty) {
      subrender
    } else {
      val cclabels = hsep(_cc.getLabels.map(_.key.box).toSeq)

      s"""{"labels": [${cclabels}] "lines": [""".box +: subrender :+ "]}".box

    }
  }

  }
