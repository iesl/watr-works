package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable
import scalaz._
import utils._
import watrmarks._

import TypeTags._
import textboxing.{TextBoxing => TB}
import watrmarks.{StandardLabels => LB}


case class CCRenderState(
  numOfPages: Int,
  startingPage: Int@@PageID = PageID(0),
  idgen: IdGenerator[TokenID] = IdGenerator[TokenID](),
  tokens:mutable.ArrayBuffer[(Int@@PageID, Int@@TokenID, LTBounds)] = mutable.ArrayBuffer()
) {
  private var currPage: Int@@PageID = startingPage

  def currentPage: Int@@PageID = currPage

  def advancePage(): Int@@PageID = {
    currPage = PageID(PageID.unwrap(currPage)+1)
    currentPage
  }
}

object ComponentRendering {
  import ComponentOps._
  import IndexShapeEnrichments._
  import ComponentTypeEnrichments._


  def renderConnectedComponents(_cc: Component)(implicit ostate: Option[CCRenderState] = None): Seq[TB.Box] = {
    import TB._

    _cc match {
      case cc: ConnectedComponents =>
        cc.blockRole.map{ _ match {
          case LB.Line =>
            renderConnectedComponents(cc.tokenizeLine())

          case LB.Page =>
            // should be a set of blocks
            val vs = cc.components.flatMap({ c=>
              renderConnectedComponents(c)
            })
            Seq(vcat(vs))

          case LB.Column =>
            Seq(
              vcat(cc.components.flatMap({ c=>
                renderConnectedComponents(c)
              })))


          case LB.TokenizedLine =>
            // println(s"   ${cc.blockRole}")
            ostate.map{ state =>
              val currPage = state.currentPage

              val vs = cc.components.map({ c=>
                val tokenId = state.idgen.nextId
                state.tokens.append((currPage, tokenId, c.bounds))
                val trend = renderConnectedComponents(c)
                val token = hcat(trend)

                val quoted = "\"".box+token+"\""
                (quoted,  tokenId.toString.box)
              })

              Seq(
                "[[".box + hsepb(vs.map(_._1), ",") +"]" + ",     " + "[" + hsepb(vs.map(_._2), ",") +"]]"
              )

            } getOrElse {
              val vs = cc.components.map({ c=>
                val trend = renderConnectedComponents(c)
                hcat(trend)
              })

              Seq(hsep(vs))
            }


          case LB.Token =>
            // println(s"   ${cc.blockRole}")
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

            // println(s"""| Rendering token: ${cc.chars}
            //             |   ${texFmtLabels}
            //             |   ${mapped.map(_.chars).mkString(" ")}
            //             |""".stripMargin)

            if (texFmtLabels.isEmpty) {
              mapped
            } else {
              "{".box +: mapped :+ "}".box
            }

          case LB.Sup   =>
            // println(s"   ${cc.blockRole}")
            val vs = cc.components.flatMap(c =>
              renderConnectedComponents(c)
            )

            Seq("^{".box + hcat(vs) + "}")

          case LB.Sub   =>
            // println(s"   ${cc.blockRole}")
            val vs = cc.components.flatMap(c =>
              renderConnectedComponents(c)
            )

            Seq("_{".box + hcat(vs) + "}")

          case LB.Block =>
            // println(s"   ${cc.blockRole}")
            val vs = cc.components.map(c =>
              hcat(renderConnectedComponents(c))
            )


            Seq(
              s"""{"labels": ["body"], "lines": [""".box %
                indent(4)(vjoinTrailSep(left, ",")(vs:_*)) %
              "]}".box
            )

          case LB.Para  => ???
          case LB.Image => ???
          case LB.Table => ???
          case x =>
            println(s"  ??? ${cc.blockRole}")
            val vs = cc.components.flatMap(c =>
              renderConnectedComponents(c)
            )

            Seq(hcat(vs))
        }} getOrElse {
          val vs = cc.components.flatMap(c =>
            renderConnectedComponents(c)
          )

          Seq(hcat(vs))
        }


      case charcomp: CharComponent =>
        Seq(charcomp.component.bestGuessChar.box)
    }
  }



  def charInfosBox(cbs: Seq[CharBox]): Seq[TB.Box] = {
    import TB._

    cbs.zip(spaceWidths(cbs))
      .map{ case (c, dist) =>
        (tbox(c.char.toString) +| "->" +| (dist.pp)) %
          c.bbox.top.pp %
          (c.bbox.left.pp +| c.bbox.right.pp) %
          (c.bbox.bottom.pp +| "(w:" + c.bbox.width.pp + ")")
    }
  }


  def debugLineComponentStats(linecc: ConnectedComponents): Unit = {
    // linecc.components.foreach{_ match {
    //   case cc: ConnectedComponents =>
    //     println(s"""    cc: ${cc.toText} ${cc.bounds.prettyPrint} cc.right: ${cc.bounds.right}""")

    //   case cc: CharComponent =>
    //     println(s"""    c:  ${cc.toText} ${cc.bounds.prettyPrint} cc.right: ${cc.bounds.right}""")

    // }}
    val firstCC = linecc.components.head
    linecc.components.sliding(2).foreach{_ match {
      case Seq(c1, c2) =>
        val totalBBox = firstCC.bounds.union(c2.bounds)
        println(s"""| ${c1.toText} - ${c2.toText}
                    |    ${c1.bounds.prettyPrint} - ${c2.bounds.prettyPrint}
                    |    c1.left: ${c1.bounds.left} c1.right: ${c1.bounds.right} c2.left: ${c2.bounds.left}
                    |    dist = ${c2.bounds.left} - ${c1.bounds.right} = ${c2.bounds.left - c1.bounds.right}
                    |    totalBBox = ${totalBBox.prettyPrint}, bb.right:${totalBBox.right.pp}
                    |""".stripMargin)
      case Seq(c1) =>
    }}

  }

  def printCCStats(component: Component, range: (Int, Int), centerY: Double): Unit = {
    import TB._

    val stats = component.children.zip(pairwiseSpaceWidths(component.children))
      .drop(range._1)
      .take(range._2).map({case (c, dist) =>
        (tbox(c.toText) +| "->" +| (dist.pp)) %
          c.bounds.top.pp %
          (c.bounds.left.pp +| c.bounds.right.pp) %
          (c.bounds.bottom.pp +| "(w:" +| c.bounds.width.pp)
      }).toList

    println(
      hsep(stats)
    )
  }
}
