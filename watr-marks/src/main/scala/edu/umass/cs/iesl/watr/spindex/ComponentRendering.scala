package edu.umass.cs.iesl.watr
package spindex

import scala.collection.mutable
import scalaz._
// import utils._
import watrmarks._

import TypeTags._
import textboxing.{TextBoxing => TB}
import watrmarks.{StandardLabels => LB}


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
  import ComponentOperations._
  import IndexShapeOperations._
  import ComponentTypeEnrichments._

  import TB._

  def selectPinForLabel(lb: Label, n: BioNode): BioPin = {
    n.pins
      .filter(p => p.label==lb)
      .head
  }
  def isBegin(lb: Label, n: BioNode) = {
    n.pins.exists(p => p.label==lb && (p.isBegin || p.isUnit))
  }

  def hasID(lb: Label, id: Int, n: BioNode) = {
    n.pins.exists(p => p.label==lb && p.id == id)
  }

  def selectBioLabelings(l: Label, seq: Seq[BioNode]): Seq[Seq[BioNode]] = {


    // if (ns.isEmpty) Seq.empty[Seq[BioNode]] else {

    def loop(ns: Seq[BioNode]): Seq[Seq[BioNode]] = {
      var currID: Int = 0
      val atBegin = ns
        .dropWhile({ node => !isBegin(l, node) })

      atBegin.headOption
        .map ({ node =>
          node.pins
            .filter(_.label==l)
            .foreach(p => currID = p.id.unwrap)

          val (yes, after) = atBegin
            .span(node => hasID(l, currID, node))


          yes +: loop(after)
        })
        .getOrElse({
          Seq.empty[Seq[BioNode]]
        })
    }

    loop(seq)
  }

  def serializeLabeling(label: Label, spine: Seq[BioNode]): Box = {
    val labeledSpans = selectBioLabelings(label, spine)

    // serialize a bio labeling

    val spanBoxes = for {
      span <- labeledSpans
    } yield {

      val bioSpan = span
        .map(p => selectPinForLabel(label, p))

      val spanId = bioSpan.head.id
      val compIds = span.map(_.component.id)

      val cids = compIds.mkString(",")

      s"""["${label}", [${cids}], ${spanId}]""".box
    }

    vjoinTrailSep(sep=",")(spanBoxes:_*)
  }

  def serializeDocument(pages: ZoneIndexer): String = {

    implicit val initState = Option(CCRenderState(
      numOfPages = pages.getPages.length,
      startingPage = PageID(0)
    ))

    val lineSpine = pages.bioSpine("TextBlockSpine")

    val serComponents = List(
      LB.SectionHeadingLine
    ).map(l =>
      serializeLabeling(l, lineSpine)
    )


    val lines = for {
      linec <- lineSpine
      line = linec.component
    } yield {
      hjoin(center1, ", ")(renderConnectedComponents(line):_*)
    }

    val joinedLines =  vjoinTrailSep(left, ",")(lines:_*)
    val joinedLabels =  vjoinTrailSep(left, ",")(serComponents:_*)


    val tokenDict = initState.map { state =>
      val tokLines = state.tokens
        .map({case (pg, tok, bb) => s"[${tok},[${pg}, ${bb.compactPrint}]]".box })
        .grouped(10)
        .map(group => hjoin(sep=",")(group:_*))
        .toList

      indent()(vjoinTrailSep(left, ",")(tokLines:_*))
    } getOrElse nullBox



    (s"""|
         |  "labels": [
         |    ${indent(4)(joinedLabels)}
         |  ],
         |{ "lines": [
         |    ${indent(4)(joinedLines)}
         |  ],
         |  "ids": [
         |     ${indent()(tokenDict)}
         |  ]}
         |""".stripMargin)

  }

  def serializeComponent(currentComponent: Component)(implicit ostate: Option[CCRenderState] = None): Unit = {
    import TB._

    // dfs through components:
    def loopDfs[A](_cc: Component, path: List[Component], prefn: (Component, List[Component]) => A): Unit = {
      prefn(_cc, path)

      _cc.children
        .map(c => loopDfs(c, _cc :: path, prefn))
    }


    def fn0(c: Component, p: List[Component]): Unit = {
      val indent = "   "*p.length
      val lls = c.getLabels
      if (!lls.isEmpty) {
        if (lls.contains(LB.VisualLine)) {
          val renderedLine = hsep(renderConnectedComponents(c)).toString
          println(s"""$indent${renderedLine}      [${c.getLabels.mkString(", ")}] ${c.id}""")
        } else {
          println(s"""$indent[${c.getLabels.mkString(", ")}] ${c.id}""")
        }
      }
    }

    loopDfs(currentComponent, Nil, fn0)

  }

  def renderConnectedComponents(_cc: Component)(implicit ostate: Option[CCRenderState] = None): Seq[TB.Box] = {
    import TB._

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
        case b: CharRegion =>
          Seq(b.bestGuessChar.box)
        case b: ImgRegion =>
          Seq()
      }
    }

    val blockLabels = _cc.getLabels.intersect(Set(LB.Para, LB.SectionHeadingLine))

    if (blockLabels.isEmpty) {
      subrender
    } else {
      val cclabels = hsep(_cc.getLabels.map(_.key.box).toSeq)

      s"""{"labels": [${cclabels}], "lines": [""".box +: subrender :+ "]}".box

    }
  }



  def charInfosBox(cbs: Seq[CharRegion]): Seq[TB.Box] = {
    import TB._

    cbs.zip(spaceWidths(cbs))
      .map{ case (c, dist) =>
        (tbox(c.char.toString) +| "->" +| (dist.pp)) %
          c.region.bbox.top.pp %
          (c.region.bbox.left.pp +| c.region.bbox.right.pp) %
          (c.region.bbox.bottom.pp +| "(w:" + c.region.bbox.width.pp + ")")
    }
  }


  def debugLineComponentStats(linecc: ConnectedComponents): Unit = {
    // linecc.components.foreach{_ match {
    //   case cc: ConnectedComponents =>
    //     println(s"""    cc: ${cc.toText} ${cc.bounds.prettyPrint} cc.right: ${cc.bounds.right}""")

    //   case cc: PageComponent =>
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
