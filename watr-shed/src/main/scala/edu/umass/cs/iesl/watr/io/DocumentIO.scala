package edu.umass.cs.iesl.watr
package format

import watrmarks._

import TypeTags._
import textboxing.{TextBoxing => TB}
import watrmarks.{StandardLabels => LB}
import spindex._

import ComponentRendering._

object DocumentIO {
  import ComponentOperations._
  import IndexShapeOperations._
  // import ComponentTypeEnrichments._
  import BioLabeling._

  import TB._

  def selectPinForLabel(lb: Label, n: BioNode): BioPin = {
    n.pins
      .filter(p => p.label==lb)
      .head
  }

  def serializeLabeling(label: Label, spine: Seq[BioNode]): Seq[Box] = {
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

    // vjoinTrailSep(sep=",")(spanBoxes:_*)
    spanBoxes
  }

  def serializeDocument(zoneIndexer: ZoneIndexer): String = {

    implicit val initState = Option(CCRenderState(
      numOfPages = zoneIndexer.getPages.length,
      startingPage = PageID(0)
    ))

    val lineSpine = zoneIndexer.bioSpine("TextBlockSpine")

    val serComponents = List(
      LB.SectionHeadingLine,
      LB.ParaBegin,
      LB.TextBlock,
      LB.Abstract,
      LB.AbstractHeading
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
    val joinedLabels =  vjoinTrailSep(left, ",")(serComponents.flatten:_*)


    val tokenDict = initState.map { state =>
      val tokLines = state.tokens
        .map({case (pg, tok, bb) => s"[${tok},[${pg}, ${bb.compactPrint}]]".box })
        .grouped(10)
        .map(group => hjoin(sep=",")(group:_*))
        .toList

      indent()(vjoinTrailSep(left, ",")(tokLines:_*))
    } getOrElse nullBox



    (s"""|{ "labels": [
         |${indent(4)(joinedLabels)}
         |  ],
         |  "lines": [
         |${indent(4)(joinedLines)}
         |  ],
         |  "ids": [
         |${indent()(tokenDict)}
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



  def charInfosBox(cbs: Seq[CharAtom]): Seq[TB.Box] = {
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



  import java.io.InputStream
  import watrmarks._
  import play.api.libs.json._
  import java.io.InputStream
  import play.api.libs.json._

  import spindex._
  import extract.PdfTextExtractor

  import utils.IdGenerator


  def extractChars(
    pdfis: InputStream,
    charsToDebug: Set[Int] = Set()
  ): Seq[(PageAtoms, PageGeometry)] = {

    val charExtractor = new PdfTextExtractor(
      charsToDebug,
      IdGenerator[RegionID]() //, IdGenerator[PageID]
    )
    val _ = charExtractor.extractCharacters(pdfis)

    val pageInfos = charExtractor.pagesInfo

    pageInfos.map({ case (pchars, pgeom) =>
      (pchars, pgeom)
    })

  }


  // def serializeDocumentAsSvg(zoneIndexer: ZoneIndexer, artifactPath: Option[String]): String = {

  //   import TB._

  //   def animationStyle = {
  //     """|<svg:style>
  //        |  .path {
  //        |    opacity: 0.2;
  //        |    stroke: cyan;
  //        |    fill: none;
  //        |    stroke-width: 1;
  //        |    stroke-dasharray: 20;
  //        |    stroke-dashoffset: 200;
  //        |    animation: dash 5s linear forwards infinite;
  //        |  }
  //        |
  //        |  // @keyframes dash {
  //        |  //   to {
  //        |  //     stroke-dashoffset: 0;
  //        |  //   }
  //        |  // }
  //        |  .linebox {
  //        |    opacity: 0.3;
  //        |    stroke: blue;
  //        |    stroke-width: 1;
  //        |  }
  //        |  .pagebox {
  //        |    opacity: 0.3;
  //        |    stroke: black;
  //        |    stroke-width: 1;
  //        |  }
  //        |</svg:style>
  //        |""".stripMargin.mbox
  //   }



  //   val lineSpine = zoneIndexer.bioSpine("TextBlockSpine")


  //   val lines = for {
  //     linec <- lineSpine
  //     lineComponent = linec.component
  //   } yield {
  //     val pageId = zoneIndexer.getPageForComponent(lineComponent)

  //     // hjoin(center1, ", ")(renderConnectedComponents(line):_*)


  //   // val allPageLines = for {
  //   //   (pageId, pageLines) <- segmenter.zoneIndexer.getPages zip pageLines
  //   // } yield {
  //     val pageGeom = zoneIndexer.getPageGeometry(pageId)

  //     val sortedYLines = pageLines.map({ line =>
  //       // val lineX = line.bounds.left
  //       // val lineY = line.bounds.top

  //       val xs = line.charComponents.map(_.component.region.bbox.left.pp).mkString(" ")
  //       val ys = line.charComponents.map(_.component.region.bbox.bottom.pp).mkString(" ")
  //       val escChars = escapeXml11(line.chars)

  //       line.tokenizeLine()

  //       val linetext = line.toText.replaceAll("-", "–")
  //       s"""|                <!--
  //           |${linetext} --> <svg:rect class="linebox" x="${line.bounds.left.pp}" y="${line.bounds.top.pp}" width="${line.bounds.width.pp}"  height="${line.bounds.height.pp}" />
  //           |                <svg:text font-size="1" height="${line.bounds.height}" width="${line.bounds.width}"><svg:tspan height="${line.bounds.height}" x="${xs}" y="${ys}">${escChars}</svg:tspan></svg:text>
  //           |""".stripMargin.trim.mbox
  //     })


  //     val readingOrder = s"""M0,0""".box +| hsep(
  //       pageLines.map({ line =>
  //         val c = line.bounds.toCenterPoint
  //         s"""L${c.x.pp},${c.y.pp}""".box
  //       })
  //     )

  //     val readingOrderLine = s"""<svg:path class="path" d="${readingOrder}" />"""


  //     val x = pageGeom.bounds.left
  //     val y = pageGeom.bounds.top
  //     val pwidth = pageGeom.bounds.width
  //     val pheight = pageGeom.bounds.height


  //     val pageRect = s"""|  <svg:rect
  //                        |      page="${pageId}" file="file://${artifactPath.getOrElse("")}"
  //                        |      class="pagebox" x="${x}" y="${y}" width="${pwidth}"  height="${pheight}" />
  //                        |""".stripMargin.trim.mbox


  //     (pageGeom.bounds, pageRect % vcat(sortedYLines) % readingOrderLine)
  //   }

  //   // val totalBounds = zoneIndexer.map(_._1).reduce(_ union _)
  //   val (totalBounds, totalSvg) = allPageLines
  //     .foldLeft({
  //       (LTBounds(0, 0, 0, 0), nullBox)
  //     })({case ((totalBounds, totalSvg), (pageBounds, pageSvg)) =>
  //       val translatedPageBounds = pageBounds.translate(0, totalBounds.bottom)
  //       val newBounds = totalBounds union translatedPageBounds

  //       (newBounds,
  //         (
  //           totalSvg %
  //             s"""<svg:g transform="translate(0, ${totalBounds.bottom.pp})">""".box %
  //             indent(4)(pageSvg) %
  //             """</svg:g>"""))

  //     })

  //   val pwidth = totalBounds.width
  //   val pheight = totalBounds.height

  //   val svgHead = s"""<svg:svg version="1.1" width="${pwidth}px" height="${pheight}px" viewBox="0 0 ${pwidth} ${pheight}" xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">"""

  //   (svgHead % animationStyle % totalSvg % "</svg:svg>").toString
  // }



  // def extractChars(
  //   pdfis: InputStream,
  //   charsToDebug: Set[Int] = Set()
  // ): Seq[(PageAtoms, PageGeometry)] = {

  //   val charExtractor = new PdfTextExtractor(
  //     charsToDebug,
  //     IdGenerator[RegionID]() //, IdGenerator[PageID]
  //   )
  //   val _ = charExtractor.extractCharacters(pdfis)

  //   val pageInfos = charExtractor.pagesInfo

  //   // Use computed page bounds (based on char bounds) rather than reported page bounds
  //   pageInfos.map({ case (pchars, pgeom) =>

  //     val computedBounds =  charBoxesBounds(pchars.regions)

  //     (pchars,
  //       pgeom.copy(bounds = computedBounds)
  //     )

  //   })

  // }

  def modifyZoneLabelName(name: String): Label = {
    val Array(pre, post0) = name.toLowerCase.split("_", 2)
    val post = post0.replace("_", "-")

    Label("bx", s"${pre}:${post}")
  }


  def loadSpatialIndices(jsvalue: JsValue): ZoneIndexer = {
    // jsvalue.validate[ZoneRecords] match {
    //   case JsSuccess(zoneRecords, path) =>
    //     ZoneIndexer.loadSpatialIndices(zoneRecords)
    //   case JsError(err) =>
    //     sys.error(s"error validating zone records: ${err}")
    // }
    ???
  }
}
