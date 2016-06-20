package edu.umass.cs.iesl.watr
package extract

import watrmarks._
import play.api.libs.json._
import java.io.InputStream
import com.itextpdf.text.pdf.DocumentFont
import play.api.libs.json._
import org.apache.commons.lang3.StringEscapeUtils.escapeXml11

import spindex._

import IndexShapeOperations._
import ComponentOperations._

import utils.IdGenerator
import textboxing.{TextBoxing => TB}

object DocumentExtractor extends ComponentDataTypeFormats {

  def extractBBoxesAsSvg(pdfins: InputStream, artifactPath: Option[String]): String = {

    import TB._

    def animationStyle = {
      """|<svg:style>
         |  .path {
         |    opacity: 0.2;
         |    stroke: cyan;
         |    fill: none;
         |    stroke-width: 1;
         |    stroke-dasharray: 20;
         |    stroke-dashoffset: 200;
         |    animation: dash 5s linear forwards infinite;
         |  }
         |
         |  // @keyframes dash {
         |  //   to {
         |  //     stroke-dashoffset: 0;
         |  //   }
         |  // }
         |  .linebox {
         |    opacity: 0.3;
         |    stroke: blue;
         |    stroke-width: 1;
         |  }
         |  .pagebox {
         |    opacity: 0.3;
         |    stroke: black;
         |    stroke-width: 1;
         |  }
         |</svg:style>
         |""".stripMargin.mbox
    }




    val segmenter = segment.DocumentSegmenter.createSegmenter(pdfins)
    segmenter.runPageSegmentation()
    val pageLines = segmenter.visualLineOnPageComponents

    val allPageLines = for {
      (pageId, pageLines) <- segmenter.zoneIndexer.getPages zip pageLines
    } yield {
      val pageGeom = segmenter.zoneIndexer.getPageGeometry(pageId)

      val sortedYLines = pageLines.map({ line =>
        // val lineX = line.bounds.left
        // val lineY = line.bounds.top

        val xs = line.charComponents.map(_.component.region.bbox.left.pp).mkString(" ")
        val ys = line.charComponents.map(_.component.region.bbox.bottom.pp).mkString(" ")
        val escChars = escapeXml11(line.chars)

        line.tokenizeLine()

        val linetext = line.toText.replaceAll("-", "–")
        s"""|                <!--
            |${linetext} --> <svg:rect class="linebox" x="${line.bounds.left.pp}" y="${line.bounds.top.pp}" width="${line.bounds.width.pp}"  height="${line.bounds.height.pp}" />
            |                <svg:text font-size="1" height="${line.bounds.height}" width="${line.bounds.width}"><svg:tspan height="${line.bounds.height}" x="${xs}" y="${ys}">${escChars}</svg:tspan></svg:text>
            |""".stripMargin.trim.mbox
      })


      val readingOrder = s"""M0,0""".box +| hsep(
        pageLines.map({ line =>
          val c = line.bounds.toCenterPoint
          s"""L${c.x.pp},${c.y.pp}""".box
        })
      )

      val readingOrderLine = s"""<svg:path class="path" d="${readingOrder}" />"""


      val x = pageGeom.bounds.left
      val y = pageGeom.bounds.top
      val pwidth = pageGeom.bounds.width
      val pheight = pageGeom.bounds.height


      val pageRect = s"""|  <svg:rect
                         |      page="${pageId}" file="file://${artifactPath.getOrElse("")}"
                         |      class="pagebox" x="${x}" y="${y}" width="${pwidth}"  height="${pheight}" />
                         |""".stripMargin.trim.mbox


      (pageGeom.bounds, pageRect % vcat(sortedYLines) % readingOrderLine)
    }

    // val totalBounds = zoneIndexer.map(_._1).reduce(_ union _)
    val (totalBounds, totalSvg) = allPageLines
      .foldLeft({
        (LTBounds(0, 0, 0, 0), nullBox)
      })({case ((totalBounds, totalSvg), (pageBounds, pageSvg)) =>
        val translatedPageBounds = pageBounds.translate(0, totalBounds.bottom)
        val newBounds = totalBounds union translatedPageBounds

        (newBounds,
          (
            totalSvg %
              s"""<svg:g transform="translate(0, ${totalBounds.bottom.pp})">""".box %
              indent(4)(pageSvg) %
              """</svg:g>"""))

      })

    val pwidth = totalBounds.width
    val pheight = totalBounds.height

    val svgHead = s"""<svg:svg version="1.1" width="${pwidth}px" height="${pheight}px" viewBox="0 0 ${pwidth} ${pheight}" xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">"""

    (svgHead % animationStyle % totalSvg % "</svg:svg>").toString
  }



  def extractChars(
    pdfis: InputStream,
    charsToDebug: Set[Int] = Set()
  ): Seq[(PageAtoms, PageGeometry)] = {

    val charExtractor = new XITextCharacterExtractor(
      charsToDebug,
      IdGenerator[RegionID]() //, IdGenerator[PageID]
    )
    val _ = charExtractor.extractCharacters(pdfis)

    val pageInfos = charExtractor.pagesInfo

    // Use computed page bounds (based on char bounds) rather than reported page bounds
    pageInfos.map({ case (pchars, pgeom) =>

      val computedBounds =  charBoxesBounds(pchars.regions)

      (pchars,
        pgeom.copy(bounds = computedBounds)
      )

    })

  }

  def modifyZoneLabelName(name: String): Label = {
    val Array(pre, post0) = name.toLowerCase.split("_", 2)
    val post = post0.replace("_", "-")

    Label("bx", s"${pre}:${post}")
  }

  def getFontID(fullFontName: String, fontDict: Map[String, (Long, DocumentFont)]): Long = {
    val maybeFont = fontDict.getOrElse(fullFontName, sys.error(s"no font found with fullname =${fullFontName}"))
    maybeFont._1
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
