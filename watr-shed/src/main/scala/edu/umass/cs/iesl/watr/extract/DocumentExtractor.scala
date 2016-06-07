package edu.umass.cs.iesl.watr
package extract

import watrmarks._
import play.api.libs.json._
import java.io.InputStream
import com.itextpdf.text.pdf.DocumentFont
import play.api.libs.json._
import org.apache.commons.lang3.StringEscapeUtils.escapeXml11

import Bounds._

object DocumentExtractor extends SpatialJsonFormat {

  def extractBBoxesAsSvg(pdfins: InputStream, artifactPath: Option[String]): String = {


    import watrmarks.TB._

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
    val pageLines = segmenter.pageSegAccum.pageLines

    val allPageLines = for {
      (pageId, pageLines) <- segmenter.pages.getPages zip pageLines
    } yield {
      val pageGeom = segmenter.pages.getPageGeometry(pageId)

      val sortedYLines = pageLines.map({ line =>
        // val lineX = line.bounds.left
        // val lineY = line.bounds.top

        val xs = line.charComponents.map(_.component.bbox.left.pp).mkString(" ")
        val ys = line.charComponents.map(_.component.bbox.bottom.pp).mkString(" ")
        val escChars = escapeXml11(line.chars)


        val linetext = line.tokenizeLine().toText.replaceAll("-", "–")
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

    // val totalBounds = pages.map(_._1).reduce(_ union _)
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
  ): Seq[(PageChars, PageGeometry)] = {

    val charExtractor = new XITextCharacterExtractor(
      charsToDebug,
      IdGenerator[CharID]() //, IdGenerator[PageID]
    )
    val _ = charExtractor.extractCharacters(pdfis)

    val pageInfos = charExtractor.pagesInfo

    // Use computed page bounds (based on char bounds) rather than reported page bounds
    pageInfos.map({ case (pchars, pgeom) =>

      val computedBounds =  charBoxesBounds(pchars.chars)

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

  // import scala.collection.mutable

  // def cermineZonesToJson(pdfis: InputStream): String = {

  //   val (bxDoc, zoneRecords) = extractDocumentZones(pdfis)


  //   def formatBounds(bounds: BxBounds): String = {
  //     val x = bounds.getX
  //     val y = bounds.getY
  //     val w = bounds.getWidth
  //     val h = bounds.getHeight
  //     def fmt = (d: Double) => f"${d}%1.2f"
  //     s"""(x:${fmt(x)}, y:${fmt(y)}, w:${fmt(w)}, h:${fmt(h)})"""
  //   }

  //   val zoneIds = IdGenerator[ZoneID]
  //   val labelIds = IdGenerator[LabelID]
  //   val regionIds = IdGenerator[RegionID]

  //   val zones = mutable.ListMap[Int@@ZoneID, Zone]()
  //   val zoneAndLabels = mutable.ArrayBuffer[ZoneAndLabel]()

  //   var zpageNum = -1
  //   var page0Zones = 0
  //   var page0Lines = 0
  //   var page0Tokens = 0
  //   var page0Chars = 0

  //   def addZone(label: Label, target: Int, bboxes: LTBounds*): Zone = {

  //     val zone = Zone(
  //       zoneIds.nextId,
  //       bboxes.toList.map(TargetedBounds(regionIds.nextId, PageID(target), _))
  //     )

  //     zones.put(zone.id, zone)
  //     zoneAndLabels.append(zone.withLabel(label))

  //     zone
  //   }



  //   bxDoc.asPages.zip(zoneRecords.pageGeometries).zipWithIndex.foreach {
  //     case ((page, pageGeometry), pageNum) =>
  //       zpageNum = pageNum

  //       page.iterator.foreach { zone =>

  //         // Adding generic Zone label (output of Document segmentation)
  //         addZone(LB.Block, pageNum, zone.getBounds.toLTBounds)
  //         //  ... as well as adding Document's classification of the zone
  //         addZone(modifyZoneLabelName(zone.getLabel.name), pageNum, zone.getBounds.toLTBounds)

  //         if (pageNum==0) {
  //           page0Zones += 1
  //         }

  //         zone.iterator().toList.foreach { line =>
  //           addZone(LB.Line, pageNum, line.getBounds.toLTBounds)
  //           if (pageNum==0) {
  //             page0Lines += 1
  //           }

  //           line.iterator().toList.foreach { token =>
  //             addZone(LB.Token(token.toText), pageNum, token.getBounds.toLTBounds)
  //             if (pageNum==0) {
  //               page0Tokens += 1
  //             }

  //             // token.iterator().toList.foreach { chunk =>
  //             //   if (pageNum==0) {
  //             //     page0Chars += 1
  //             //   }
  //             //   addZone(Label("bx", "char"), pageNum, chunk.getBounds.toLTBounds)
  //             // }
  //           }
  //         }
  //       }
  //   }

  //   // if (zpageNum >= 0) {
  //   //   println(s"page ${zpageNum}: tokens=${page0Tokens}, chars=${page0Chars} lines=${page0Lines}")
  //   // }

  //   val z0 = zoneRecords.copy(
  //     zones = zones.values.toList,
  //     labels = zoneAndLabels.toList
  //   )

  //   val zoneObj = Json.toJson(z0)

  //   Json.prettyPrint(zoneObj)
  // }

  def loadSpatialIndices(jsvalue: JsValue): ZoneIndexer = {
    jsvalue.validate[ZoneRecords] match {
      case JsSuccess(zoneRecords, path) =>
        ZoneIndexer.loadSpatialIndices(zoneRecords)
      case JsError(err) =>
        sys.error(s"error validating zone records: ${err}")
    }
  }




}
