package edu.umass.cs.iesl.watr
package ext


import com.itextpdf.text.pdf.parser.TextRenderInfo
import edu.umass.cs.iesl.watr.watrmarks.{ BioLabel, TextBounds }
import java.io.InputStream

import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.ExtractionUtils
import pl.edu.icm.cermine.structure.model.{ BxBounds, BxZone }
import pl.edu.icm.cermine.structure.model.BxChunk
import pl.edu.icm.cermine.structure.model.BxZoneLabel
import scala.collection.JavaConversions._
import scala.collection.mutable
import com.itextpdf.text.pdf.DocumentFont

import better.files._
import watrmarks.dom

object ITextPdfToSvg extends App {

  import java.io.{File => JFile}

  case class AppConfig(
    file: JFile = new JFile(""),
    force: Boolean = false
  )

  val parser = new scopt.OptionParser[AppConfig]("scopt") {
    head("itext zone extractor", "0.1")


    opt[Unit]('x', "overwrite") action { (v, opts) =>
      opts.copy(force = true) } text("force overwrite of existing files")

    opt[JFile]('f', "file") action { (v, opts) =>
      opts.copy(file = v) } text("input file or directory")

  }


  val config = parser.parse(args, AppConfig()).getOrElse{
    sys.error(parser.usage)
  }

  val f = File(config.file.getPath)

  if (f.isDirectory) {
    f.glob("**/*.pdf").foreach { pdf =>
      val artifactPath = s"${pdf.path}.d".toFile
      val output = s"${artifactPath}/cermine-zones.svg".toFile
      val fileSha1 = DigestUtils.shaHex(pdf.byteArray)

      pdf.inputStream.map { is =>
        if (!output.isReadable || config.force) {
          println(s"processing ${pdf}, force=${config.force}")
          output < itextUtil.itextPdfToSvg(fileSha1, is)
        } else {
          println(s"skipping $pdf")
        }
      }
    }
  } else if (f.isReadable) {

    val artifactPath = s"${f.path}.d".toFile
    val fileSha1 = DigestUtils.shaHex(f.byteArray)

    println("file sha1: "+fileSha1)

    f.inputStream.map { is =>
      val output = s"${artifactPath}/cermine-zones.svg".toFile
      if (!output.isReadable || config.force) {
        println(s"processing ${f}, force=${config.force}")
        output < itextUtil.itextPdfToSvg(fileSha1, is)
      } else {
        println(s"skipping $f")
      }
    }
  } else {
    sys.error("please specify a file or directory")
  }
}

object itextUtil {
  def bxBoundsToSpatialBounds(bounds: BxBounds): spatialindex.Bounds = {
    spatialindex.regions.bbox(
      bounds.getX,
      bounds.getY,
      bounds.getWidth,
      bounds.getHeight
    )
  }
  def reportFontInfo(font: DocumentFont): Unit = {
    val fontFullname = font.getFullFontName.map(_.mkString("[", ",", "]")).mkString(", ")
    val allNameEntries = font.getAllNameEntries.map(_.mkString("[", ",", "]")).mkString(", ")
    val fontDictionary = font.getFontDictionary
    val fontDictionaryKeys = fontDictionary.getKeys.map(_.toString()).mkString(",")
    val unicodeDiffs = font.getUnicodeDifferences.dropWhile(_ == "").mkString(",")
    val diffs = font.getDifferences.dropWhile(_ == null).mkString(",")

    debugReport(
      allNameEntries,
      // font.getCharBBox,
      // font.getFontDictionary,
      fontDictionaryKeys,
      // font.getFontMatrix,
      fontFullname,
      font.getFullFontStream,
      // font.getKerning,
      font.getPostscriptFontName,
      // font.getWidth,
      // font.getWidth,
      font.hasKernPairs,
      font.isVertical,
      // font.getAscent,
      // font.getAscentPoint,
      // font.getCidCode,
      // font.getCodePagesSupported.mkString(", "),
      // font.getCompressionLevel,
      // font.getDescent,
      // font.getDescentPoint,
      font.getEncoding,
      font.getFontType,
      // font.getSubfamily,
      unicodeDiffs,
      diffs,
      // font.getUnicodeEquivalent,
      // font.getWidthPoint,
      // font.getWidthPoint,
      // font.getWidthPointKerned,
      // font.getWidths.mkString(", "),
      font.isDirectTextToByte,
      font.isEmbedded,
      font.isFontSpecific,
      font.isForceWidthsOutput,
      font.isSubset
    )

    // charExists            (Int)           => Boolean
    // convertToBytes        (String)        => Array[Byte]
    // getAllNameEntries     ()              => Array[Array[String]]
    // getCharBBox           (Int)           => Array[Int]
    // getFamilyFontName     ()              => Array[Array[String]]
    // getFontDescriptor     (Int, Float)    => Float
    // getFontDictionary     ()              => PdfDictionary
    // getFontMatrix         ()              => Array[Double]
    // getFullFontName       ()              => Array[Array[String]]
    // getFullFontStream     ()              => PdfStream
    // getKerning            (Int, Int)      => Int
    // getPostscriptFontName ()              => String
    // getWidth              (String)        => Int
    // getWidth              (Int)           => Int
    // hasKernPairs          ()              => Boolean
    // isVertical            ()              => Boolean
    // setKerning            (Int, Int, Int) => Boolean
    // setPostscriptFontName (String)        => Unit

    // com.itextpdf.text.pdf.BaseFont
    // ---------------------------
    // addSubsetRange        (Array[Int])    => Unit
    // correctArabicAdvance  ()              => Unit
    // getAscent             (String)        => Int
    // getAscentPoint        (String, Float) => Float
    // getCidCode            (Int)           => Int
    // getCodePagesSupported ()              => Array[String]
    // getCompressionLevel   ()              => Int
    // getDescent            (String)        => Int
    // getDescentPoint       (String, Float) => Float
    // getDifferences        ()              => Array[String]
    // getEncoding           ()              => String
    // getFontType           ()              => Int
    // getSubfamily          ()              => String
    // getUnicodeDifferences ()              => Array[Char]
    // getUnicodeEquivalent  (Int)           => Int
    // getWidthPoint         (String, Float) => Float
    // getWidthPoint         (Int, Float)    => Float
    // getWidthPointKerned   (String, Float) => Float
    // getWidths             ()              => Array[Int]
    // isDirectTextToByte    ()              => Boolean
    // isEmbedded            ()              => Boolean
    // isFontSpecific        ()              => Boolean
    // isForceWidthsOutput   ()              => Boolean
    // isSubset              ()              => Boolean
    // setCharAdvance        (Int, Int)      => Boolean
    // setCompressionLevel   (Int)           => Unit
    // setDirectTextToByte   (Boolean)       => Unit
    // setFontDescriptor     (Int, Float)    => Unit
    // setForceWidthsOutput  (Boolean)       => Unit
    // setSubset             (Boolean)       => Unit
  }

  def formatLabel(l: BxZoneLabel): String = {
    val cat = l.getCategory
    val gen = l.getGeneralLabel
    s"""label=$l, category = $cat, general = $gen """
  }

  def modifyZoneLabelName(name: String): String = {
    val Array(pre, post0) = name.toLowerCase.split("_", 2)
    val post = post0.replace("_", "-")

    s"${pre}:${post}"
  }

  import scalaz.{Show, TreeLoc, Tree}

  def getFontID(fullFontName: String, fontDict: Map[String, (Long, DocumentFont)]): Long = {
    val maybeFont = fontDict.getOrElse(fullFontName, sys.error(s"no font found with fullname =${fullFontName}"))
    maybeFont._1
  }

  def itextPdfToSvg(fileSha1: String, pdfis: InputStream): String = {

    val conf = new ComponentConfiguration()
    val charExtractor = new XITextCharacterExtractor()
    conf.setCharacterExtractor(charExtractor)

    val d0 = ExtractionUtils.extractCharacters(conf, pdfis)
    val d1 = ExtractionUtils.segmentPages(conf, d0)
    val d2 = ExtractionUtils.resolveReadingOrder(conf, d1);
    val d3 = ExtractionUtils.classifyInitially(conf, d2);
    val d4 = ExtractionUtils.classifyMetadata(conf, d3);

    val pageSpatialInfo = charExtractor.spatialInfo
    val fontDict = charExtractor.fontDict

    // fontDict.foreach {
    //   case (key, font) =>
    //     reportFontInfo(font)
    // }

    def formatBounds(bounds: BxBounds): String = {
      val x = bounds.getX
      val y = bounds.getY
      val w = bounds.getWidth
      val h = bounds.getHeight
      def fmt = (d: Double) => f"${d}%1.2f"
      s"""(x:${fmt(x)}, y:${fmt(y)}, w:${fmt(w)}, h:${fmt(h)})"""
    }



    // <defs class="annotation-boxes"
    //   <g class="annotation-set"
    //      target="sha:ac40d">
    // <!-- paragraph annotation that spans column and page
    //      <g class="annotation"
    //         id="uuid:xx-xx-xx"
    //         label-name="paragraph">
    //         <rect target="p3" x="1.96" y="4.45" width="36.07" height="31.09" />
    //         <rect target="p3" x="1.96" y="4.45" width="36.07" height="31.09" />
    //         <rect target="p4" x="1.96" y="4.45" width="36.07" height="31.09" />
    //      </g>
    //
    // <!-- sentence annotation targeted to paragraph listed above.
    // <!--   sentence crosses all 3 rects
    //      <g class="annotation"
    //         id="uuid:xx-xx-xx"
    //         label-name="pos:sentence">
    //         <rect target="p3" x="1.96" y="4.45" width="36.07" height="31.09" />
    //         <rect target="p3" x="1.96" y="4.45" width="36.07" height="31.09" />
    //         <rect target="p4" x="1.96" y="4.45" width="36.07" height="31.09" />
    //      </g>

    //   </g>
    // </defs>

    val spatialInfo = charExtractor.spatialInfo.toList

    object annotationTags extends ScalatagsDefs {

      def zoneExtents(zone: BxZone) = Seq(
        zone.getBounds.getX.attrX,
        zone.getBounds.getY.attrY,
        zone.getBounds.getWidth.attrWidth,
        zone.getBounds.getHeight.attrHeight)

      def zoneExtents(tb: TextBounds) = Seq(
        tb.left.attrX,
        tb.bottom.attrY,
        tb.width.attrWidth,
        tb.height.attrHeight)

    }


    import annotationTags.texttags._
    import annotationTags._


    var annotationSet = <.g("annotation-set".clazz,
      s"file:$fileSha1".attrTarget
    )

    val pages = d4.asPages().toList

    pages.zip(spatialInfo).zipWithIndex.foreach {
      case ((page, spatial), pagenum) =>

        annotationSet = annotationSet(
          <.rect(
            "bounding-box".clazz,
            s"page-${pagenum}".labelName,
            s"page-${pagenum}".id,
            zoneExtents(spatial.pageBounds)
          ))


        page.iterator().foreach { zone =>
          val zlabel = modifyZoneLabelName(zone.getLabel.name)

          val z = <.g("annotation".clazz, <.rect(
            zlabel.labelName,
            s"page-${pagenum}".attrTarget,
            zoneExtents(zone)
          ))
          annotationSet = annotationSet(z)

          zone.iterator().toList.foreach { line =>

            val z = <.g("annotation".clazz, <.rect(
              "line".labelName,
              s"page-${pagenum}".attrTarget,
              zoneExtents(zone)
            ))
            annotationSet = annotationSet(z)

          }
        }
    }

    val heights = pageSpatialInfo.map(_.pageBounds.height)
    val widths = pageSpatialInfo.map(_.pageBounds.width)
    val hoffsets = heights.reverse.tails.map(_.sum).toList.reverse

    annotationSet.toString()
  }

}


// val spatialInfo = pageSpatialInfo(pagenum)
// spatialInfo.labelBbox(bxBoundsToSpatialBounds(zone.getBounds), modifyZoneLabelName(zone.getLabel.name))
// spatialInfo.labelBbox(bxBoundsToSpatialBounds(line.getBounds), "layout:line")
// spatialInfo.labelBbox(bxBoundsToSpatialBounds(line.getBounds), "layout:word")

// line.iterator().toList.foreach { word =>
//   word.iterator.foreach { chunk =>
//     val c = chunk.toText().head
//     val x = chunk.getX
//     val y = chunk.getY
//     val w = chunk.getWidth
//     val h = chunk.getHeight
//   }
// }
