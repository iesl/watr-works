package edu.umass.cs.iesl.watr
package ext

import java.io.InputStream

import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.ExtractionUtils
import pl.edu.icm.cermine.structure.model.BxBounds
import pl.edu.icm.cermine.structure.model.BxChunk
import pl.edu.icm.cermine.structure.model.BxZoneLabel
import scala.collection.JavaConversions._
import scala.collection.mutable
import com.itextpdf.text.pdf.DocumentFont

import watrmarks.dom._
import watrmarks._
import better.files._

object ITextPdfToSvg extends App {

  def argsToMap(args: Array[String]): Map[String, List[String]] = {
    import scala.collection.mutable.ListMap
    val argmap = ListMap[String, List[String]]()
    args.foldLeft(argmap){ (m, k: String) =>
        val ss: Seq[Char] = k
        ss match {
          case Seq('-', '-', opt @ _*) => m.put(opt.toString, List[String]())
          case Seq('-', opt @ _*) => m.put(opt.toString, List[String]())
          case opt @ _ => m.put(m.head._1, m.head._2 ++ List[String](opt.toString))
        }
        m
    }
    Map[String, List[String]](argmap.toList.reverse: _*)
  }

  // val argMap = argsToMap(args)


  val fin = File(args(0))
  if (fin.isDirectory) {
    println(s"processing dir $fin")
    val m = fin.glob("*.pdf")

    m.foreach { f =>
      val output = s"${f.path}.svg".toFile
      println(s"$f -> $output")


      f.inputStream.map { is =>
        val wdom = itextUtil.itextPdfToSvg(is)
        output < wdom.toSvg()
      }
    }

  } else if (fin.isReadable) {
    val output = s"${fin.path}.svg".toFile

    fin.inputStream.map { is =>
      val wdom = itextUtil.itextPdfToSvg(is)
      output < wdom.toSvg()
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
      font.getSubfamily,
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
    s"""label=$l, ${l.name()}  category = $cat, ${cat.name}, general = $gen, ${gen.name}"""
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

  def itextPdfToSvg(pdfis: InputStream): WatrDom = {

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
    fontDict.foreach {
      case (key, font) =>
        reportFontInfo(font)
    }

    // to svg...
    val pages = d4.asPages().toList
    var accum: TreeLoc[WatrElement] = null

    val pageBounds = mutable.ArrayBuffer[BxBounds]()

    pages.zipWithIndex.foreach {
      case (page, pagenum) =>
        println(s"page...$pagenum")
        val spatialInfo = pageSpatialInfo(pagenum)

        pageBounds.append(page.getBounds)

        if (accum == null) {
          accum = Tree.leaf(Grp(List(watrmarks.Matrix(1, 0, 0, -1, 0, 0))).asInstanceOf[WatrElement]).loc
        } else {
          accum = accum.insertRight(Tree.leaf(Grp(List(watrmarks.Matrix(1, 0, 0, -1, 0, 0)))))
        }

        page.iterator().foreach { zone =>
          println(s"zone... ${formatLabel(zone.getLabel)}")

          val zoneBounds = zone.getBounds

          val n = Grp(List())
          accum = accum.insertDownLast(Tree.leaf(n))

          spatialInfo.labelBbox(bxBoundsToSpatialBounds(zone.getBounds), modifyZoneLabelName(zone.getLabel.name))
          zone.iterator().toList.foreach { line =>
            spatialInfo.labelBbox(bxBoundsToSpatialBounds(line.getBounds), "layout:line")

            accum = accum.insertDownLast(Tree.leaf(Text(List(watrmarks.Scale(1, -1)))))

            line.iterator().toList.foreach { word =>

              accum = accum.insertDownLast(Tree.leaf(TSpanAttribs()))

              spatialInfo.labelBbox(bxBoundsToSpatialBounds(line.getBounds), "layout:word")

              word.iterator.foreach { chunk =>
                val c = chunk.toText().head
                val x = chunk.getX
                val y = chunk.getY
                val w = chunk.getWidth
                val h = chunk.getHeight

                val currentTSpan = accum.getLabel.asInstanceOf[TSpanAttribs]

                accum = accum.modifyLabel { init =>
                  val attribs = init.asInstanceOf[TSpanAttribs]

                  attribs.copy(
                    chars = c +: attribs.chars,
                    xs = x +: attribs.xs,
                    ys = y +: attribs.ys,
                    widths = w +: attribs.widths,
                    heights = h +: attribs.heights
                  // fonts  = w :: attribs.fonts
                  )
                }
                // println(s"chunk: ${chunk.toText()} ")
                // create tspan:
              }
              accum = accum.parent.get
            }
            accum = accum.parent.get
          }
          accum = accum.parent.get
        }
    }

    // val (x, y, w, h) = pageBounds.foldLeft((0d, 0d,0d,0d)){
    //   case ((x, y, w, h), bounds) =>
    //     (bounds.getX,
    //       bounds.getY,
    //       bounds.getWidth,
    //       h+bounds.getHeight)
    // }

    // val heights = pageBounds.map{ _.getHeight }
    // val widths = pageBounds.map{ _.getWidth }
    // val hoffsets = heights.reverse.tails.map(_.sum).toList.reverse
    val heights = pageSpatialInfo.map(_.pageBounds.height)
    val widths = pageSpatialInfo.map(_.pageBounds.width)
    val hoffsets = heights.reverse.tails.map(_.sum).toList.reverse

    val docRoot = Document(StandardLabels.bioDict).asInstanceOf[WatrElement]

    val shiftedPages = accum.toForest.zipWithIndex.map {
      case (n, i) =>
        n.rootLabel
        n.loc.modifyLabel { root =>
          val pageGroup = root.asInstanceOf[Grp]
          pageGroup.copy(
            transforms = pageGroup.transforms :+ watrmarks.Translate(1, -hoffsets(i))
          )
        }.toTree
    }

    val svgWrap = Tree.node(Svg(
      width = widths.max,
      height = heights.sum,
      viewBox = ViewBox(0, 0, widths.max, heights.sum),
      List() // getTransforms(elem)
    ), shiftedPages)

    val wdom = WatrDom(Tree.node(docRoot, svgWrap #:: Stream.Empty))

    wdom

  }

}
