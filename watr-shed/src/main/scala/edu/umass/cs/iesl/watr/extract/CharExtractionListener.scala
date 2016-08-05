package edu.umass.cs.iesl.watr
package extract

import com.itextpdf.kernel.pdf.PdfPage
import spindex._
import GeometricFigure._

import scala.collection.mutable
import scala.collection.JavaConversions._
// import TypeTags._
import scalaz.{@@}
import util._

import _root_.com.itextpdf
import itextpdf.kernel.geom.{Vector => PVector}
import itextpdf.kernel.pdf.canvas.parser.listener.IEventListener
import itextpdf.kernel.pdf.canvas.parser.EventType
import itextpdf.kernel.pdf.canvas.parser.data._
import itextpdf.kernel.pdf.PdfReader

import fonts._
import UnicodeUtil._
import utils.IdGenerator


object PdfPageObjectOutput {
  import textboxing.{TextBoxing => TB}

  import com.itextpdf.kernel.pdf.tagging.IPdfStructElem

  import TB._

  def fmtelem(e: IPdfStructElem) = {
    s"""struct role = ${e.getRole}""".box
  }

  def renderElemLoc(elem: IPdfStructElem, l:Int=0): Box = {
    val path = renderElemPath(elem)
    val tree = renderElemTree(elem, 0)

    val pboxes = path.reverse.zipWithIndex.map({case (p, i) =>
      indent(i*3)("^" + p)
    })
    val pathBox = vcat(left)(pboxes.toList)

    val treeBox = indent(path.length*3)(
      tree
    )

    pathBox atop treeBox
  }

  def renderElemPath(elem: IPdfStructElem): Seq[Box] = {
    if (elem == null) Seq() else {
      fmtelem(elem) +: renderElemPath(elem.getParent)
    }
  }

  def renderElemTree(elem: IPdfStructElem, l:Int=0): Box = {
    val p = indent(l*3)(
      fmtelem(elem)
    )
    val ks = vcat(left)({
      val kids = elem.getKids
      if (kids!= null) {
        kids.toList.map(renderElemTree(_, l+1))
      } else {
        List()
      }
    })

    p atop ks
  }


}

class CharExtractionListener(
  reader: PdfReader,
  charsToDebug: Set[Int] = Set(),
  componentIdGen: IdGenerator[RegionID],
  currCharBuffer: mutable.ArrayBuffer[PageAtom], // = mutable.ArrayBuffer[PageAtom]()
  pdfPage: PdfPage,
  pageId: Int@@PageID,
  pageGeometry: PageGeometry,
  geomTranslation:GeometryTranslation,
  glyphMap: Map[(String, Int), String]
) extends IEventListener {

  override def getSupportedEvents(): java.util.Set[EventType] ={
    Set(EventType.RENDER_TEXT)
  }

  override def eventOccurred(data: IEventData,  eventType: EventType): Unit = {
    if (eventType.equals(EventType.RENDER_TEXT)) {
      val tri = data.asInstanceOf[TextRenderInfo]
      renderText(tri)
    }
  }


  val charWindow = mutable.MutableList[Char]()




  def lookupGlyph(charTri: TextRenderInfo): Seq[Char] = {
    // Get glyphHash for this char
    val pdfString = charTri.getPdfString
    val font = charTri.getFont
    val fprogram = font.getFontProgram
    val fontNames = fprogram.getFontNames
    val fontName = fontNames.getFontName

    val valueBytes = pdfString.getValueBytes.map(Byte.byte2int(_))

    val chars = for {
      vb <- valueBytes
      glHash <- glyphMap.get((fontName, vb))
      glChar <- GlyphHashLookup.global.lookup(glHash)
    } yield {
      glChar
      // println(s"found ${hash} / $ival")
    }

    chars
  }

  var index = 0
  def renderText(charTrix: TextRenderInfo): Unit = {
    for {
      charTri <- charTrix.getCharacterRenderInfos
    } {

      val mcid = charTri.getMcid
      val rawChars = if (charTri.getText.isEmpty && charTri.hasMcid(mcid, false)) {
        lookupGlyph(charTri: TextRenderInfo)
      } else if (charTri.getText.isEmpty) {
        lookupGlyph(charTri: TextRenderInfo)
      } else {
        charTri.getText().toCharArray().toSeq
      }


      rawChars.foreach({ rawCharX =>

        val subChars = maybeSubChar(rawCharX).filterNot(_ == ' ')

        if (!subChars.isEmpty) {
          val charBounds = computeTextBounds(charTri)
          val charStr = subChars.mkString

          val charBox = charBounds.map(bnds =>
            CharAtom(
              TargetRegion(
                componentIdGen.nextId,
                pageId,
                bnds
              ),
              charStr
            )
          ).getOrElse ({
            val msg = s"ERROR bounds are invalid"
            sys.error(msg)
          })
          currCharBuffer.append(charBox)
          charWindow ++= subChars
        }

        if (index > 0)  {
          println(s"""chars: raw:${rawCharX} subs: [${subChars.mkString(", ")}] / ${subChars.map(_.toInt).mkString(", ")}""")
          // GlyphPositioning.traceGlyphPositioning(charTri, reader)
          // println(fonts.DocumentFontInfo.getCharTriInfo(charTri, reader))
          // println("\n\n")
          index -= 1
        }

      })
    }
  }

  def computeTextBounds(charTri: TextRenderInfo): Option[LTBounds] = {
    val fontProgramEmbedded = charTri.getFont.getFontProgram

    val fontProgram = fontProgramEmbedded

    val fontMetrics = fontProgram.getFontMetrics

    val ascentStart = charTri.getAscentLine().getStartPoint()
    val descentStart = charTri.getDescentLine().getStartPoint()

    val absoluteCharLeft: Double = descentStart.get(PVector.I1).toDouble
    val absoluteCharBottom: Double = descentStart.get(PVector.I2).toDouble

    val charLeft = geomTranslation.transX(absoluteCharLeft)
    val charBottom = geomTranslation.transY(absoluteCharBottom)

    var charHeight = ascentStart.get(PVector.I2).toDouble - descentStart.get(PVector.I2)
    var charWidth = charTri.getDescentLine().getLength().toDouble

    if (charWidth.toInt == 0) {
      // figure out the exact dimensions of this glyph...
      // In glyph space:

      val pdfString = charTri.getPdfString
      val decoded = charTri.getFont.decode(pdfString)
      val bs = pdfString.getValueBytes.map(Byte.byte2int(_) & 0xFF)
      val glyphCode = bs(0)

      if (glyphCode < 0) {
        // println(s"""bs = [${bs.mkString(", ")}], pdfString = ${pdfString.toString()}""")

        // import DocumentFontInfo._
        // println(getCharTriInfo(charTri, reader))

      }
      val charBBox = fontProgram.getCharBBox(glyphCode)
      val fontBbox = fontMetrics.getBbox
      val glyphWidths = fontMetrics.getGlyphWidths

      if (glyphWidths!=null && glyphWidths.length > glyphCode) {
        charWidth = glyphWidths(glyphCode).toDouble
      } else {
        if (fontBbox != null) {
          val y0 = fontBbox(1)
          val y1 = fontBbox(3)
          charWidth = (y1 - y0).toDouble
        }
      }
    }


    if (charHeight.nan || charHeight.inf) {
      println(s"warning: char height is NaN or Inf")
      charHeight = 0
    }

    if (charWidth.nan || charWidth.inf || charWidth.toInt==0) {
      println(s"warning: char width is 0, NaN, or Inf")
      charWidth = 0
    }


    val charTop = charBottom - charHeight

    Some(LTBounds(
      left=charLeft,
      top=charTop,
      width=charWidth,
      height=charHeight
    ))
  }


  def outputCharDebugInfo():Unit = {
    // if(charsToDebug contains charIndex) {
    //   println(s"""Char bounds ${charBounds.prettyPrint}""")
    // }
    // if (!charsToDebug.isEmpty) {
    //   if(charsToDebug contains charIndex) {
    //     println(s"Outputting char info #${charIndex}")
    //     println(s"  text = ${text}")
    //     DocumentFontInfo.outputCharInfo(charTri, reader)
    //     DocumentFontInfo.reportFontInfo(charTri.getFont)
    //     println(s"-------------------------------------\n\n")
    //   } else {
    //     if (charsToDebug.min - 10 < charIndex && charIndex < charsToDebug.max + 10) {
    //       println(s" renderText(${text} ${charIndex})")
    //     }
    //   }
    // }
  }



  def renderImage(iri: ImageRenderInfo): Unit = {
    // TODO figure out why this isn't working (img type not supported...)
    // val img = iri.getImage
    // val bimg = img.getBufferedImage

    // val x = bimg.getMinX.toDouble
    // val y = bimg.getMinY.toDouble
    // val w = bimg.getWidth.toDouble
    // val h = bimg.getHeight.toDouble

    // val bounds = LTBounds(
    //   x - pageRectangle.getLeft,
    //   pageRectangle.getHeight - y - pageRectangle.getBottom - h,
    //   w, h
    // )

    // val imgRegion = ImgAtom(
    //     TargetRegion(
    //       componentIdGen.nextId,
    //       PageID(0),
    //       bounds
    //     )
    //   )

    // currCharBuffer.append(imgRegion)
  }
}
