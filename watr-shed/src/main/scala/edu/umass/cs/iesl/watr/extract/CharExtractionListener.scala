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
  geomTranslation:GeometryTranslation
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
  // import fonts.DocumentFontInfo._
  val charWindow = mutable.MutableList[Char]()



  def charTranslation(tri: TextRenderInfo): Unit = {
    // map from spline -> char

  }

  // Global lookup table
  val globalGlyphTranslation = Map[String, GlyphClass]()


  // Local lookup table (built at start of extraction)
  // fontname/glyphcode -> glyphHash
  // e.g., 'EAXSD+TimesMT/32'   -> sha:xxxxxxxx
  val localGlyphTranslation = Map[String, GlyphInstance]()


  def lookupGlyph(charTri: TextRenderInfo): Unit = {
    val pdfString = charTri.getPdfString
    val bs = pdfString.getValueBytes.map(Byte.byte2int(_))
    val glyphCode = bs(0)

    val font = charTri.getFont

    val fprogram = font.getFontProgram
    val fontNames = fprogram.getFontNames
    val fontName = fontNames.getFontName


    // map this to a canonical glyph
    val glyphInstanceId = s"""${fontName}/${glyphCode}"""
    val glyphInstance = localGlyphTranslation(glyphInstanceId)


  }

  def renderText(charTrix: TextRenderInfo): Unit = {
    for {
      charTri <- charTrix.getCharacterRenderInfos
    } {
      // Map charTri to an extracted glyph, and try to match it to our
      //   glyph hash table

      // lookupGlyph(charTri)

      val mcid = charTri.getMcid
      if (charTri.getText.isEmpty && charTri.hasMcid(mcid, false)) {
        // println(s"""MCID encoded text near: ${charWindow.takeRight(20).mkString}""")
        // DocumentFontInfo.outputPdfDecoding(charTri, reader, "    ")
        val font = charTri.getFont

        val at = charTri.getActualText
        val spi = pdfPage.getStructParentIndex
        val structRoot = pdfPage.getDocument.getStructTreeRoot
        val pageMC = structRoot.getPageMarkedContentReferences(pdfPage)
        // pageMC
        //   .filter(_.getMcid==charTri.getMcid)
        //   .headOption.map({ mc =>
        //     println(
        //       PdfPageObjectOutput.renderElemLoc(mc)
        //     )
        //   })
        // println(s"actual text = ${at}")
        // DocumentFontInfo.outputCharInfo(charTri, reader, true)


      } else if (charTri.getText.isEmpty) {
        // println(s"""Empty text near: ${charWindow.takeRight(20).mkString}""")
        // DocumentFontInfo.outputPdfDecoding(charTri, reader, "    ")

        val pdfString = charTri.getPdfString
        // val bs = pdfString.getValueBytes.m
        val bs = pdfString.getValueBytes.map(Byte.byte2int(_))
        val b0 = bs(0)


        val font = charTri.getFont

        val fprogram = font.getFontProgram
        val pglyph = fprogram.getGlyph(b0)
        val pglyphByCode = fprogram.getGlyphByCode(b0)

        // println(s"got glyph ${b0}? ${pglyph}, by-code:${pglyphByCode}")
        // println(outputGlyphInfo(pglyphByCode, reader))
        // println(
        //   getCharTriInfo(charTri, reader, true)
        // )
      } else {
        // DocumentFontInfo.outputPdfDecoding(charTri, reader, "normal")

        val rawChar = charTri.getText().charAt(0)
        charWindow += rawChar

        val subChars = maybeSubChar(rawChar)
        val bakedChar = transformRawChar(rawChar)
        val charBounds = computeTextBounds(charTri)

        val wonkyChar = if (rawChar.toString != bakedChar.toString) Some(rawChar.toInt) else None
        // skip spaces
        if (!wonkyChar.exists(_ == 32)) {

          val charBox = charBounds.map(bnds =>
            CharAtom(
              TargetRegion(
                componentIdGen.nextId,
                pageId,
                bnds
              ),
              bakedChar,
              subChars.map(_.mkString).getOrElse(""),
              wonkyCharCode = wonkyChar
            )
          ).getOrElse ({
            val msg = s"ERROR bounds are invalid"
            sys.error(msg)
          })


          // if (wonkyChar.isDefined || subChars.isDefined) {
          //   println(s"char: ${charBox}")
          // }
          // DocumentFontInfo.outputCharInfo(charTri, reader)

          // DocumentFontInfo.reportFontInfo(charTri.getFont)
          // DocumentFontInfo.addFontInfo(charTri.getFont
          // val fullFontName = charTri.getFont().getFullFontName()(0)(3)
          // chunk.setFontName(fullFontName)

          currCharBuffer.append(charBox)
        }
      }
    }
  }


  def computeTextBounds(charTri: TextRenderInfo): Option[LTBounds] = {
    val ascentStart = charTri.getAscentLine().getStartPoint()
    val descentStart = charTri.getDescentLine().getStartPoint()

    val absoluteCharLeft: Double = descentStart.get(PVector.I1).toDouble
    val absoluteCharBottom: Double = descentStart.get(PVector.I2).toDouble

    // val pageRectangle = pdfPage.getCropBox()
    // pageGeometry.bounds

    val charLeft = geomTranslation.transX(absoluteCharLeft)
    val charBottom = geomTranslation.transY(absoluteCharBottom)


    // val charLeft = absoluteCharLeft - pageRectangle.getLeft()
    // val charBottom = absoluteCharBottom - pageRectangle.getBottom() // in math coords


    var charHeight = ascentStart.get(PVector.I2).toDouble - descentStart.get(PVector.I2)
    var charWidth = charTri.getDescentLine().getLength().toDouble

    if (charHeight.nan || charHeight.inf) {
      println(s"warning: char height is NaN or Inf")
      charHeight = 0
    }

    if (charWidth.nan || (charWidth.inf)) {
      println(s"warning: char width is NaN or Inf")
      charWidth = 0
    }

    // if (absoluteCharLeft < pageRectangle.getLeft()
    //   || absoluteCharLeft + charWidth > pageRectangle.getRight()
    //   || absoluteCharBottom < pageRectangle.getBottom()
    //   || absoluteCharBottom + charHeight > pageRectangle.getTop()) {
    // if (bounds.getX().nan || bounds.getX().inf
    //   || bounds.getY().nan || bounds.getY().inf
    //   || bounds.getHeight().nan || bounds.getHeight().inf
    //   || bounds.getWidth().nan || bounds.getWidth().inf) {
    //   // skip
    //   println(s"skipping text w/bbox= nan|inf: ${text}")
    // } else {

    val charTop = charBottom - charHeight

    Some(LTBounds(
      left=charLeft,
      top=charTop,
      width=charWidth,
      height=charHeight
    ))
  }

  def transformRawChar(ch: Char): String = {
    if (ch <= ' ') ""
    else ch.toString
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
