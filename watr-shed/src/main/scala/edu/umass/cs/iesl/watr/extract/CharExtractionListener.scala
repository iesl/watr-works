package edu.umass.cs.iesl.watr
package extract

import com.itextpdf.kernel.pdf.PdfPage
import geometry._

import scala.collection.mutable
import scala.collection.JavaConversions._

import _root_.com.itextpdf
import itextpdf.kernel.geom.{Vector => PVector}
import itextpdf.kernel.pdf.canvas.parser.listener.IEventListener
import itextpdf.kernel.pdf.canvas.parser.EventType
import itextpdf.kernel.pdf.canvas.parser.data._
import itextpdf.kernel.pdf.PdfReader

import fonts._
import utils.IdGenerator
import utils.EnrichNumerics._
import TypeTags._

import textboxing.{TextBoxing => TB}, TB._

object PdfPageObjectOutput {

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

case class GeometryTranslation(
  transX: (Double) => Double,
  transY: (Double) => Double
)
class CharExtractionListener(
  reader: PdfReader,
  stableId: String@@DocumentID,
  charsToDebug: Set[Int] = Set(),
  charIdGen: IdGenerator[CharID],
  currCharBuffer: mutable.ArrayBuffer[CharAtom], // = mutable.ArrayBuffer[CharAtom]()
  pdfPage: PdfPage,
  pageNum: Int@@PageNum,
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

  def fallbackRep(charTri: TextRenderInfo): (String, Option[Int]) = {
    val pdfString = charTri.getPdfString
    val valueBytes = pdfString.getValueBytes.map(Byte.byte2int(_))

    ((for (b <- valueBytes) yield s"¿$b;").mkString ->
      Option(valueBytes(0)))
  }


  val charWindow = mutable.MutableList[String]()
  var __triggerText = "pathways" // "ansferredinto100" // Start debug logging when this text is found
  var (__curr: Int, __start:Int, __enable:Boolean) = (0, 150, false)
  var __verbose:Boolean = false

  var (__len: Int, __skip:Int) = (100, 20)

  def renderText(charTris: TextRenderInfo): Unit = {
    if (__enable) { __curr += 1 }

    for (charTri <- charTris.getCharacterRenderInfos) {

      if (__verbose) {
        println(s"renderText:charTri.mcid=${charTri.getMcid}")
      }

      val (stringRep: String, code: Option[Int]) = if (!charTri.getText.isEmpty) {
        if (__verbose) {
          println(s"   for text(): ${charTri.getText}")
        }
        val t = charTri.getText()
          .map{ c =>
            if (__verbose) {
              val maybsub = UnicodeUtil.maybeSubChar(c)
              println(s"    c=${c}, asint=${c.toInt}, maybeSub: ${maybsub.map(_.toInt)}")
            }
            UnicodeUtil.maybeSubChar(c).filter(_ > ' ').mkString
          }
          .mkString
        (t, None)
      } else {
        // TODO reinstate glyph hash lookups
        fallbackRep(charTri)
      }

      if (__enable) { charWindow += stringRep }

      if (!stringRep.isEmpty && !code.exists(_ <= 32)) {

        computeTextBounds(charTri).map { charBounds =>
          val nextId = charIdGen.nextId

          val charAtom = CharAtom(
            nextId,
            PageRegion(
              RecordedPageID(
                PageID(-(1+pageNum.unwrap)), // This is just  hacky way to set an invalid PageID (which will be later changed when added to db)
                StablePageID(stableId, pageNum)
              ),
              charBounds
            ),
            stringRep,
            code
          )

          currCharBuffer.append(charAtom)

          if (__enable) {
            if (!__triggerText.isEmpty()) {
              val currCharWindow = charWindow.takeRight(20).mkString
              if (currCharWindow.endsWith(__triggerText)) {
                __start = __curr
                __verbose = true
              }

            }
            if (__start <= __curr &&  __curr < __start + __len) {
              if (__verbose) {
                println(s"@${__curr}: ${charAtom}: wonky=${charAtom.wonkyCharCode}")
                println(s"""text near: ${charWindow.takeRight(20).mkString}""")
              }
            } else {
              __verbose = false
            }

          }
        }
      }
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

    val charHeight = ascentStart.get(PVector.I2).toDouble - descentStart.get(PVector.I2)
    var charWidth = charTri.getDescentLine().getLength().toDouble

    if (charWidth.toInt == 0) {
      // figure out the exact dimensions of this glyph...
      // In glyph space:

      val pdfString = charTri.getPdfString
      // val decoded = charTri.getFont.decode(pdfString)
      val bs = pdfString.getValueBytes.map(Byte.byte2int(_) & 0xFF)
      val glyphCode = bs(0)

      if (glyphCode < 0) {
        // println(s"""bs = [${bs.mkString(", ")}], pdfString = ${pdfString.toString()}""")

        // import DocumentFontInfo._
        // println(getCharTriInfo(charTri, reader))

      }
      // val charBBox = fontProgram.getCharBBox(glyphCode)
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


    if (charHeight.nan || charHeight.inf || charHeight.toInt==0) {
      None
    } else if (charWidth.nan || charWidth.inf || charWidth.toInt==0) {
      None
    } else {
      val charTop = charBottom - charHeight

      Some(LTBounds.Doubles(
        left=charLeft,
        top=charTop,
        width=charWidth,
        height=charHeight
      ))
    }
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
    //       PageNum(0),
    //       bounds
    //     )
    //   )

    // currCharBuffer.append(imgRegion)
  }
}

      // val rawChars = if (charTri.getText.isEmpty && charTri.hasMcid(mcid, false)) {
      //   val gl = lookupGlyph(charTri: TextRenderInfo)
      //   if (gl.isEmpty) {
      //     val pdfString = charTri.getPdfString
      //     val bs = pdfString.getValueBytes.map(Byte.byte2int(_).toString.toSeq).mkString(",").toSeq
      //     // Seq('?', '{') ++ bs ++ Seq('}')
      //     '¿' +: bs
      //   } else gl
      // } else if (charTri.getText.isEmpty) {
      //   if (1455 <= index && index <= 1470) {
      //     println("here 2")
      //   }
      //   lookupGlyph(charTri: TextRenderInfo)
      // } else {
      //   if (1455 <= index && index <= 1470) {
      //     println("here 3")
      //   }
      //   val txt = charTri.getText().toCharArray().toSeq
      //   if (txt.isEmpty) {
      //     val pdfString = charTri.getPdfString
      //     val bs = pdfString.getValueBytes.map(Byte.byte2int(_).toString.toSeq).mkString(",").toSeq
      //     // Seq('¿', '{') ++ bs ++ Seq('}')
      //     '¿' +: bs
      //   } else txt
      // }

// if (index > 0)  {
//   println(s"""chars: raw:${rawCharX} subs: [${subChars.mkString(", ")}] / ${subChars.map(_.toInt).mkString(", ")}""")
//   // GlyphPositioning.traceGlyphPositioning(charTri, reader)
//   // println(fonts.DocumentFontInfo.getCharTriInfo(charTri, reader))
//   println("=======================\n\n")
//   index -= 1
// }
