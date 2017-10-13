package edu.umass.cs.iesl.watr
package extract


// class RunTrackingListener(
//   reader: PdfReader,
//   charIdGen: IdGenerator[CharID],
//   pdfPage: PdfPage,
//   pageNum: Int@@PageNum,
//   pageGeometry: PageGeometry,
//   geomTranslation: GeometryTranslation
// ) extends  ExtractionBasics (geomTranslation) with IEventListener {

//   def fallbackRep(charTri: TextRenderInfo): (String, Option[Int]) = {
//     val pdfString = charTri.getPdfString
//     val valueBytes = pdfString.getValueBytes.map(Byte.byte2int(_))

//     ((for (b <- valueBytes) yield s"¿$b;").mkString ->
//       Option(valueBytes(0)))
//   }

//   // Limit the # of chars that can be extracted per page to prevent pathological cases (e.g., embedded charts using symbol font-based dots)
//   val MAX_EXTRACTED_CHARS_PER_PAGE = 10000
//   var totalCharCount = 0
//   var lastItem: ExtractedItem = null

//   def renderText(charTris: TextRenderInfo): Unit = {

//     for (charTri <- charTris.getCharacterRenderInfos.asScala) {
//       totalCharCount += 1

//       if (totalCharCount < MAX_EXTRACTED_CHARS_PER_PAGE) {

//         val (stringRep: String, code: Option[Int]) = if (!charTri.getText.isEmpty) {
//           // val t = charTri.getText().map{ c =>
//           //     UnicodeUtil.maybeSubChar(c).filter(_ > ' ').mkString
//           //   }.mkString
//           val t = charTri.getText()
//           (t, None)
//         } else {
//           fallbackRep(charTri)
//         }

//         if (stringRep.nonEmpty && !code.exists(_ <= 32)) {
//           computeTextBounds(charTri).foreach { charBounds =>

//             val nextId = charIdGen.nextId

//             val charAtom = CharItem(
//               nextId,
//               charBounds,
//               stringRep,
//               code
//             )

//             addCharItem(charAtom)

//           }
//         } else if (code.exists(_ < 32)) {
//           computeTextBounds(charTri).foreach { charBounds =>
//             val nextId = charIdGen.nextId
//             val charAtom = CharItem(
//               nextId,
//               charBounds,
//               "░",
//               code
//             )

//             addCharItem(charAtom)

//           }
//         }
//       }
//     }
//   }

//   def renderClipPathInfo(cpi: ClippingPathInfo): Unit = {
//     val ctm = cpi.getCtm
//     val pathTransVec = ctmToLTBoundsPdfSpace(ctm).toPoint(Dir.TopLeft)
//     val path = cpi.getClippingPath
//     println("=== Begin Waypoints ===")
//     val waypointList = for {
//       subPath <- path.getSubpaths.asScala : Seq[Subpath]
//     } yield for {
//       ishape  <- subPath.getSegments.asScala:  Seq[IShape]
//     } yield for {
//       ipoint   <- ishape.getBasePoints.asScala:  Seq[IPoint]
//     } yield {
//       val p = ipointToPoint(ipoint)
//       val trans = p.translate(pathTransVec)
//       val tInvert = invertPointSpace(trans)
//       // val endPoint = pathCurrPt.translate(p )
//       // pathCurrPt = endPoint
//       println(s"      waypoint: $trans /inv= ${tInvert}")
//       tInvert
//     }

//     val waypoints = waypointList.flatten.flatten
//     println("=== end Waypoints ===")

//   }

//   def renderPath(renderInfo: PathRenderInfo): Unit = {
//     val ctm = renderInfo.getCtm
//     val pathTransVec = ctmToLTBoundsPdfSpace(ctm).toPoint(Dir.TopLeft)

//     val path = renderInfo.getPath

//     val waypointList = for {
//       subPath <- path.getSubpaths.asScala : Seq[Subpath]
//     } yield for {
//       ishape  <- subPath.getSegments.asScala:  Seq[IShape]
//     } yield for {
//       ipoint   <- ishape.getBasePoints.asScala:  Seq[IPoint]
//     } yield {
//       val p = ipointToPoint(ipoint)
//       val trans = p.translate(pathTransVec)
//       val tInvert = invertPointSpace(trans)
//       // val endPoint = pathCurrPt.translate(p )
//       // pathCurrPt = endPoint
//       // println(s"      waypoint: $trans /inv= ${tInvert}")
//       tInvert
//     }

//     val waypoints= waypointList.flatten.flatten

//     // val waypoints = for {
//     //   subPath <- path.getSubpaths.asScala : Seq[Subpath]
//     //   ishape  <- subPath.getSegments.asScala:  Seq[IShape]
//     //   ipoint   <- ishape.getBasePoints.asScala:  Seq[IPoint]
//     // } yield {
//     //   val p = ipointToPoint(ipoint)
//     //   val trans = p.translate(pathTransVec)
//     //   val tInvert = invertPointSpace(trans)
//     //   // val endPoint = pathCurrPt.translate(p )
//     //   // pathCurrPt = endPoint
//     //   // println(s"      waypoint: $trans /inv= ${tInvert}")
//     //   tInvert
//     // }

//     if (waypoints.nonEmpty) {


//       val xsort = waypoints.sortBy(_.x)
//       val ysort = waypoints.sortBy(_.y)
//       val xmin = xsort.head.x
//       val xmax = xsort.last.x
//       val ymin = ysort.head.y
//       val ymax = ysort.last.y
//       val bounds = LTBounds(xmin, ymin, xmax-xmin, ymax-ymin)

//       addPathItem(
//         ExtractedItem.PathItem(
//           charIdGen.nextId,
//           bounds,
//           waypoints
//         )
//       )
//     }

//   }

//   def renderImage(iri: ImageRenderInfo): Unit = {
//     val ctm = iri.getImageCtm
//     val imgBounds = ctmToLTBounds(ctm)

//     addImgItem(ExtractedItem.ImgItem(
//       charIdGen.nextId,
//       imgBounds
//     ))

//   }
// }

// sealed trait CharClass
// object CharClass {
//   final case object ImageClass extends CharClass
// }

// sealed trait CharBioProp
// object CharBioProp {


//   // class OutChar extends CharBioProp {
//   //   var isImage: Boolean = false
//   //   var isPath: Boolean =  false
//   // }

//   // class BegChar extends CharBioProp {
//   //   // var lineEndId: Int@@CharID = CharID(0)
//   //   // var prevLineBeginId: Int@@CharID = CharID(0)
//   //   // var nextLineBeginId: Int@@CharID = CharID(0)
//   //   // var alignedLeftToPrev: Boolean = false
//   //   // var alignedLeftToNext: Boolean = false

//   // }

//   // class InsChar extends CharBioProp {
//   // }

//   // class LastChar extends CharBioProp {
//   //   // var lineBeginId: Int@@CharID = CharID(0)
//   //   // var prevLineBeginId: Int@@CharID = CharID(0)

//   // }

// }

// import _root_.com.itextpdf

// import itextpdf.kernel.geom.{
//   Vector => PVector,
//   Point => IPoint,
//   Matrix
// }

// import itextpdf.kernel.pdf.canvas.parser.data._
// import utils.EnrichNumerics._
// import utils.ExactFloats._
// import geometry._
// import geometry.syntax._
// import utils.{RelativeDirection => Dir}

// abstract class ExtractionBasics(
//   geomTranslation: GeometryTranslation
// ) {

//   def computeTextBounds(charTri: TextRenderInfo): Option[LTBounds] = {
//     val fontProgramEmbedded = charTri.getFont.getFontProgram

//     val fontProgram = fontProgramEmbedded

//     val fontMetrics = fontProgram.getFontMetrics

//     val ascentStart = charTri.getAscentLine().getStartPoint()
//     val descentStart = charTri.getDescentLine().getStartPoint()

//     val absoluteCharLeft: Double = descentStart.get(PVector.I1).toDouble

//     val ascentY = ascentStart.get(PVector.I2).toDouble
//     val descentY = descentStart.get(PVector.I2).toDouble

//     var absCharBottom: Double = descentY
//     var charHeight = ascentY - descentY

//     if (charHeight < 0 ) {
//       charHeight = - charHeight
//       absCharBottom = ascentY
//     }

//     val charLeft = geomTranslation.transX(absoluteCharLeft)
//     val charBottom = geomTranslation.transY(absCharBottom)

//     var charWidth = charTri.getDescentLine().getLength().toDouble

//     if (charWidth.toInt == 0) {
//       // figure out the exact dimensions of this glyph...
//       // In glyph space:

//       val pdfString = charTri.getPdfString
//       // val decoded = charTri.getFont.decode(pdfString)
//       val bs = pdfString.getValueBytes.map(Byte.byte2int(_) & 0xFF)
//       val glyphCode = bs(0)

//       val fontBbox = fontMetrics.getBbox
//       val glyphWidths = fontMetrics.getGlyphWidths

//       if (glyphWidths!=null && glyphWidths.length > glyphCode) {
//         charWidth = glyphWidths(glyphCode).toDouble
//       } else {
//         if (fontBbox != null) {
//           val y0 = fontBbox(1)
//           val y1 = fontBbox(3)
//           charWidth = (y1 - y0).toDouble
//         }
//       }
//     }




//     if (charHeight.nan || charHeight.inf || charHeight <= 0) {
//       charHeight = 0
//     } else if (charWidth.nan || charWidth.inf || charWidth <= 0) {
//       charWidth = 0
//     }

//     val charTop = charBottom - charHeight

//     Some(LTBounds.Doubles(
//       left=charLeft,
//       top=charTop,
//       width=charWidth,
//       height=charHeight
//     ))
//   }

//   def ctmToLTBounds(ctm:  Matrix): LTBounds = {
//     val x1 = ctm.get(6).toDouble
//     val y1 = ctm.get(7).toDouble
//     val x2 = x1 + ctm.get(0)
//     val y2 = y1 + ctm.get(4)
//     val w = x2 - x1
//     val h = y2 - y1

//     val left = geomTranslation.transX(x1)
//     val top = geomTranslation.transY(y2)

//     val width = math.max(w, 0.1)
//     val height = math.max(h, 0.1)

//     LTBounds.Doubles(left, top, width, height)
//   }

//   def ctmToLTBoundsPdfSpace(ctm:  Matrix): LTBounds = {
//     val x1 = ctm.get(6).toDouble
//     val y1 = ctm.get(7).toDouble
//     val x2 = x1 + ctm.get(0)
//     val y2 = y1 + ctm.get(4)
//     val w = x2 - x1
//     val h = y2 - y1

//     val left = x1 //  geomTranslation.transX(x1)
//     val top = y2 // geomTranslation.transY(y2)

//     LTBounds.Doubles(left, top, w, h)
//   }

//   def ipointToPoint(p:  IPoint): Point = {
//     // val x = geomTranslation.transX(p.x)
//     // val y = geomTranslation.transY(p.y)
//     Point.Doubles(p.x, p.y)
//   }

//   def invertPointSpace(p:  Point): Point = {
//     val x = geomTranslation.transX(p.x.asDouble())
//     val y = geomTranslation.transY(p.y.asDouble)
//     Point.Doubles(x, y)
//   }

// }
