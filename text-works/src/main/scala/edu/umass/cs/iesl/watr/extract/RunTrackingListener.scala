package edu.umass.cs.iesl.watr
package extract

// import com.itextpdf.kernel.pdf.PdfPage
// import geometry._

// import scala.collection.JavaConverters._

// import _root_.com.itextpdf
// import itextpdf.kernel.pdf.canvas.parser.listener.IEventListener
// import itextpdf.kernel.pdf.canvas.parser.EventType
// import itextpdf.kernel.pdf.canvas.parser.data._
// import itextpdf.kernel.pdf.PdfReader

// import fonts._
// import utils.IdGenerator
// import utils.ExactFloats._
// import TypeTags._



// class RunTrackingListener(
//   reader: PdfReader,
//   charIdGen: IdGenerator[CharID],
//   pdfPage: PdfPage,
//   pageNum: Int@@PageNum,
//   pageGeometry: PageGeometry,
//   geomTranslation: GeometryTranslation
// ) extends  ExtractionBasics (geomTranslation) with IEventListener {

//   import ExtractedItem._


//   override def getSupportedEvents(): java.util.Set[EventType] ={
//     Set(
//       EventType.RENDER_TEXT,
//       EventType.RENDER_PATH,
//       // EventType.CLIP_PATH_CHANGED,
//       EventType.RENDER_IMAGE
//     ).asJava
//   }


//   override def eventOccurred(data: IEventData,  eventType: EventType): Unit = {
//     if (eventType.equals(EventType.RENDER_TEXT)) {
//       val tri = data.asInstanceOf[TextRenderInfo]
//       renderText(tri)

//     } else if (eventType.equals(EventType.RENDER_PATH)) {
//       val renderInfo = data.asInstanceOf[PathRenderInfo]
//       renderPath(renderInfo)

//     } else if (eventType.equals(EventType.RENDER_IMAGE)) {
//       val tri = data.asInstanceOf[ImageRenderInfo]
//       renderImage(tri)
//     } else if (eventType.equals(EventType.CLIP_PATH_CHANGED)) {
//       val cpi = data.asInstanceOf[ClippingPathInfo]
//       renderClipPathInfo(cpi)
//     }
//   }

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

//   var runLists = List[List[ExtractedItem]]()

//   def getPageItems(): Seq[ExtractedItem] = {
//     var currCharRunId = 0
//     def nextRunId() = {
//       currCharRunId = currCharRunId + 1
//       currCharRunId
//     }

//     val lineRows = runLists.reverse.map{ rline =>
//       val runId = nextRunId()

//       rline.foreach { item =>
//         item.charProps.charRunId = runId
//       }

//       val line = rline.reverse

//       line.head.charProps.isRunBegin = true
//       line
//     }

//     debugPrintPageItems(lineRows)

//     lineRows.flatten

//   }

//   private def debugPrintPageItems(rows: List[List[ExtractedItem]]): Unit = {
//     val rowStrs = rows.map { row =>
//       val runId = row.headOption.map(_.charProps.charRunId).getOrElse(-1)
//       val chars = row.map(_.strRepr()).mkString
//       s"${runId} >> $chars"
//     }

//     val pageStr = rowStrs.mkString("\n  ", "\n  ", "\n")

//     println(pageStr)
//   }

//   def addPathItem(item: ExtractedItem.PathItem): Unit = {
//     runLists = List(item) :: runLists
//     lastItem = item
//   }

//   def addImgItem(imgItem: ExtractedItem.ImgItem): Unit = {
//     runLists = List(imgItem) :: runLists
//     // pageImages = imgItem :: pageImages
//     lastItem = imgItem
//   }


//   def addCharItem(charAtom: ExtractedItem.CharItem): Unit = {
//     println(s"adding ${charAtom}")
//     if (lastItem==null || lastItem.bbox.bottom != charAtom.bbox.bottom) {
//       runLists = List(charAtom) :: runLists
//     } else {
//       runLists = (charAtom :: runLists.head) :: runLists.tail
//     }
//     lastItem = charAtom
//   }

//   def renderText(charTris: TextRenderInfo): Unit = {

//     if (pageNum.unwrap == 0) {
//       if (totalCharCount > 314 && totalCharCount < 318) {
//         println(s"${totalCharCount} (prequel): =================================================================")


//         val pdfString = charTris.getPdfString
//         val valueBytes = pdfString.getValueBytes
//         if (valueBytes.length > 2) {
//           val isUnicodeEncoded = valueBytes(0).toInt == 254 && valueBytes(1).toInt == 255
//           if (isUnicodeEncoded) {
//             println(s"Found Unicode Encoding: ")
//             // valueBytes.map(_.toInt)
//             val s1 = valueBytes.mkString(", ")
//             println(s"  valueBytes: ${s1}   ")
//           }
//         }

//         val info = DocumentFontInfo.getCharTriInfo(charTris, reader)
//         println(s"${totalCharCount}: =================================================================")
//         println(info)
//       }
//     }


//     for (charTri <- charTris.getCharacterRenderInfos.asScala) {
//       totalCharCount += 1


//       if (pageNum.unwrap == 0) {
//         if (totalCharCount > 314 && totalCharCount < 318) {
//           {

//             val pdfString = charTri.getPdfString
//             val valueBytes = pdfString.getValueBytes
//             if (valueBytes.length > 2) {
//               val isUnicodeEncoded = valueBytes(0).toInt == 254 && valueBytes(1).toInt == 255
//               if (isUnicodeEncoded) {
//                 println(s"Found Unicode Encoding: ")
//                 // valueBytes.map(_.toInt)
//                 val s1 = valueBytes.mkString(", ")
//                 println(s"  valueBytes: ${s1}   ")
//               }
//             }
//           }
//           val info = DocumentFontInfo.getCharTriInfo(charTri, reader)
//           println(s"${totalCharCount}: =================================================================")
//           println(info)
//         }
//       }



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

//         // if (totalCharCount > 244 && totalCharCount < 250) {
//         //   println(s"(stringRep/code) = ${stringRep} / ${code} (${stringRep.map(_.toInt).mkString(','.toString())})")
//         // }

//         if (stringRep.nonEmpty && !code.exists(_ <= 32)) {
//           // if (totalCharCount > 244 && totalCharCount < 250) {
//           //   println(s"1: ")
//           // }

//           computeTextBounds(charTri).foreach { charBounds =>
//             // if (totalCharCount > 244 && totalCharCount < 250) {
//             //   println(s"2: ")
//             // }

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
//           // if (totalCharCount > 244 && totalCharCount < 250) {
//           //   println(s"3: ")
//           // }
//           computeTextBounds(charTri).foreach { charBounds =>
//             val nextId = charIdGen.nextId
//             // if (totalCharCount > 244 && totalCharCount < 250) {
//             //   println(s"4: ")
//             // }

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

//   import geometry.syntax._
//   import utils.{RelativeDirection => Dir}
//   import itextpdf.kernel.geom.{
//     Subpath, IShape,
//     Point => IPoint
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
