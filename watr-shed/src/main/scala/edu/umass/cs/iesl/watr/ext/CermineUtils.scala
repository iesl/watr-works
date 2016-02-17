package edu.umass.cs.iesl.watr
package ext

// import com.itextpdf.text.Rectangle
// import java.io.InputStreamReader
// import java.io.FileInputStream
import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.ExtractionUtils
import pl.edu.icm.cermine.content.model._
// import pl.edu.icm.cermine.content.transformers.BxContentStructToDocContentStructConverter
// import pl.edu.icm.cermine.exception.TransformationException
// import pl.edu.icm.cermine.structure.model.BxBounds
// import pl.edu.icm.cermine.structure.model.BxChunk
// import pl.edu.icm.cermine.structure.model.BxZoneLabel
// import pl.edu.icm.cermine.structure.model.BxZoneLabelCategory
// import pl.edu.icm.cermine.structure.model.BxZone
// import pl.edu.icm.cermine.structure.model.BxLine
// import pl.edu.icm.cermine.structure.model.BxWord
// import pl.edu.icm.cermine.structure.model.BxDocument
// import pl.edu.icm.cermine.structure.model.BxPage
import pl.edu.icm.cermine.structure.model._
import pl.edu.icm.cermine.structure.tools.BxBoundsBuilder
import scala.collection.JavaConversions._

import better.files._

import watrmarks._
import watrmarks.dom._

object cermineUtils {

  // diff between text/raw text/nlm text?
  // val svg = watrmarks.dom.readWatrDom(new InputStreamReader(papers.`6376.svg`), bioDict)


  // debug("chunk reporting")

  // structuredDoc.asChunks().toList.foreach{ chunk =>
  //   debugReport(
  //     chunk.getFontNames,
  //     chunk.getX,
  //     chunk.getY,
  //     chunk.getArea,
  //     chunk.getWidth,
  //     chunk.getHeight,
  //     chunk.getBounds
  //   )
  //   // chunk.getText
  // }

  // debug("zone reporting")

  def printChunk(chunk: BxChunk): Unit = {
    // debugReport(
    //   // "chunk",
    //   // chunk.toText
    //   // chunk.childrenCount,
    //   // chunk.getFontName,
    //   // chunk.getFontNames,
    //   // chunk.getMostPopularFontName,
    //   // chunk.getArea,
    //   // chunk.getBounds,
    //   // chunk.getFirstChild,
    //   // chunk.getHeight,
    //   // chunk.getId,
    //   // chunk.getNext,
    //   // chunk.getNextId,
    //   // chunk.getParent,
    //   // chunk.getPrev,
    //   // chunk.getWidth,
    //   // chunk.getX,
    //   // chunk.getY,
    //   // chunk.hasChildren,
    //   // chunk.hasNext,
    //   // chunk.hasPrev
    // )

    // if (chunk.hasNext()) {
    //   printChunk(chunk.getNext)
    // }
  }

  def printWord(word: BxWord): Unit = {
    // debugReport(
    //   // "word"
    //   // word.toText,
    //   // word.childrenCount
    //   // word.getArea,
    //   // word.getBounds,
    //   // word.getFirstChild,
    //   // word.getHeight,
    //   // word.getId,
    //   // word.getNext,
    //   // word.getNextId,
    //   // word.getParent,
    //   // word.getPrev,
    //   // word.getWidth,
    //   // word.getX,
    //   // word.getY,
    //   // word.hasChildren,
    //   // word.hasNext,
    //   // word.hasPrev
    // )
    word.iterator().toList.foreach{ chunk =>
      printChunk(chunk)

    }
    // if (word.hasChildren()) {
    //   printChunk(word.getFirstChild)
    // }

    // if (word.hasNext()) {
    //   printWord(word.getNext)
    // }
  }

  def printLine(line: BxLine): Unit = {

    debugReport(
      line.toText()
      // line.childrenCount,
      // line.getFontNames
      // line.getMostPopularFontName
    )
    line.iterator().toList.foreach{ word =>
      printWord(word)

    }

    // if (line.hasChildren()) {
    //   printWord(line.getFirstChild)
    // }

    // if (line.hasNext()) {
    //   printLine(line.getNext)
    // }
  }

  def printLabel(l: BxZoneLabel): String = {
    val cat = l.getCategory
    val gen = l.getGeneralLabel
    s"""label=$l, ${l.name()}  category = $cat, ${cat.name}, general = $gen, ${gen.name}"""

  }

  def printZones(zone: BxZone): Unit = {

    debugReport(
      s"Zone id ${zone.getId}",
      // zone.toText,
      // zone.getChunks,
      // zone.getFontNames,
      printLabel(zone.getLabel)
      // zone.iterator,
      // zone.getArea,
      // zone.getBounds,
      // zone.getHeight,
      // zone.getNext,
      // zone.getNextId,
      // zone.getParent,
      // zone.getPrev,
      // zone.getWidth,
      // zone.getX,
      // zone.getY,
      // zone.hasChildren,
      // zone.hasNext,
      // zone.hasPrev
    )

    zone.iterator().toList.foreach{ line =>
      printLine(line)

    }

    // if (zone.hasChildren()) {
    // }

    // if (zone.hasNext()) {
    //   printZones(zone.getNext)
    // }

  }

  def printSubSections(section: DocumentSection): Unit = {

    debugReport(
      "subsection",
      section.getTitle,
      section.getLevel
    )
    section.getParagraphs.toList.foreach{ p =>
      println(p.take(50))
    }

    section.getSubsections.foreach{ ss =>
      printSubSections(ss)
    }
  }

  def printSections(structure: ContentStructure): Unit = {
    structure.getSections.foreach{case (section)  =>
      printSubSections(section)
    }
  }

  def printDocument(f: File): Unit = {
    val conf = new ComponentConfiguration()


    f.inputStream.foreach { is =>

      val structuredDoc = ExtractionUtils.extractStructure(conf, is)

      // val contentStructure = ExtractionUtils.extractText(conf, structuredDoc)

      val filtered = ExtractionUtils.filterContent(conf, structuredDoc)

      val docWithMetadata = ExtractionUtils.classifyMetadata(conf, filtered);

      // val headers = ExtractionUtils.extractHeaders(conf, structuredDoc);

      println("content structure")

      // printSections(contentStructure)

      println("\n"*10)

      docWithMetadata.asPages().toList.zipWithIndex.foreach{ case (page, i) =>
        println("page "+i)

        page.iterator.toList.zipWithIndex.foreach{case (zone, ii) =>
          println("zone "+ii)
          printZones(zone)
        }

      }
    }

  }

  //   debugReport(
  //     zone.getArea,
  //     zone.getBounds,
  //     zone.getFirstChild,
  //     zone.getHeight,
  //     zone.getId,
  //     zone.getNext,
  //     zone.getNextId,
  //     zone.getParent,
  //     zone.getPrev,
  //     zone.getWidth,
  //     zone.getX,
  //     zone.getY,
  //     zone.hasChildren,
  //     zone.hasNext,
  //     zone.hasPrev
  //   )
  // }


  def pdfToWatrDomViaITextPdf(pdfFile: File, bioDict: BioLabelDictionary): WatrDom = {
    import scalaz.{Show, TreeLoc, Tree}
    import scala.collection.mutable

    val conf = new ComponentConfiguration()


    val root: WatrElement = Document(bioDict)
    var accum: TreeLoc[WatrElement] = Tree.leaf(root).loc
    val pageBounds = mutable.ListBuffer[BxBounds]()

    def totalPageHeight: Double = pageBounds.map{
      b => b.getHeight
    }.foldLeft(0.0)(_ + _)

    def totalPageWidth: Double = pageBounds.map{
      _.getWidth
    }.foldLeft(0.0)(math.max(_, _))

    pdfFile.inputStream.foreach { is =>

      val structuredDoc = ExtractionUtils.extractStructure(conf, is)

      val filtered = ExtractionUtils.filterContent(conf, structuredDoc)

      val docWithMetadata = ExtractionUtils.classifyMetadata(conf, filtered);

      val bounds = docWithMetadata.getBounds
      // accum = accum.insertDownLast(
      //   Tree.leaf(
      //     Svg(
      //       width   = bounds.getWidth,
      //       height  = bounds.getHeight,
      //       viewBox = ViewBox(
      //         bounds.getX,
      //         bounds.getY,
      //         bounds.getWidth,
      //         bounds.getHeight
      //       )
      //         // , getTransforms(elem)
      //     )
      //   ))
      accum = accum.insertDownLast(
        Tree.leaf(
          Svg(
            width   = 0,
            height  = 0,
            viewBox = ViewBox(
              0, 0, 0, 0
            )
              // , getTransforms(elem)
          )
        ))




      docWithMetadata.asPages().toList.zipWithIndex.foreach{ case (page, i) =>
        // println("page "+i)

        val m = Matrix(1, 0, 0, 1, 0, totalPageHeight)

        accum = accum.insertDownLast(
          Tree.leaf(
            Grp(List(m))
          ))

        pageBounds.append(page.getBounds)

          // set active labels

          page.iterator.toList.zipWithIndex.foreach{case (zone, ii) =>

            // println("zone "+ii)
            // zoneLabels = translateLabels(..)

            accum = accum.insertDownLast(
              Tree.leaf(
                Grp(List(Matrix(1, 0, 0, 1, 0, 0)))
            ))

          // printLabel(zone.getLabel)
          // zone.toText, // zone.getChunks, // zone.getFontNames,
          // zone.getArea, // zone.getBounds, // zone.getHeight, // zone.getNext, // zone.getNextId, // zone.getParent, // zone.getPrev,
          // zone.getWidth, // zone.getX, // zone.getY, // zone.hasChildren, // zone.hasNext, // zone.hasPrev

          zone.iterator().toList.foreach{ line =>
            // debug("per line", accum.getLabel)
            // line.toText()
            // line.childrenCount, // line.getFontNames // line.getMostPopularFontName

            // add "line" to active labels
            accum = accum.insertDownLast(
              Tree.leaf(
                Text(transforms = List())
              )
            )

            val xyOffsets = TextXYOffsets(
              xs = List(),
              endX = 0,
              ys = List()
            )

            accum = accum.insertDownLast(
              Tree.leaf(
                TSpan(
                  text          = "",
                  transforms    = List(),
                  textXYOffsets = Some(xyOffsets),
                  fontSize      = "",
                  fontFamily    = "",
                  bioBrick      = null,
                  document      = root.asInstanceOf[Document]
                )
              )
            )

            // println("adding line "+line.toText())

            line.iterator().toList.foreach{ word =>

              import org.apache.commons.lang3.StringEscapeUtils.escapeXml11
              // debug("per word", accum.getLabel)
              // add "word" to active labels
              word.iterator().toList.foreach{ chunk =>
                // either append to current tspan or start a new one:

                // val (lefts, focus, rights) = accum.get
                if (chunk.childrenCount() != 1) {
                  sys.error("chunk size != 1")
                }
                // debug("per chunk", accum.getLabel)
                val currTspan = accum.getLabel.asInstanceOf[TSpan]

                if (currTspan.fontFamily != chunk.getFontName) {
                  accum = accum.insertRight(
                    Tree.leaf(
                      TSpan(
                        text          = escapeXml11(chunk.toText()),
                        transforms    = List(),
                        textXYOffsets = Some(
                          TextXYOffsets(
                            xs = List(chunk.getX),
                            endX = 0,
                            ys = List(chunk.getY)
                          )),
                        fontSize      = chunk.getHeight.toString(),
                        fontFamily    = chunk.getFontName,
                        bioBrick      = null,
                        document      = root.asInstanceOf[Document]
                      )
                    )
                  )

                } else {
                  accum = accum.modifyLabel{ l =>
                    val tspan = l.asInstanceOf[TSpan]
                    tspan.copy(
                      text = escapeXml11(tspan.text + chunk.toText()),
                      textXYOffsets = tspan.textXYOffsets
                        .map(xyo => xyo.copy(
                               xs = xyo.xs :+ chunk.getX,
                               ys = xyo.ys :+ chunk.getY
                             )
                      )
                    )

                  }
                }

              }
              // accum = accum.parent.get

            }
            accum = accum.parent.get
            accum = accum.parent.get

          }
          accum = accum.parent.get

        }
        accum = accum.parent.get

      }

      accum = accum.parent.get

    }


    accum = accum.firstChild.map{ rootSvg =>
      rootSvg.modifyLabel { svgelem =>
        val svg = svgelem.asInstanceOf[Svg]

        svg.copy(
          height = totalPageHeight,
          width = totalPageWidth,
          viewBox = ViewBox(
            0, 0,
            width = totalPageWidth,
            height = totalPageHeight
          )
        )
      }
    }.get

    WatrDom(accum.toTree)
  }


  def main(args: Array[String]) = {

    val filename = args(0)
    val f = filename.toFile

    // printDocument(f)

    import StandardLabels._

    val wdom = pdfToWatrDomViaITextPdf(f, bioDict)

    println(
      wdom.toSvg()
    )


  }

}

// 36 	GEN_METADATA        (BxZoneLabelCategory.CAT_GENERAL),
// 39 	GEN_BODY            (BxZoneLabelCategory.CAT_GENERAL),
// 42 	GEN_REFERENCES      (BxZoneLabelCategory.CAT_GENERAL),
// 45 	GEN_OTHER           (BxZoneLabelCategory.CAT_GENERAL),
// 51     MET_ABSTRACT        (BxZoneLabelCategory.CAT_METADATA),
// 54     MET_AFFILIATION     (BxZoneLabelCategory.CAT_METADATA),
// 57     MET_ACCESS_DATA     (BxZoneLabelCategory.CAT_METADATA),
// 60     MET_BIOGRAPHY       (BxZoneLabelCategory.CAT_METADATA),
// 63     MET_AUTHOR          (BxZoneLabelCategory.CAT_METADATA),
// 66     MET_BIB_INFO        (BxZoneLabelCategory.CAT_METADATA),
// 69     MET_CORRESPONDENCE  (BxZoneLabelCategory.CAT_METADATA),
// 72     MET_DATES           (BxZoneLabelCategory.CAT_METADATA),
// 75     MET_EDITOR          (BxZoneLabelCategory.CAT_METADATA),
// 78     MET_KEYWORDS        (BxZoneLabelCategory.CAT_METADATA),
// 81     MET_TITLE           (BxZoneLabelCategory.CAT_METADATA),
// 84     MET_TYPE            (BxZoneLabelCategory.CAT_METADATA),
// 87     MET_COPYRIGHT       (BxZoneLabelCategory.CAT_METADATA),
// 93     BODY_CONTENT        (BxZoneLabelCategory.CAT_BODY),
// 96     BODY_GLOSSARY       (BxZoneLabelCategory.CAT_BODY),
// 99     BODY_EQUATION       (BxZoneLabelCategory.CAT_BODY),
// 102     BODY_EQUATION_LABEL (BxZoneLabelCategory.CAT_BODY),
// 105     BODY_FIGURE         (BxZoneLabelCategory.CAT_BODY),
// 108     BODY_FIGURE_CAPTION (BxZoneLabelCategory.CAT_BODY),
// 111     BODY_HEADING        (BxZoneLabelCategory.CAT_BODY),
// 114     BODY_JUNK           (BxZoneLabelCategory.CAT_BODY),
// 117     BODY_TABLE          (BxZoneLabelCategory.CAT_BODY),
// 120     BODY_TABLE_CAPTION  (BxZoneLabelCategory.CAT_BODY),
// 123     BODY_ACKNOWLEDGMENT (BxZoneLabelCategory.CAT_BODY),
// 126     BODY_CONTRIBUTION   (BxZoneLabelCategory.CAT_BODY),
// 129     BODY_CONFLICT_STMT  (BxZoneLabelCategory.CAT_BODY),
// 132     BODY_ATTACHMENT 	(BxZoneLabelCategory.CAT_BODY),
// 138     OTH_PAGE_NUMBER     (BxZoneLabelCategory.CAT_OTHER),
// 141     OTH_UNKNOWN         (BxZoneLabelCategory.CAT_OTHER),
// 147     REFERENCES          (BxZoneLabelCategory.CAT_REFERENCES),
// 150     MET_TITLE_AUTHOR    (BxZoneLabelCategory.CAT_METADATA),
// 153     MET_CATEGORY        (BxZoneLabelCategory.CAT_METADATA),
// 156     MET_TERMS           (BxZoneLabelCategory.CAT_METADATA);

                // accum = accum.insertDownLast(
                //   Tree.leaf(
                //     TSpan(
                //       text          = chunk.toText(),
                //       transforms    = List(),
                //       textXYOffsets = Some(xyOffsets),
                //       fontSize      = "todo",
                //       fontFamily    = chunk.getFontName,
                //       bioBrick      = null,
                //       document      = root.asInstanceOf[Document]
                //     )
                //   ))


                // debugReport(
                //   "chunk",
                //   chunk.toText
                //   chunk.childrenCount,
                //   chunk.getFontName,
                //   chunk.getFontNames,
                //   chunk.getMostPopularFontName,
                //   chunk.getArea,
                //   chunk.getBounds,
                //   chunk.getFirstChild,
                //   chunk.getHeight,
                //   chunk.getId,
                //   chunk.getNext,
                //   chunk.getNextId,
                //   chunk.getParent,
                //   chunk.getPrev,
                //   chunk.getWidth,
                //   chunk.getX,
                //   chunk.getY,
                //   chunk.hasChildren,
                //   chunk.hasNext,
                //   chunk.hasPrev
                // )
