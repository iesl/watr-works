package edu.umass.cs.iesl.watr
package ext


// import com.itextpdf.text.Rectangle
// import java.io.InputStreamReader
// import java.io.FileInputStream
import org.scalatest._
import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.ExtractionUtils
// import pl.edu.icm.cermine.content.model.ContentStructure
// import pl.edu.icm.cermine.content.transformers.BxContentStructToDocContentStructConverter
// import pl.edu.icm.cermine.exception.TransformationException
// import pl.edu.icm.cermine.structure.model.BxBounds
import pl.edu.icm.cermine.structure.model.BxChunk
import pl.edu.icm.cermine.structure.model.BxZone
import pl.edu.icm.cermine.structure.model.BxLine
import pl.edu.icm.cermine.structure.model.BxWord
import pl.edu.icm.cermine.structure.model.BxDocument
import pl.edu.icm.cermine.structure.model.BxPage
import pl.edu.icm.cermine.structure.tools.BxBoundsBuilder
import scala.collection.JavaConversions._

import spatialindex.SpatialIndex


class CermineTextAlignmentTest extends FlatSpec {
  // import watrmarks._
  // import StandardLabels._

  behavior of "cermine+itextpdf - generated text"

  it should "convert to watr dom" in {
    // diff between text/raw text/nlm text?
    // val svg = watrmarks.dom.readWatrDom(new InputStreamReader(papers.`6376.svg`), bioDict)


    val conf = new ComponentConfiguration()

    val  structuredDoc = ExtractionUtils.extractStructure(conf, papers.`6376.pdf`)

    val contentStructure  = ExtractionUtils.extractText(conf, papers.`6376.pdf`)


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

    debug("zone reporting")

    def printChunk(chunk: BxChunk): Unit = {
      debugReport(
        chunk.toText,
        chunk.childrenCount,
        chunk.getFontName,
        chunk.getFontNames,
        chunk.getMostPopularFontName,
        chunk.getArea,
        chunk.getBounds,
        chunk.getFirstChild,
        chunk.getHeight,
        chunk.getId,
        chunk.getNext,
        chunk.getNextId,
        chunk.getParent,
        chunk.getPrev,
        chunk.getWidth,
        chunk.getX,
        chunk.getY,
        chunk.hasChildren,
        chunk.hasNext,
        chunk.hasPrev
      )

      if (chunk.hasNext()) {
        printChunk(chunk.getNext)
      }
    }

    def printWord(word: BxWord): Unit = {
      debugReport(
        "Bx Word",
        word.toText,
        word.getArea,
        word.getBounds,
        word.getFirstChild,
        word.getHeight,
        word.getId,
        word.getNext,
        word.getNextId,
        word.getParent,
        word.getPrev,
        word.getWidth,
        word.getX,
        word.getY,
        word.hasChildren,
        word.hasNext,
        word.hasPrev
      )
      if (word.hasChildren()) {
        printChunk(word.getFirstChild)
      }

      if (word.hasNext()) {
        printWord(word.getNext)
      }
    }

    def printLine(line: BxLine): Unit = {

      debugReport(
        "Bx Line",
        line.childrenCount,
        line.getFontNames,
        line.getMostPopularFontName
      )

      if (line.hasChildren()) {
        printWord(line.getFirstChild)
      }

      if (line.hasNext()) {
        printLine(line.getNext)
      }
    }



    def printZones(zone: BxZone): Unit = {

      zone.getLabel
      // childrenCount       () => Int
      // getChunks           () => List[BxChunk]
      // getFontNames        () => Set[String]
      // getLabel            () => BxZoneLabel
      // getMostPopularFontName  () => String
      // iterator            () => Iterator[BxLine]
      // toText              () => String



      debugReport(
        s"Zone id ${zone.getId}",
        zone.getArea,
        zone.getBounds,
        zone.getHeight,
        zone.getNext,
        zone.getNextId,
        zone.getParent,
        zone.getPrev,
        zone.getWidth,
        zone.getX,
        zone.getY,
        zone.hasChildren,
        zone.hasNext,
        zone.hasPrev
      )

      if (zone.hasChildren()) {
        printLine(zone.getFirstChild)
      }

      if (zone.hasNext()) {
        printZones(zone.getNext)
      }

    }

    val zone1 = structuredDoc.asZones().toList.head

    printZones(zone1)

    // structuredDoc.asZones().toList.foreach{ zone =>
    //   printZones(zone)
    //   printLine(zone.getFirstChild)

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



    // contentStructure.getSections().toList.foreach{ section =>
    //   debugReport(
    //     "title",
    //     section.getTitle
    //   )
    //   debug("paragraphs")
    //   section.getParagraphs.toList.foreach{ para =>
    //     debugReport(
    //       para.take(20)
    //     )
    //  }
    //   debug("subsections")
    //   section.getSubsections.toList.foreach{ subsection =>

    //     debugReport(
    //       subsection.getTitle
    //     )
    //   }
    // }


  }



    // println("segmenting pages")
    // var doc = ExtractionUtils.segmentPages(conf, document);
    // println("resolving reading order")
    // doc = ExtractionUtils.resolveReadingOrder(conf, doc);
    // println("classifyInitially")
    // doc =  ExtractionUtils.classifyInitially(conf, doc);
    // println("extracting text")
    // val contentStructure = extractText(conf, document)

    // import scala.collection.JavaConversions._

    // contentStructure.getSections().toList.foreach{ section =>
    //   // section.getTitle()

    // }

}
