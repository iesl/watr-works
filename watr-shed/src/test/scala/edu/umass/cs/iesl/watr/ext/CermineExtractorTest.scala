package edu.umass.cs.iesl.watr
package ext


import com.itextpdf.text.Rectangle
import java.io.InputStreamReader
import org.scalatest._
import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.ExtractionUtils
import pl.edu.icm.cermine.content.model.ContentStructure
import pl.edu.icm.cermine.content.transformers.BxContentStructToDocContentStructConverter
import pl.edu.icm.cermine.exception.TransformationException
import pl.edu.icm.cermine.structure.model.BxBounds
import pl.edu.icm.cermine.structure.model.BxChunk
import pl.edu.icm.cermine.structure.model.BxDocument
import pl.edu.icm.cermine.structure.model.BxPage
import pl.edu.icm.cermine.structure.tools.BxBoundsBuilder


class CermineExtractorSpec extends FlatSpec {

  import watrmarks._
  import StandardLabels._

  object papers {
    def `6376.svg` = getClass().getResourceAsStream("/papers/6376.svg")
  }

  behavior of "svg -> cermine format"

  it should  "convert to bxdocument" in {
    val svg = watrmarks.dom.readWatrDom(new InputStreamReader(papers.`6376.svg`), bioDict)

    val document = new BxDocument()
    var actPage: BxPage = new BxPage()
    val boundsBuilder: BxBoundsBuilder = new BxBoundsBuilder()
    val pageRectangle: Rectangle = null

    def processNewBxPage(pageRectangle: Rectangle) = {
      if (actPage != null) {
        actPage.setBounds(boundsBuilder.getBounds());
        boundsBuilder.clear();
      }
      actPage = new BxPage();
      document.addPage(actPage);
    }

    def renderText(charLoc: LatticeCursor) = {

      val text = charLoc.toString
      val textBounds = charLoc.focusedBounds().map { (b1) =>
        val bounds = new BxBounds(
          b1.left,
          b1.bottom - b1.height, // pageRectangle.getHeight() - charBottom - charHeight,
          b1.width,
          b1.height
        )
        bounds
      }

      val bounds = textBounds.head

      val chunk = new BxChunk(bounds, text)
      val fontName = charLoc.focusedFonts().map(_.fontFamily).headOption.map(f =>
        chunk.setFontName(f)
      )
      actPage.addChunk(chunk)
      boundsBuilder.expand(bounds)
    }



    // watrdom -> BxDocument conversion
    val pageCursor = svg.toCursor(PageLabel).get
    // for each page:
    pageCursor.unfoldLabels.foreach { pagec =>

      val pageSize = pagec.focusedBounds() //.combine()

      var pageRect = new Rectangle(
        10f, // llx
        10f, // lly
        10f, // urx
        10f // ury
        // 0   // rotation = 0|90|180|270
      )

      processNewBxPage(pageRectangle: Rectangle)

      // get font info/dictionary
      // process alternative font names

      pagec.initCursor(CharLabel).foreach { pc =>
        pc.unfoldLabels.take(10).foreach{ charLoc =>
          renderText(charLoc)
        }
      }

    }
    val conf = new ComponentConfiguration()


    println("segmenting pages")
    var doc = ExtractionUtils.segmentPages(conf, document);
    println("resolving reading order")
    doc = ExtractionUtils.resolveReadingOrder(conf, doc);
    println("classifyInitially")
    doc =  ExtractionUtils.classifyInitially(conf, doc);
    println("extracting text")
    val contentStructure = extractText(conf, document)

    import scala.collection.JavaConversions._

    contentStructure.getSections().toList.foreach{ section =>
      // section.getTitle()

    }
  }

  def extractText(conf: ComponentConfiguration , document: BxDocument): ContentStructure = {
    try {
      println("filtering content")
      val doc = ExtractionUtils.filterContent(conf, document);

      println("extracting headers")
      var tmpContentStructure = ExtractionUtils.extractHeaders(conf, doc);
      // tmpContentStructure = ExtractionUtils.clusterHeaders(conf, tmpContentStructure);
      // conf.contentCleaner.cleanupContent(tmpContentStructure);
      val converter = new BxContentStructToDocContentStructConverter();

      converter.convert(tmpContentStructure);

    } catch {
      case ex: TransformationException =>
        throw new Error("Cannot extract content from the document!", ex);
    }
  }



}
