package edu.umass.cs.iesl.watr
package ext

import java.io.InputStream
import org.scalatest._

import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.ExtractionUtils


class ITextExtractorTest extends FlatSpec {

  behavior of "text extraction -> SVG from itext"

  def itextPdfToSvg(pdfis: InputStream): Unit = {
    val conf = new ComponentConfiguration()
    val charExtractor = new XITextCharacterExtractor()
    conf.setCharacterExtractor(charExtractor)

    val d0 = ExtractionUtils.extractCharacters(conf, pdfis)
    val d1 = ExtractionUtils.segmentPages(conf, d0)
    val d2 = ExtractionUtils.resolveReadingOrder(conf, d1);
    val d3 = ExtractionUtils.classifyInitially(conf, d2);

    // to svg...


  }


  it should "use custom char extractor" in {
    val is = papers.`6376.pdf`
    val _ = itextPdfToSvg(is)
  }

}
