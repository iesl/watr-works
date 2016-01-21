package edu.umass.cs.iesl.watr
package ext

import org.scalatest._

import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.ExtractionUtils
import pl.edu.icm.cermine.content.model.ContentStructure
import better.files._


class ITextExtractorTest extends FlatSpec {

  behavior of "text extraction -> SVG from itext"

  def cerminePDF(pdf: File): Unit = {
    val conf = new ComponentConfiguration()
    conf.setCharacterExtractor(new XITextCharacterExtractor())

    val _ = pdf.inputStream.map { is =>
      val _ = ExtractionUtils.extractCharacters(conf, is)
      val structuredDoc = ExtractionUtils.extractStructure(conf, is)
      val contentStructure = ExtractionUtils.extractText(conf, structuredDoc)
    }
  }


  it should "use custom char extractor" in {
    val is = papers.`6376.pdf`
    val conf = new ComponentConfiguration()
    conf.setCharacterExtractor(new XITextCharacterExtractor())
    val _ = ExtractionUtils.extractCharacters(conf, is)
  }

}
