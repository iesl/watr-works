package edu.umass.cs.iesl.watr
package segment

import watrmarks._
import geometry._
import geometry.syntax._
import textgrid._
import TypeTags._

import com.outr.lucene4s._
import com.outr.lucene4s.field.{Field, FieldType, IndexOption}
import com.outr.lucene4s.keyword.KeywordIndexing
import com.outr.lucene4s.query._


trait MarginalMatterDetectionPageScope extends PageScopeSegmenter {


  /**
    * Number columns:
    * - no punctuation (e.g., (1) [1] 1. )
    * - strictly increasing
    * - each page starts at 1 or last page max+1
    * - alignment is centered on midrise-band
    * - tend towards extreme left/right of page
    * - font may be unique to numbering
    * - relatively constant spacing to page edge
    * - wide whitespace margins

    * Marginalia removal
    *
    **/



  // Page numbers
  // Line numbering
  // per-page journal name/pages
  // per-page author/title name (Smith et. al.)
  // dois
  // header/footer notifications (e.g., arXiv preprint note)


  // per-page repeated text
  // every other page repeated text
  // per-page incrementing text
  // number columns

  def labelRepeatedMarginalText(): Unit = {
  }

  // val lucene = new DirectLucene(uniqueFields = List("name"), defaultFullTextSearchable = true, autoCommit = true)

  val CustomFieldType = FieldType(
    indexOptions = Set(IndexOption.Documents, IndexOption.Frequencies, IndexOption.Positions),
    tokenized = false,
    stored = false,
    frozen = true
  )
  val lucene = new DirectLucene(uniqueFields = List("name"), defaultFullTextSearchable = false, autoCommit = false)

  val trigram: Field[String] = lucene.create.field[String]("trigram", CustomFieldType)

  def indexLines(): Unit = {
    val lineShapeTuples = getLabeledLines(LB.BaselineMidriseBand)
      .map(l => (l, getCharsForShape(l), getFontsForShape(l).head))

    lineShapeTuples.foreach { case (lineShape, lineChars, scaledFontId) =>
      val lineTrigrams = lineChars.sliding(3).toList.map{ case ngram =>
        val tri = ngram.map(_.char).mkString
        trigram(tri)
      }

      val doc = lucene.doc().fields(lineTrigrams:_*).index()
    }

    lucene.commit()

  }



}

trait MarginalMatterDetectionDocScope extends DocumentScopeSegmenter {

  def findRepeatedMarginalLines(): Unit = {
    pageSegmenters.foreach { pageSegmenter =>
      pageSegmenter.indexLines()

    }

  }
}
