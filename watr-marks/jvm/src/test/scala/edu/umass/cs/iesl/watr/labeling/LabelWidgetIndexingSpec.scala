package edu.umass.cs.iesl.watr
package labeling

import org.scalatest._

import geometry._
import textreflow.data._
import data._
import utils.EnrichNumerics._
import TypeTags._
import corpora._

class LabelWidgetIndexingSpec extends FlatSpec with Matchers with CorpusTestingUtil {
  def createEmptyDocumentCorpus(): DocumentCorpus = new MemDocstore

  import LabelWidgetIndex._

  behavior of "LabelWidgetIndexing"

  // report the total widget geometry
  // normalize the added target regions to a common geometry ?? (to prevent text size diffs between pages)
  // list all of the contained target regions
  // foreach target region, find page geometry of targeted page

  // Queries: point/box, return all contained or intersected
  //   Widget query:
  //     find overlay regions and labeled regions
  //   Target page queries:
  //

  // Clipping:
  //     clip the results of a query, s.t. all target regions are clipped to bbox

  //
  it should "" in new FreshDocstore() {}
}
