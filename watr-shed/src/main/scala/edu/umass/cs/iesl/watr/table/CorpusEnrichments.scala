package edu.umass.cs.iesl.watr
package table

import corpora._
import corpora.filesys._

trait CorpusEnrichments extends
    ComponentEnrichments with
    TextReflowEnrichments {

  import ammonite.ops._

  def initCorpus(wd: Path): Corpus = {
    Corpus(wd / "corpus-test")
  }

}
