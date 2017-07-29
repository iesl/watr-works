package edu.umass.cs.iesl.watr
package table

import corpora.filesys._

trait CorpusEnrichments extends TextReflowEnrichments {

  import ammonite.ops._

  def initCorpus(wd: Path): Corpus = {
    Corpus(wd / "corpus-test")
  }

}
