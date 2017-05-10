package edu.umass.cs.iesl.watr
package table

import corpora._

trait CorpusEnrichments extends
    ComponentEnrichments with
    TextReflowEnrichments {

  import ammonite.ops._

  def initCorpus(wd: Path): Corpus = {
    Corpus(wd / "corpus-test")
  }

  // implicit class RicherCorpus(val thisCorpus: Corpus)  {
  //   def chooseEntries(n: Int = 0, skip: Int = 0): Seq[CorpusEntry] = {
  //     val allEntries = thisCorpus.entries()
  //     val skipped = if (skip > 0) allEntries.drop(skip) else allEntries
  //     val entries = if (n > 0) skipped.take(n) else skipped
  //     entries
  //   }
  // }

}
