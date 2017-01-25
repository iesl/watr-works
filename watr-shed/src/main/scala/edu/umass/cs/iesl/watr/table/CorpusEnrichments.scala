package edu.umass.cs.iesl.watr
package table

import spindex._
import corpora._
// import textreflow.data._

import textboxing.{TextBoxing => TB}
// import watrmarks.{StandardLabels => LB}
// import TypeTags._

trait CorpusEnrichments extends
    ComponentEnrichments with
    TextReflowEnrichments {
  import TB._

  import ammonite.ops._

  def initCorpus(wd: Path): Corpus = {
    Corpus(wd / "corpus-test")
  }

  implicit class RicherCorpus(val thisCorpus: Corpus)  {

    def chooseEntries(n: Int = 0, skip: Int = 0): Seq[CorpusEntry] = {
      val allEntries = thisCorpus.entries()
      val skipped = if (skip > 0) allEntries.drop(skip) else allEntries
      val entries = if (n > 0) skipped.take(n) else skipped
      entries
    }

    def formatLineComponent(entry: CorpusEntry, c: Component): TB.Box = {
      c.showUnknowns beside indent(8)(entry.entryDescriptor.box)
    }

    def sketchyLines(n: Int = 0, skip: Int = 0): Seq[Component] = {
      // val lls = for (entry <- chooseEntries(n, skip)) yield {
      //   entry.lines.filter(_.hasUnknownWords())
      // }
      // lls.flatten
      ???
    }

    def showSketchyLines(n: Int = 0, skip: Int = 0): Seq[Box] = {
      // val lls = for (entry <- chooseEntries(n, skip)) yield {
      //   entry.lines
      //     .filter(_.hasUnknownWords())
      //     .map(formatLineComponent(entry, _))
      // }
      // lls.flatten
      ???
    }
  }

}
