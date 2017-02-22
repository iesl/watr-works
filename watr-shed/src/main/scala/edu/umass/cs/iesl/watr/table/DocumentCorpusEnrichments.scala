package edu.umass.cs.iesl.watr
package table


import textreflow.data._
import corpora._

import bioarxiv._
import BioArxivOps._

import TypeTags._

import labeling._
import labeling.data._

trait DocumentCorpusEnrichments extends LabelWidgetUtils {

  def printPageLines(stableId: String@@DocumentID, pageNum: Int)(implicit docStore: DocumentCorpus): Unit = {
    for {
      (vlineZone, linenum) <- docStore.getPageVisualLines(stableId, PageNum(pageNum)).zipWithIndex
    }  {
      val maybeReflow = docStore.getTextReflowForZone(vlineZone.id)
      maybeReflow match {
        case Some(reflow) => println(reflow.toText)
        case None => println(s"no reflow for zone ${vlineZone}")
      }

    }
  }

  def bioarxivLabelers(stableIds: Seq[String@@DocumentID])(implicit corpus: Corpus, docStore: DocumentCorpus): LabelWidget = {
    val lws = for {
      stableId <- stableIds
      entry <- corpus.entry(stableId.unwrap)
      rec   <- getBioarxivJsonArtifact(entry)
    } yield { TitleAuthorsLabelers.bioArxivLabeler(stableId, rec, docStore) }


    makePagePanel(LW.col(lws:_*))
  }

  def nameLabelers(stableIds: Seq[String@@DocumentID])(implicit corpus: Corpus, docStore: DocumentCorpus): LabelWidget = {
    val docs = for {
      stableId <- stableIds
      entry <- corpus.entry(stableId.unwrap)
      rec   <- getBioarxivJsonArtifact(entry)
    } yield (stableId, rec)

    val nameLabeler  = AuthorNameLabelers.nameLabeler(docStore, docs)

    makePagePanel(nameLabeler)
  }

  implicit class RicherDocumentCorpus(val theDocumentCorpus: DocumentCorpus) {

    def documents(n: Int=0, skip: Int=0): Seq[String@@DocumentID] = {
      val allEntries = theDocumentCorpus.getDocuments()
      val skipped = if (skip > 0) allEntries.drop(skip) else allEntries
      val entries = if (n > 0) skipped.take(n) else skipped
      entries
    }
  }

}
