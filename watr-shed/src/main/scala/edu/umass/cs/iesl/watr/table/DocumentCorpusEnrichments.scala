package edu.umass.cs.iesl.watr
package table

import textreflow.data._
import corpora._

import bioarxiv._
import BioArxivOps._

import TypeTags._

import labeling._
import geometry.zones.syntax._

trait DocumentCorpusEnrichments extends LabelWidgetUtils {


  def bioarxivLabelers(stableIds: Seq[String@@DocumentID])(implicit corpus: Corpus, docStore: DocumentCorpus): Seq[LabelingPanel] = {
    val lws = for {
      stableId <- stableIds
      entry <- corpus.entry(stableId.unwrap)
      rec   <- getBioarxivJsonArtifact(entry)
    } yield { TitleAuthorsLabelers.bioArxivLabeler(stableId, rec, docStore) }

    lws
  }

  // def nameLabelers(stableIds: Seq[String@@DocumentID])(implicit corpus: Corpus, docStore: DocumentCorpus): Seq[LabelingPanel] = {
  //   val docs = for {
  //     stableId <- stableIds
  //     entry <- corpus.entry(stableId.unwrap)
  //     rec   <- getBioarxivJsonArtifact(entry)
  //   } yield (stableId, rec)

  //   val nameLabeler  = AuthorNameLabelers.nameLabeler(docStore, docs)

  //   makePagePanel(nameLabeler)
  // }

  implicit class RicherDocumentCorpus(val theDocumentCorpus: DocumentCorpus) {

    def documents(n: Int=0, skip: Int=0): Seq[String@@DocumentID] = {
      val allEntries = theDocumentCorpus.getDocuments()
      val skipped = if (skip > 0) allEntries.drop(skip) else allEntries
      val entries = if (n > 0) skipped.take(n) else skipped
      entries
    }
  }

  implicit class RicherStableID(val thisStableId: String@@DocumentID) {
    def getDocument()(implicit docStore: DocumentCorpus): Int@@DocumentID = {
      docStore.getDocument(thisStableId).getOrElse { sys.error(s"no document ${thisStableId}") }
    }

    def getPages()(implicit docStore: DocumentCorpus): Seq[Int@@PageID] = {
      docStore.getDocument(thisStableId).toSeq
        .flatMap(docId => docStore.getPages(docId))
    }

    def printPageLines(pageNum: Int)(implicit docStore: DocumentCorpus): Unit = {
      for {
        (vlineZone, linenum) <- docStore.getPageVisualLines(thisStableId, PageNum(pageNum)).zipWithIndex
      }  {
        val maybeReflow = docStore.getTextReflowForZone(vlineZone.id)
        maybeReflow match {
          case Some(reflow) => println(reflow.toText)
          case None => println(s"no reflow for zone ${vlineZone}")
        }

      }
    }

  }

}
