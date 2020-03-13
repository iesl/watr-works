package org.watrworks
package corpora

import org.watrworks.{geometry => G}
import scala.collection.mutable
import TypeTags._
import textgrid._
import geometry._


class MemDocZoningApi extends DocumentZoningApi {

  object tables  {

    object documents extends DBRelation[DocumentID, Rel.Document] {
      val stableIds = mutable.HashMap[String@@DocumentID, Int@@DocumentID]()

      def forStableId(stableId: String@@DocumentID): Option[Int@@DocumentID] = {
        stableIds.get(stableId)
      }

      def add(stableId: String@@DocumentID): Rel.Document= {
        val rec = Rel.Document(nextId(), stableId)
        insert(rec.prKey, rec)
        stableIds.put(stableId, rec.prKey)
        rec
      }
    }

    object pages extends DBRelation[PageID, Rel.Page] {
      val documentFKey = mutable.HashMap[Int@@DocumentID, Int@@PageID]()
      val docIdPageNumKey = mutable.HashMap[(Int@@DocumentID, Int@@PageNum), Int@@PageID]()

      def forDocAndPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Option[Int@@PageID] = {
        docIdPageNumKey.get((docId, pageNum))
      }

      def add(docId: Int@@DocumentID, pageNum: Int@@PageNum): Rel.Page = {
        val rec = Rel.Page(nextId(), docId, pageNum, G.LTBounds.zero)
        insert(rec.prKey, rec)
        documentFKey.put(docId, rec.prKey)
        docIdPageNumKey.put((docId, pageNum), rec.prKey)
        rec
      }

      def setGeometry(pageId: Int@@PageID, bbox:G.LTBounds): Rel.Page = {
        val curr = unique(pageId)
        val up = curr.copy(bounds=bbox)
        update(pageId, up)
        up
      }

      def getGeometry(pageId: Int@@PageID): G.LTBounds = {
        unique(pageId).bounds
      }

      val pageText = mutable.HashMap[Int@@PageID, TextGrid]()

      def setPageText(pageId: Int@@PageID, text: TextGrid): Unit = {
        pageText.update(pageId, text)
      }

      def getPageText(pageId: Int@@PageID): Option[TextGrid] = {
        pageText.get(pageId)
      }
    }

    object charatoms extends DBRelation[CharID, PageItem.CharAtom] {
      object forPage extends EdgeTableOneToMany[PageID, CharID]

      def add(pageId: Int@@PageID, charAtom: PageItem.CharAtom): Unit = {
        insert(charAtom.id, charAtom)
        forPage.addEdge(pageId, charAtom.id)
      }
    }
  }



  import tables._

  def getDocuments(n: Int=Int.MaxValue, skip: Int=0): Seq[String@@DocumentID] = {
    documents.all().map(_.stableId)
  }

  def getDocumentCount(): Int = {
    documents.all().length
  }

  def addDocument(stableId: String@@DocumentID): Int@@DocumentID = {
    val prKey = documents.add(stableId).prKey
    prKey
  }
  def getDocument(stableId: String@@DocumentID): Option[Int@@DocumentID] = {
    documents
      .forStableId(stableId)
      .map(stableId => documents.unique(stableId).prKey)
  }

  def getDocumentStableId(docId: Int@@DocumentID): String@@DocumentID = {
    documents.unique(docId).stableId
  }

  def getPageIdentifier(pageId: Int@@PageID): StablePage = {
    val p = pages.unique(pageId)
    val d = documents.unique(p.document)
    StablePage(
      d.stableId, p.pagenum
    )
  }


  def addPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Int@@PageID = {
    pages.add(docId, pageNum).prKey
  }

  def getPages(docId: Int@@DocumentID): Seq[Int@@PageID] = {
    pages.all().map(_.prKey)
  }

  def getPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Option[Int@@PageID] = {
    pages.forDocAndPage(docId, pageNum)
  }

  def getPageDef(pageId: Int@@PageID): Option[Rel.Page] = {
    pages.option(pageId)
  }

  def getPageGeometry(pageId: Int@@PageID): G.LTBounds = {
    pages.getGeometry(pageId)
  }

  def setPageGeometry(pageId: Int@@PageID, pageBounds: G.LTBounds): Unit = {
    pages.setGeometry(pageId, pageBounds)
  }

  def addCharAtom(pageId: Int@@PageID, charAtom: PageItem.CharAtom): Unit = {
    charatoms.add(pageId, charAtom)
  }

  def getCharAtoms(pageId: Int@@PageID): Seq[PageItem.CharAtom] = {
    charatoms.forPage.getEdges(pageId)
      .map(charatoms.unique(_))
  }

}
