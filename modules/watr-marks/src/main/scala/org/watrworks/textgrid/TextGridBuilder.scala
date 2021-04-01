package org.watrworks
package textgrid

import TypeTags._
import corpora._

trait TextGridBuilder extends TextGridConstruction {
  def docStore: DocumentZoningApi
  def annotApi: DocumentAnnotationApi

  def addDocument(documentId: String@@DocumentID, pages:Seq[String]): Seq[TextGrid]  = {
    docStore.addDocument(documentId)
    val pageRegions = for {
      (page, n) <- pages.zipWithIndex
    } yield {
      val textGrid = loadPageFromString(documentId, PageNum(n), page)
      (textGrid, textGrid.pageBounds().head)
    }

    // docStore.labelRegions(LB.FullPdf, pageRegions.map(_._2))
    pageRegions.map(_._1)
  }

  def loadPageFromString(
    documentId: String@@DocumentID,
    pageNum: Int@@PageNum,
    pageBlock: String
  ): TextGrid = {
    val docId = docStore.getDocument(documentId).get
    val pageId = docStore.addPage(docId, pageNum)

    stringToPageTextGrid(documentId, pageBlock, pageNum, Some(pageId))
  }

  def visualizeDocStore(): Unit = {
    for {
      documentId     <- docStore.getDocuments()
      _             = println(s"documentId: ${documentId}")
      docId        <- docStore.getDocument(documentId).toSeq
      _             = println(s"Document $documentId id:${docId}")
      pageId       <- docStore.getPages(docId)
      pageGeometry  = docStore.getPageGeometry(pageId)
      _             = println(s"  Page  ${pageId}: ${pageGeometry}")
      // pageTextGrid <- docStore.getPageText(pageId)
    } {
      // println(pageTextGrid.toText())
      ???
    }
  }

}

