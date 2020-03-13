package org.watrworks
package textgrid

import TypeTags._
import corpora._

trait TextGridBuilder extends TextGridConstruction {
  def docStore: DocumentZoningApi
  def annotApi: DocumentAnnotationApi

  def addDocument(stableId: String@@DocumentID, pages:Seq[String]): Seq[TextGrid]  = {
    docStore.addDocument(stableId)
    val pageRegions = for {
      (page, n) <- pages.zipWithIndex
    } yield {
      val textGrid = loadPageFromString(stableId, PageNum(n), page)
      (textGrid, textGrid.pageBounds().head)
    }

    // docStore.labelRegions(LB.FullPdf, pageRegions.map(_._2))
    pageRegions.map(_._1)
  }

  def loadPageFromString(
    stableId: String@@DocumentID,
    pageNum: Int@@PageNum,
    pageBlock: String
  ): TextGrid = {
    val docId = docStore.getDocument(stableId).get
    val pageId = docStore.addPage(docId, pageNum)

    stringToPageTextGrid(stableId, pageBlock, pageNum, Some(pageId))
  }

  def visualizeDocStore(): Unit = {
    for {
      stableId     <- docStore.getDocuments()
      _             = println(s"stableId: ${stableId}")
      docId        <- docStore.getDocument(stableId).toSeq
      _             = println(s"Document $stableId id:${docId}")
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

