package edu.umass.cs.iesl.watr
package textgrid


import watrmarks.{StandardLabels => LB}
import TypeTags._
import corpora._

trait TextGridBuilder {
  import geometry._

  import utils.EnrichNumerics._
  import geometry.syntax._

  def docStore: DocumentZoningApi

  val charIds = utils.IdGenerator[CharID]()

  def xscale = 10.0d
  def yscale = 10.0d

  def linesWithLeftPadding(str: String): Seq[(Int, String)] = {
    str.split("\n")
      .map({s =>
             val pre = s.takeWhile(_ == ' ').length
             val line = s.trim
             (pre, line)
           })
  }

  def getRegionBoundsDbl(x: Double, y: Double, w: Double, h: Double): LTBounds = {
    val left   = x * xscale
    val top    = y * yscale
    val width  = w * xscale
    val height = h * yscale

    LTBounds.Doubles(left, top, width, height)
  }

  // bbox areas (for non-empty bounding boxes) are a bit smaller than full 1x1 area
  def getRegionBounds(x: Int, y: Int, w: Int, h: Int): LTBounds = {
    getRegionBoundsDbl(x.toDouble, y.toDouble, w.toDouble, h.toDouble)
  }


  def mkPageRegion(pageId: Int@@PageID, x: Int, y: Int, w: Int, h: Int): PageRegion = {
    val bbox = getRegionBounds(x, y, w, h)
    val recPageId = docStore.getPageIdentifier(pageId)
    PageRegion(recPageId, bbox)
  }


  def addDocument(stableId: String@@DocumentID, pages:Seq[String]): Seq[TextGrid]  = {
    docStore.addDocument(stableId)
    val pageRegions = for {
      (page, n) <- pages.zipWithIndex
    } yield {
      val textGrid = loadPageFromString(stableId, PageNum(n), page)
      // val pageId = docStore.getPage(docId, PageNum(n)).get
      // docStore.setPageText(pageId, textGrid)
      (textGrid, textGrid.pageBounds().head)
    }

    docStore.labelRegions(LB.DocumentPages, pageRegions.map(_._2))
    pageRegions.map(_._1)
  }


  def loadPageFromString(
    stableId: String@@DocumentID,
    pageNum: Int@@PageNum,
    pageBlock: String
  ): TextGrid = {
    val docId = docStore.getDocument(stableId).get
    val pageId = docStore.addPage(docId, pageNum)

    stringToPageTextGrid(pageBlock, pageNum, Some(pageId))
  }


  def stringToPageTextGrid(
    pageBlock: String,
    pageNum: Int@@PageNum,
    maybePageId: Option[Int@@PageID]
  ): TextGrid = {

    val pageId = maybePageId.getOrElse { PageID(pageNum.unwrap) }

    val pageLines = linesWithLeftPadding(pageBlock)

    val rows = for {
      ((lpad, line), linenum)   <- pageLines.zipWithIndex
    } yield {
      val cells = for { (char, ichar) <- line.toCharArray.zipWithIndex } yield {
        val chnum = lpad + ichar
        val pageRegion = mkPageRegion(pageId, x=chnum, y=linenum, w=1, h=1)
        // TODO make this clipBorder(..)
        val adjustedBbox = pageRegion.bbox.scale(-2.percent).translate(0.1, 0.1)
        val adjRegion = pageRegion.copy(bbox = adjustedBbox)
        val charAtom = CharAtom(
          charIds.nextId,
          adjRegion,
          char.toString
        )
        TextGrid.PageItemCell(charAtom, char = char)
      }

      val row = TextGrid.Row.fromCells(cells)
      val headBounds = row.pageBounds().head
      docStore.labelRegions(LB.VisualLine, Seq(headBounds))
      row
    }

    val grid = TextGrid.fromRows(rows)
    val headBounds = grid.pageBounds().head.bbox

    docStore.setPageGeometry(pageId, headBounds)
    grid
  }

  def visualizeDocStore(): Unit = {
    for {
      stableId     <- docStore.getDocuments()
      docId        <- docStore.getDocument(stableId).toSeq
      // _            <- putStrLn(s"Document $stableId id:${docId}")
      pageId       <- docStore.getPages(docId)
      pageGeometry  = docStore.getPageGeometry(pageId)
      // _            <- putStrLn(s"  Page  ${pageId}: ${pageGeometry}")
      pageTextGrid <- docStore.getPageText(pageId)
    } {
      println(pageTextGrid.toText())

    }
  }

}
