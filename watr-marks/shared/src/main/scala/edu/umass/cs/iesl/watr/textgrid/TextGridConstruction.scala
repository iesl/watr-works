package edu.umass.cs.iesl.watr
package textgrid

trait TextGridConstruction {
  import geometry._

  import utils.EnrichNumerics._
  import geometry.syntax._

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


  def mkPageRegion(stablePage: StablePage, x: Int, y: Int, w: Int, h: Int): PageRegion = {
    val bbox = getRegionBounds(x, y, w, h)
    PageRegion(stablePage, bbox)
  }


  def stringToPageTextGrid(
    stableId: String@@DocumentID,
    initText: String,
    pageNum: Int@@PageNum,
    maybePageId: Option[Int@@PageID]
  ): TextGrid = {

    val stablePage = StablePage(stableId, pageNum)
    val pageLines = linesWithLeftPadding(initText)

    val rows = for {
      ((lpad, line), linenum)   <- pageLines.zipWithIndex
    } yield {
      val cells = for { (char, ichar) <- line.toCharArray.zipWithIndex } yield {
        val chnum = lpad + ichar
        val pageRegion = mkPageRegion(stablePage, x=chnum, y=linenum, w=1, h=1)
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
      TextGrid.Row.fromCells(cells)
    }

    TextGrid.fromRows(stableId, rows)
  }

  def addLabelsToGridRow(row: TextGrid.Row, labelSpans: Seq[((Int, Int), watrmarks.Label)]): TextGrid.Row = {
    val row0 = row.toCursor.get
    val labeledCursor = labelSpans.foldLeft(row0) {case (accCur, ((start, end), label)) =>
      val win = accCur.move(start)
        .get.toWindow
        .slurpRight({ case (window, next) => window.length <= end-start })

      LabeledSequence.addBioLabel(label, win.cells)
      win.closeWindow.start
    }

    TextGrid.Row.fromCells(labeledCursor.toList)
  }
}
