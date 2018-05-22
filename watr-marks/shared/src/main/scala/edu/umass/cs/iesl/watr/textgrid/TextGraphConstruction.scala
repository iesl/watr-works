package edu.umass.cs.iesl.watr
package textgrid

import geometry._
import geometry.syntax._

import utils.ExactFloats._
import utils.EnrichNumerics._

trait TextGraphConstruction {

  val charIds = utils.IdGenerator[CharID]()

  def xscale = 2d
  def yscale = 2d

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

  def stringToTextGraphCells(
    stableId: String@@DocumentID,
    initText: String,
    pageNum: Int@@PageNum
  ): Seq[Seq[TextGraph.GridCell]] = {

    val stablePage = StablePage(stableId, pageNum)
    val pageLines = linesWithLeftPadding(initText)

    val rows = for {
      ((lpad, line), linenum)   <- pageLines.zipWithIndex
    } yield {
      for { (char, ichar) <- line.toList.zipWithIndex } yield {
        if (char == ' ') {
          TextGraph.InsertCell(' ')
        } else {
          val chnum = lpad + ichar
          val pageRegion = mkPageRegion(stablePage, x=chnum, y=linenum, w=1, h=1)
          val adjustedBbox = pageRegion.bbox.shave(0.1d.toFloatExact)
          val adjRegion = pageRegion.copy(bbox = adjustedBbox)
          val charAtom = CharAtom(
            charIds.nextId,
            adjRegion,
            char.toString
          )
          TextGraph.GlyphCell(char, charAtom)
        }
      }
    }
    rows

  }

}
