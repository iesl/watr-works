package edu.umass.cs.iesl.watr
package textgrid

import scala.collection.mutable
// import rtrees._
import geometry._
import geometry.syntax._
import geometry.PageComponentImplicits._
import textboxing.{TextBoxing => TB}, TB._
import TypeTags._

import utils.{Cursor, Cursors, Window}
import utils.SlicingAndDicing._
import utils.DoOrDieHandlers._

// import scala.scalajs.js.annotation._

import _root_.io.circe
import circe._
import circe.literal._

trait TextGraph { self =>

  import TextGraph._

  def toText(): String = {
    getRows().map{ row =>
      row.map(_.char).mkString
    }.mkString("\n")
  }


  def splitOneLeafLabelPerLine(): TextGraph = {
    ???
  }

  def pageBounds(): Seq[PageRegion] = {
    ???
  }

  def split(row: Int, col: Int): Boolean = {
    ???
  }

  def slurp(row: Int): Boolean = {
    ???
  }

  def appendRow(row: Seq[GridCell]): Unit
  def getRows(): Seq[Seq[GridCell]]


}

object TextGraph {


  def fromJsonStr(jsStr: String): TextGraph = {
    ???
  }

  def fromJson(js: Json): TextGraph = {
    ???
  }

  sealed trait GridCell {
    def char: Char
  }

  def mbrRegionFunc(h: PageItem, tail: Seq[PageItem]): PageRegion = {
    (h +: tail).map(_.pageRegion).reduce { _ union _ }
  }

  case class GlyphCell(
    char: Char,
    headItem: PageItem,
    tailItems: Seq[PageItem] = Seq(),
  ) extends GridCell {
    val pageRegion: PageRegion = mbrRegionFunc(headItem, tailItems)
  }


  case class InsertCell(
    char: Char
  ) extends GridCell



}
