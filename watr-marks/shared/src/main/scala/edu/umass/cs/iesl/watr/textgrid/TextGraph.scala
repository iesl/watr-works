package edu.umass.cs.iesl.watr
package textgrid

import scala.collection.mutable
import rtrees._
import geometry._
import geometry.syntax._
import geometry.PageComponentImplicits._
import textboxing.{TextBoxing => TB}, TB._
import TypeTags._
import utils.GraphPaper

import utils.{Cursor, Cursors, Window}
import utils.SlicingAndDicing._
import utils.DoOrDieHandlers._

import watrmarks.Label
import scalaz.{@@ => _, _} //, Scalaz._
// import scala.scalajs.js.annotation._

import _root_.io.circe
import circe._
import circe.literal._

sealed trait Attr

object Attr {
  case class Glyph(
    glyph: TextGraph.GridCell
  ) extends Attr

  case object Empty extends Attr

}

sealed trait TextGraphShape extends LabeledShape[GeometricFigure, Attr]

object TextGraphShape {

  case class GlyphShape(
    shape: LTBounds,
    id: Int@@ShapeID,
    attr: Attr.Glyph,
    labels: Set[Label] = Set()
  ) extends TextGraphShape {
    def addLabels(l: Label*): GlyphShape = copy(
      labels = this.labels ++ l.toSet
    )
  }

  case class LabelShape(
    shape: LTBounds,
    id: Int@@ShapeID,
    parent: Option[LabelShape],
    labels: Set[Label] = Set()
  ) extends TextGraphShape {
    def attr: Attr = Attr.Empty

    def addLabels(l: Label*): LabelShape = copy(
      labels = this.labels ++ l.toSet
    )
  }

  implicit val ShowLabelShape: Show[LabelShape] = Show.shows[LabelShape]{ shape =>
    s"<shape#${shape.id}>"
  }

}

sealed trait MatrixArea {
  def area: GraphPaper.Box
}

object MatrixArea {

  case class Row(
    area: GraphPaper.Box,
    glyphs: Seq[TextGraphShape.GlyphShape]
  ) extends MatrixArea

  case class Rows(
    area: GraphPaper.Box,
    rows: Seq[Row]
  ) extends MatrixArea

}


trait TextGraph { self =>

  import TextGraph._
  import TextGraphShape._

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

  // def getMatrixContent(fromRow: Int=0, len: Int=Int.MaxValue): Option[MatrixArea.Rows]
  // def getContent(): Option[MatrixArea.Rows]

  def addLabel(row: Int, len: Int, label: Label, parent: Label): Option[LabelShape]

  def addLabel(row: Int, len: Int, label: Label): Option[LabelShape]

  def findLabelTrees(area: GraphPaper.Box): Seq[Tree[LabelShape]] 


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
