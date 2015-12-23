package edu.umass.cs.iesl.watr
package watrmarks
package dom


import scalaz.{Show, TreeLoc, Tree}

sealed trait WatrElement

case class WatrDom(
  tree: Tree[WatrElement]
) {

  def prettyPrint: String = tree.drawTree

  def toDomCursor = DomCursor(tree.loc)

  def toCursor(l: BioLabel):Option[BioCursor] = for {
    tspanCursor <- toDomCursor.nextTSpan
    bioCursor <- BioCursor.initCursorFwd(l, tspanCursor)
  } yield bioCursor

}

case class Document (
  labelDictionary: BioLabelDictionary
) extends WatrElement  {
  override def toString = s"""<doc:>"""
}

case class Svg (
  transforms: List[Transform] = List()
) extends WatrElement  {
  override def toString = s"""<svg:${transforms}>"""
}

case class Desc (
) extends WatrElement {
  override def toString = s"""<desc:>"""
}

case class Grp (
  transforms: List[Transform] = List()
) extends WatrElement {
  override def toString = s"""<g:${transforms.mkString("{", ", ", "}")}>"""
}

case class Defs (
) extends WatrElement {
  override def toString = s"""<defs:>"""
}

case class Text (
  transforms: List[Transform] = List()
) extends WatrElement   {
  override def toString = s"""<text:${transforms}>"""
}

case class Path (
  transforms: List[Transform] = List()
) extends WatrElement   {
  override def toString = s"""<path:>"""
}

object NullElement extends WatrElement   {
  override def toString = s"""<null-elem>"""
}


case class TextXYOffsets(
  y: Double,
  xs: List[Double], endX: Double
)


// helper class for deserializing Dom - not to be used directly
private case class TSpanInit (
  text: String,
  transforms: List[Transform],
  textXYOffsets: Option[TextXYOffsets],
  fontSize: String,
  fontFamily: String,
  bioBrickStr: Option[String]
) extends WatrElement

case class TSpan (
  text: String,
  transforms: List[Transform],
  textXYOffsets: Option[TextXYOffsets],
  fontSize: String,
  fontFamily: String,
  bioBrick: BrickColumns,
  document: Document
) extends WatrElement  {

  override def toString = s"""<tspan:${text}>"""
}
