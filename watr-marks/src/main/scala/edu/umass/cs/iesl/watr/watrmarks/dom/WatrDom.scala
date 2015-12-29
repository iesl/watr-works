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

  def toCursor(l: BioLabel):Option[LatticeCursor] = for {
    tspanCursor <- toDomCursor.nextTSpan
    bioCursor <- BioLattice.initFromDom(this).initLatticeCursor(l)
  } yield bioCursor


  def toSvg(): String = {
    // import scala.collection.mutable.Stack
    // val stack = Stack[String]()

    def transformString(e: Transformable): String = {
      val tstr = e.transforms.map({
        t => t match {
          case m:Matrix => List(m.m0, m.m1, m.m2, m.m3, m.m4, m.m5).mkString("matrix(", " ", ")")
          case m:Scale => List(m.m0, m.m1).mkString("scale(", " ", ")")
          case m:Translate => List(m.m0, m.m1).mkString("translate(", " ", ")")
        }
      }).mkString(" ")

      s"""transform=\"${tstr}\""""
    }


    def _toSvg(eloc: TreeLoc[WatrElement]): String = {
      val ch = eloc.firstChild.map(_toSvg(_))
      val rt = eloc.right.map(_toSvg(_))

      eloc.getLabel match {
        case e: WatrDom =>
          ""
        case e: Document  =>
          s"""|<?xml version="1.0"?>
              |${ch.getOrElse("")}
              |${rt.getOrElse("")}
              |""".stripMargin

        case e: Svg  =>
          val vb = s"${e.viewBox.x} ${e.viewBox.y} ${e.viewBox.width} ${e.viewBox.height}"
          s"""|<svg
              |  width="${e.width}"
              |  height="${e.height}"
              |  viewBox="$vb"
              |  ${transformString(e)}
              |>
              |${ch.getOrElse("")}
              |</svg>
              |${rt.getOrElse("")}
              |""".stripMargin

        case e: Desc  =>
          s"""|<desc>
              |${ch.getOrElse("")}
              |</desc>
              |${rt.getOrElse("")}
              |""".stripMargin
        case e: Grp  =>
          s"""|<g
              |  ${transformString(e)}
              |>
              |${ch.getOrElse("")}
              |</g>
              |${rt.getOrElse("")}
              |""".stripMargin
        case e: Defs  =>
          s"""|<defs>
              |${ch.getOrElse("")}
              |</defs>
              |${rt.getOrElse("")}
              |""".stripMargin
        case e: Text  =>
          s"""|<text
              |  ${transformString(e)}
              |>
              |  ${ch.getOrElse("")}
              |</text>
              |${rt.getOrElse("")}
              |""".stripMargin
        case e: Path  =>
          s"""|<path>
              |  ${transformString(e)}
              |>
              |${ch.getOrElse("")}
              |</path>
              |${rt.getOrElse("")}
              |""".stripMargin
        case e: TSpan  =>
          val xstr = e.textXYOffsets.map(o => "x="+o.xs.mkString("\"", " ", "\"")).getOrElse("")
          val ystr = e.textXYOffsets.map(o => "y="+o.ys.mkString("\"", " ", "\"")).getOrElse("")
          val endxstr = e.textXYOffsets.map(o => s"""endX=\"${o.endX}\"""").getOrElse("")

          s"""|<tspan
              |  $xstr
              |  $ystr
              |  $endxstr
              |  font-size="${e.fontSize}"
              |  font-family="${e.fontFamily}"
              |>${e.text}</tspan>
              |${rt.getOrElse("")}
              |""".stripMargin
        case e  =>
          ""
      }

    }

    _toSvg(tree.loc)
  }

}

sealed trait Transformable {
  def transforms: List[Transform]
}

case class Document (
  labelDictionary: BioLabelDictionary
) extends WatrElement  {
  override def toString = s"""<doc:>"""
}

case class ViewBox(x: Double, y: Double, width: Double, height: Double)

case class Svg (
  width: Double, height: Double, viewBox: ViewBox,
  transforms: List[Transform] = List()
) extends WatrElement with Transformable {
  override def toString = s"""<svg:${transforms}>"""
}

case class Desc (
) extends WatrElement {
  override def toString = s"""<desc:>"""
}

case class Grp (
  transforms: List[Transform] = List()
) extends WatrElement with Transformable {
  override def toString = s"""<g:${transforms.mkString("{", ", ", "}")}>"""
}

case class Defs (
) extends WatrElement {
  override def toString = s"""<defs:>"""
}

case class Text (
  transforms: List[Transform] = List()
) extends WatrElement  with Transformable  {
  override def toString = s"""<text:${transforms}>"""
}

case class Path (
  transforms: List[Transform] = List()
) extends WatrElement   with Transformable {
  override def toString = s"""<path:>"""
}

object NullElement extends WatrElement   {
  override def toString = s"""<null-elem>"""
}


case class TextXYOffsets(
  // sourceY: Double,
  xs: List[Double], endX: Double,
  ys: List[Double]
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
