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

  def toCursor(l: BioLabel) = {
    // val (nodesBeforeLabel, nodesStartingWithLabel) = tree.loc
    val nodesStartingWithLabel = tree.loc
      .cojoin
      .toTree.flatten
      .filter(_.getLabel.isInstanceOf[TSpan])
      .dropWhile({ domloc =>
        val tspan = domloc.getLabel.asInstanceOf[TSpan]
        tspan.bioBrick.initBrickCursor(l).isEmpty
      })

      // .span({ maybeBcur =>
      //   val tspan = domloc.getLabel.asInstanceOf[TSpan]
      //   println("tspan: "+tspan)
      //   tspan.bioBrick.initBrickCursor(l).isEmpty
      // })


    val nodesWithLabelMinusOne =
      nodesStartingWithLabel
        .takeWhile({ domloc =>
          val tspan = domloc.getLabel.asInstanceOf[TSpan]
          // !tspan.bioBrick.initBrickCursor(l).get.coversEndOfLabel
          val bioBrick = tspan.bioBrick
          println(s"bioBrick: ${bioBrick}; \n\n")
          bioBrick.initBrickCursor(l).exists {
            bcur => !bcur.coversEndOfLabel
          }
        })
        // .span( { domloc =>
        //   val tspan = domloc.getLabel.asInstanceOf[TSpan]
        //   // !tspan.bioBrick.initBrickCursor(l).get.coversEndOfLabel
        //   val bioBrick = tspan.bioBrick
        //   println(s"bioBrick: ${bioBrick}; \n\n")
        //   val obcur = bioBrick.initBrickCursor(l)
        //   val bcur = obcur.get

        //   !bcur.coversEndOfLabel
        // })

    // import _root_.ammonite.repl.Main._
    // debug("self" -> this, "nodesStartingWithLabel" -> nodesStartingWithLabel, "nodesWithLabelMinusOne" -> nodesWithLabelMinusOne)


    val nodesWithLabel =  nodesWithLabelMinusOne ++ nodesStartingWithLabel.drop(nodesWithLabelMinusOne.length).take(1)

    // val nodesAfterLabel = nodesAfterLabelPlusOne.drop(1)

    val nodeBrickCursorPairs =
      nodesWithLabel.map({nloc => (
        DomCursor(nloc),
        nloc.getLabel.asInstanceOf[TSpan].bioBrick.initBrickCursor(l).get
      )})

    BioCursor(nodeBrickCursorPairs)
  }

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

case class TSpan (
  text: String,
  transforms: List[Transform],
  textXYOffsets: Option[TextXYOffsets],
  fontSize: String,
  fontFamily: String,
  bioBrickStr: Option[String],
  document: Document
) extends WatrElement  {

  // println(s"text.len=${text.length()}, textxyoffsets = $textXYOffsets")
  // assert(text.length==0 || textXYOffsets.exists(_.xs.length==text.length))

  override def toString = s"""<tspan:${text}>"""


  def bounds: Option[List[TextBounds]] =
    textXYOffsets.map {
      xyoffs => xyoffs.xs.map{x => TextBounds(
        left   = x,
        bottom = xyoffs.y,
        width  = 1,
        height = 1
      )
    }
  }


  lazy val fontInfo =  FontInfo(fontFamily, fontSize)

  def fonts: List[FontInfo] = List.fill(text.length)(fontInfo)

  def emptyBioBrick: BrickColumns = BrickColumns((text zip bounds.getOrElse(List())).toList.map{ case (char, bnd) =>
    BrickColumn(Set(), char, Some(fontInfo), Some(bnd))
  })

  lazy val bioBrick: BrickColumns = {
    bioBrickStr.map{str =>
      biolu.parseBioBrick(
        str,
        document.labelDictionary,
        Some(text),
        Some(bounds.getOrElse(List())),
        Some(fonts)
      )
    } getOrElse emptyBioBrick
  }
}
