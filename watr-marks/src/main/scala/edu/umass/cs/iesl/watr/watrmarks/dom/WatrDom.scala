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
