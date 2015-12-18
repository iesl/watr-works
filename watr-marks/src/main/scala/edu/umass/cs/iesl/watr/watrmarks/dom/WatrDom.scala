package edu.umass.cs.iesl.watr
package watrmarks
package dom


import scalaz.{Show, TreeLoc, Tree}

case class Cursor(
  focii: Seq[(WatrDomCursor, BrickCursor)]
) {

  def getText: String =  {
    focii.map({ case (dcur, bcur) =>
      bcur.current.map(_.char).mkString
    }).mkString
  }

  def next: Option[Cursor] = {
    val (ldcur, lbcur) = focii.last

    lbcur.next match {
      case Some(ncur) =>
      case None =>
    }
    None
  }


  def foreach(f: (Cursor) => Unit): Unit = {
    println("running foreach ")

    f(this)
    next match {
      case Some(ncur) => ncur.foreach(f)
      case None =>
    }
  }

  override def toString = {
    val fs = focii.map{ case (dcur,bcur) =>

      println(s"dcur: ${dcur}")
      println(s"bcur: ${bcur}")

      s""" ${dcur.toString}
      ${bcur.toString}"""
    }.mkString("\n")
    s"cur<${fs}; nx=>"
  }

}

case class WatrDomCursor(
  loc: TreeLoc[WatrElement]
) {

  def getLabel: WatrElement                   = loc.getLabel
  def root: WatrDomCursor                     = WatrDomCursor(loc.root)
  def parent: Option[WatrDomCursor]           = loc.parent map {p => WatrDomCursor(p) }
  def left: Option[WatrDomCursor]             = loc.left map {p => WatrDomCursor(p) }
  def right: Option[WatrDomCursor]            = loc.right map {p => WatrDomCursor(p) }
  def firstChild: Option[WatrDomCursor]       = loc.firstChild map {p => WatrDomCursor(p) }
  def lastChild: Option[WatrDomCursor]        = loc.lastChild map {p => WatrDomCursor(p) }
  def getChild(n: Int): Option[WatrDomCursor] = loc.getChild(n) map {p => WatrDomCursor(p) }


  // def map[V](f: WatrElement => V): TreeLoc[B] = WatrDomCursor(loc.map(f))

  // def findChild(p: Tree[A] =
  // def split(acc: TreeForest[A], xs: TreeForest[A]): Option[(TreeForest[A], Tree[A], TreeForest[A])] =
  // def find(p: WatrDomCursor =
  // def toTree: Tree[A] =
  // def toForest: TreeForest[A] =
  // def isRoot: Boolean =
  // def isFirst: Boolean =
  // def isLast: Boolean =
  // def isLeaf: Boolean =
  // def isChild: Boolean =
  // def hasChildren: Boolean =

}


sealed trait WatrElement

case class WatrDom(
  tree: Tree[WatrElement]
) {

  def prettyPrint: String = tree.drawTree

  def toDomCursor = WatrDomCursor(tree.loc)

  def toCursor(l: BioLabel) = {
    // val (nodesBeforeLabel, nodesStartingWithLabel) = tree.loc
    val nodesStartingWithLabel = tree.loc
      .cojoin
      .toTree.flatten
      .filter(_.getLabel.isInstanceOf[TSpan])
      .dropWhile({ domloc =>
        val tspan = domloc.getLabel.asInstanceOf[TSpan]
        tspan.bioBrick.toBrickCursor(l).isEmpty
      })

      // .span({ maybeBcur =>
      //   val tspan = domloc.getLabel.asInstanceOf[TSpan]
      //   println("tspan: "+tspan)
      //   tspan.bioBrick.toBrickCursor(l).isEmpty
      // })


    val nodesWithLabelMinusOne =
      nodesStartingWithLabel
        .takeWhile({ domloc =>
          val tspan = domloc.getLabel.asInstanceOf[TSpan]
          // !tspan.bioBrick.toBrickCursor(l).get.coversEndOfLabel
          val bioBrick = tspan.bioBrick
          println(s"bioBrick: ${bioBrick}; \n\n")
          bioBrick.toBrickCursor(l).exists {
            bcur => !bcur.coversEndOfLabel
          }
        })
        // .span( { domloc =>
        //   val tspan = domloc.getLabel.asInstanceOf[TSpan]
        //   // !tspan.bioBrick.toBrickCursor(l).get.coversEndOfLabel
        //   val bioBrick = tspan.bioBrick
        //   println(s"bioBrick: ${bioBrick}; \n\n")
        //   val obcur = bioBrick.toBrickCursor(l)
        //   val bcur = obcur.get

        //   !bcur.coversEndOfLabel
        // })

    // import _root_.ammonite.repl.Main._
    // debug("self" -> this, "nodesStartingWithLabel" -> nodesStartingWithLabel, "nodesWithLabelMinusOne" -> nodesWithLabelMinusOne)
 

    val nodesWithLabel =  nodesWithLabelMinusOne ++ nodesStartingWithLabel.drop(nodesWithLabelMinusOne.length).take(1)

    // val nodesAfterLabel = nodesAfterLabelPlusOne.drop(1)

    val nodeBrickCursorPairs =
      nodesWithLabel.map({nloc => (
        WatrDomCursor(nloc),
        nloc.getLabel.asInstanceOf[TSpan].bioBrick.toBrickCursor(l).get
      )})

    Cursor(nodeBrickCursorPairs)
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

  def emptyBioBrick: LabeledSpan = LabeledSpan((text zip bounds.getOrElse(List())).toList.map{ case (char, bnd) =>
    LabeledColumn(Set(), char, Some(fontInfo), Some(bnd))
  })

  lazy val bioBrick: LabeledSpan = {
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
