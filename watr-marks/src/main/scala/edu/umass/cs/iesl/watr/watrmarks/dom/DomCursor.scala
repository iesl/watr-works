package edu.umass.cs.iesl.watr
package watrmarks
package dom


// import scala.annotation.tailrec
import scalaz.{TreeLoc}

case class DomCursor(
  loc: TreeLoc[WatrElement]
) {


  def getLabel: WatrElement               = loc.getLabel
  def root: DomCursor                     = DomCursor(loc.root)
  def parent: Option[DomCursor]           = loc.parent map {p => DomCursor(p) }
  def left: Option[DomCursor]             = loc.left map {p => DomCursor(p) }
  def right: Option[DomCursor]            = loc.right map {p => DomCursor(p) }
  def firstChild: Option[DomCursor]       = loc.firstChild map {p => DomCursor(p) }
  def lastChild: Option[DomCursor]        = loc.lastChild map {p => DomCursor(p) }
  def getChild(n: Int): Option[DomCursor] = loc.getChild(n) map {p => DomCursor(p) }


  private def firstParentRight(tl: TreeLoc[WatrElement]): Option[TreeLoc[WatrElement]] = {
    tl.parent match {
      case Some(p) => p.right match {
        case Some(pr) => p.right
        case None => firstParentRight(p)
      }
      case None => None
    }
  }

  private def nextLocDepthFirst(tl: TreeLoc[WatrElement]): Option[TreeLoc[WatrElement]] = (
    tl.firstChild orElse
      tl.right orElse
      firstParentRight(tl)
  )

  def nextElem: Option[DomCursor] =
    nextLocDepthFirst(loc).map(DomCursor(_))

  def nextTSpan: Option[DomCursor] = {

    def _next(tl: TreeLoc[WatrElement]): Option[TreeLoc[WatrElement]] = {
      tl.getLabel match {
        case e: TSpan => Some(tl)
        case _        => nextLocDepthFirst(tl).flatMap(_next(_))
      }
    }

    nextLocDepthFirst(loc).flatMap{ nloc =>
      _next(nloc).map{DomCursor(_)}
    }
  }


  // def map[V](f: WatrElement => V): TreeLoc[B] = DomCursor(loc.map(f))

  // def findChild(p: Tree[A] =
  // def split(acc: TreeForest[A], xs: TreeForest[A]): Option[(TreeForest[A], Tree[A], TreeForest[A])] =
  // def find(p: DomCursor =
  // def toTree: Tree[A] =
  // def toForest: TreeForest[A] =
  // def isRoot: Boolean =
  // def isFirst: Boolean =
  // def isLast: Boolean =
  // def isLeaf: Boolean =
  // def isChild: Boolean =
  // def hasChildren: Boolean =

}
