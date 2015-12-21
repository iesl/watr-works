package edu.umass.cs.iesl.watr
package watrmarks
package dom


// import scala.annotation.tailrec
import scalaz.{TreeLoc}
import scalaz.std.stream._

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

  private def firstLeftsDownmost(tl: TreeLoc[WatrElement]): Option[TreeLoc[WatrElement]] = {
    tl.left match {
      case Some(l) => l.firstChild match {
        case Some(ld) => downRightMost(l)
        case None => None // firstLeftsDownmost(l)
      }
      case None => None
    }
  }

  private def downRightMost(tl: TreeLoc[WatrElement]): Option[TreeLoc[WatrElement]] = {
    val rightmost = unfold[Option[TreeLoc[WatrElement]], TreeLoc[WatrElement]](
      Some(tl)
    )({case t0 => t0 match {
      case Some(t) =>
        Some(t -> t.lastChild)
      case None => None
    }})
    rightmost.lastOption
  }

  private def nextLocDepthFirst(tl: TreeLoc[WatrElement]): Option[TreeLoc[WatrElement]] = (
    tl.firstChild orElse
      tl.right orElse
      firstParentRight(tl)
  )


  private def prevLocDepthFirst(tl: TreeLoc[WatrElement]): Option[TreeLoc[WatrElement]] = {
    // tl.left.flatMap{firstLeftsDownmost(_)}) orElse
    firstLeftsDownmost(tl) orElse
    tl.left orElse
    tl.parent
  }

  def prevElem: Option[DomCursor] =
    prevLocDepthFirst(loc).map(DomCursor(_))

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
