package edu.umass.cs.iesl.watr
package watrmarks
package dom


import scalaz.{TreeLoc}

case class DomCursor(
  loc: TreeLoc[WatrElement]
) {

  def getLabel: WatrElement                   = loc.getLabel
  def root: DomCursor                     = DomCursor(loc.root)
  def parent: Option[DomCursor]           = loc.parent map {p => DomCursor(p) }
  def left: Option[DomCursor]             = loc.left map {p => DomCursor(p) }
  def right: Option[DomCursor]            = loc.right map {p => DomCursor(p) }
  def firstChild: Option[DomCursor]       = loc.firstChild map {p => DomCursor(p) }
  def lastChild: Option[DomCursor]        = loc.lastChild map {p => DomCursor(p) }
  def getChild(n: Int): Option[DomCursor] = loc.getChild(n) map {p => DomCursor(p) }


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

