// package edu.umass.cs.iesl.watr
// package watrmarks
// package dom

// // import scala.annotation.tailrec
// import scalaz.{Tree, TreeLoc, Node}

// case class DomCursor(
//   loc: TreeLoc[WatrElement]
// ) {

//   override def toString = {
//     val path = loc.path.reverse.mkString(" :: ")
//     path
//   }


//   def showBox: TB.Box = {
//     val parentPath = loc
//       .parents
//       .reverse
//       .map({ case (ls, focus, rs) =>
//         val branchIndex = (
//           if (ls.length>0) s"[${ls.length}]"
//           else ""
//         )
//         focus.getClass.getSimpleName.toLowerCase+branchIndex
//       }).mkString("", "/", "/")

//     val index = (
//       if (loc.lefts.length>0) s"[${loc.lefts.length}]"
//       else ""
//     )

//     val selfNode = loc.getLabel.getClass.getSimpleName.toLowerCase+index

//     parentPath+selfNode

//   }

//   def getLabel: WatrElement               = loc.getLabel
//   def setLabel(a: WatrElement): DomCursor = modifyTree((t: Tree[WatrElement]) => Tree.node(a, t.subForest))

//   def modifyLabel(f: WatrElement => WatrElement): DomCursor = setLabel(f(getLabel))


//   def root: DomCursor                     = DomCursor(loc.root)
//   def parent: Option[DomCursor]           = loc.parent map {p => DomCursor(p) }
//   def left: Option[DomCursor]             = loc.left map {p => DomCursor(p) }
//   def right: Option[DomCursor]            = loc.right map {p => DomCursor(p) }
//   def firstChild: Option[DomCursor]       = loc.firstChild map {p => DomCursor(p) }
//   def lastChild: Option[DomCursor]        = loc.lastChild map {p => DomCursor(p) }
//   def getChild(n: Int): Option[DomCursor] = loc.getChild(n) map {p => DomCursor(p) }

//   def setTree(t: Tree[WatrElement]): DomCursor = DomCursor(TreeLoc.loc(t, loc.lefts, loc.rights, loc.parents))
//   def modifyTree(f: Tree[WatrElement] => Tree[WatrElement]): DomCursor = setTree(f(loc.tree))

//   def map[V](f: WatrElement => WatrElement): DomCursor = DomCursor(loc.map(f))

//   def getLabelAsTSpan: TSpan = getLabel.asInstanceOf[TSpan]
//   def isFocusedOnTSpan: Boolean = getLabel.isInstanceOf[TSpan]

//   def unfoldTSpansCursors: Stream[DomCursor] = {
//     import scalaz.std.stream._
//     val start = if (isFocusedOnTSpan){
//       Some(this)
//     } else nextTSpan
//     unfold[Option[DomCursor], DomCursor](
//       start
//     )(_ match {
//       case Some(dcur) => Some(dcur -> dcur.nextTSpan)
//       case _ => None
//     })
//   }

//   private def firstParentRight(tl: TreeLoc[WatrElement]): Option[TreeLoc[WatrElement]] = {
//     tl.parent match {
//       case Some(p) => p.right match {
//         case Some(pr) => p.right
//         case None => firstParentRight(p)
//       }
//       case None => None
//     }
//   }

//   private def firstLeftsDownmost(tl: TreeLoc[WatrElement]): Option[TreeLoc[WatrElement]] = {
//     tl.left match {
//       case Some(l) => l.firstChild match {
//         case Some(ld) => downRightMost(l)
//         case None => None // firstLeftsDownmost(l)
//       }
//       case None => None
//     }
//   }

//   private def downRightMost(tl: TreeLoc[WatrElement]): Option[TreeLoc[WatrElement]] = {
//     import scalaz.std.stream._
//     val rightmost = unfold[Option[TreeLoc[WatrElement]], TreeLoc[WatrElement]](
//       Some(tl)
//     )({case t0 => t0 match {
//       case Some(t) =>
//         Some(t -> t.lastChild)
//       case None => None
//     }})
//     rightmost.lastOption
//   }

//   private def nextLocDepthFirst(tl: TreeLoc[WatrElement]): Option[TreeLoc[WatrElement]] = (
//     tl.firstChild orElse
//       tl.right orElse
//       firstParentRight(tl)
//   )


//   private def prevLocDepthFirst(tl: TreeLoc[WatrElement]): Option[TreeLoc[WatrElement]] = {
//     firstLeftsDownmost(tl) orElse
//     tl.left orElse
//     tl.parent
//   }

//   def prevElem: Option[DomCursor] =
//     prevLocDepthFirst(loc).map(DomCursor(_))

//   def nextElem: Option[DomCursor] =
//     nextLocDepthFirst(loc).map(DomCursor(_))

//   def nextTSpan: Option[DomCursor] = {

//     def _next(tl: TreeLoc[WatrElement]): Option[TreeLoc[WatrElement]] = {
//       tl.getLabel match {
//         case e: TSpan =>
//           Some(tl)
//         case _        =>
//           nextLocDepthFirst(tl).flatMap(_next(_))
//       }
//     }

//     nextLocDepthFirst(loc).flatMap{ nloc =>
//       _next(nloc).map{DomCursor(_)}
//     }
//   }



//   // def findChild(p: Tree[A] =
//   // def split(acc: TreeForest[A], xs: TreeForest[A]): Option[(TreeForest[A], Tree[A], TreeForest[A])] =
//   // def find(p: DomCursor =
//   // def toTree: Tree[A] =
//   // def toForest: TreeForest[A] =
//   // def isRoot: Boolean =
//   // def isFirst: Boolean =
//   // def isLast: Boolean =
//   // def isLeaf: Boolean =
//   // def isChild: Boolean =
//   // def hasChildren: Boolean =

// }
