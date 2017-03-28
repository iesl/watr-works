package edu.umass.cs.iesl.watr
package geometry

import textboxing.{TextBoxing => TB}
import watrmarks._
import scalaz.{@@ => _, _}, Scalaz._

import matryoshka._
import matryoshka.data._

import scalaz.{
  Traverse,
  Applicative,
  Show
}


// // This is going away in favor of the ZoneTree version
// case class Zone(
//   id: Int@@ZoneID,
//   regions: Seq[TargetRegion],
//   labels: Seq[Label]
// )


sealed trait ZoneTreeF[+A]

object ZoneTreeF {

  def fixf = Fix[ZoneTreeF](_)

  case class ZLeaf(
    region: GeometricRegion,
    label: Option[Label],
    zoneId: Option[Int@@ZoneID]
  ) extends ZoneTreeF[Nothing]

  case class ZNode[A](
    as: List[A],
    label: Option[Label],
    zoneId: Option[Int@@ZoneID]
  ) extends ZoneTreeF[A]


  implicit def ZoneTreeTraverse: Traverse[ZoneTreeF] = new Traverse[ZoneTreeF] {
    def traverseImpl[G[_], A, B](fa: ZoneTreeF[A])(f: A => G[B])(implicit G: Applicative[G]): G[ZoneTreeF[B]] = fa match {
      case fa: ZLeaf    => G.point(fa.copy())
      case fa@ ZNode(as, l, id)        => as.traverse(f).map(ZNode(_, l, id))
    }
  }

  implicit def ZoneTreeShow: Delay[Show, ZoneTreeF] = new Delay[Show, ZoneTreeF] {
    def apply[A](show: Show[A]) = Show.show {
      case fa@ ZLeaf(region, l, id)    => fa.toString()
      case fa@ ZNode(as, l, id)        => fa.toString()
    }
  }
}

object ZoneTrees {
  import matryoshka.implicits._
  import ZoneTreeF._

  type ZoneTree = Fix[ZoneTreeF]
  type Zone = ZoneTree
  type ZoneTreeT = ZoneTreeF[Fix[ZoneTreeF]]

  def leaf(r: GeometricRegion): ZoneTree = {
    fixf(ZLeaf(r, None, None))
  }

  def node(children: Seq[ZoneTree]): ZoneTree =
    fixf(ZNode(children.toList, None, None))

  def role(label: Label, a: ZoneTree): ZoneTree = fixf {
    a.project match {
      case z@ ZLeaf(region, optl, id) if optl.isEmpty => z.copy(label=Option(label))
      case z@ ZNode(as, optl, id)     if optl.isEmpty => z.copy(label=Option(label))
      case z => sys.error(s"Cannot assign new label to ${z}")
    }
  }

  def ref(zoneId: Int@@ZoneID, a: ZoneTree): ZoneTree = fixf {
    a.project match {
      case z@ ZLeaf(region, l, optid) if optid.isEmpty => z.copy(zoneId=Option(zoneId))
      case z@ ZNode(as, l, optid)     if optid.isEmpty => z.copy(zoneId=Option(zoneId))
      case z => sys.error(s"Cannot assign new zoneId to ${z}")
    }
  }

  import utils.ScalazTreeImplicits._

  def prettyPrintTree(zoneTree: ZoneTree): TB.Box = {
    zoneTree.cata(toTree).drawBox
  }


}
