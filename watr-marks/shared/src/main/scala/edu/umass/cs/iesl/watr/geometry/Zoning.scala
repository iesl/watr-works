package edu.umass.cs.iesl.watr
package geometry

// import textboxing.{TextBoxing => TB}
// import watrmarks._
// import scalaz.{@@ => _, _}, Scalaz._

// import matryoshka._
// import matryoshka.data._

// import scalaz.{
//   Traverse,
//   Applicative,
//   Show
// }

// import TypeTags._


// sealed trait ZoneTreeF[+A] {
//   def label: Option[Label]
//   def zoneId: Int@@ZoneID
// }

// object ZoneTreeF {

//   def fixf = Fix[ZoneTreeF](_)

//   case class ZLeaf(
//     regionId: Int@@RegionID,
//     label: Option[Label],
//     zoneId: Int@@ZoneID
//   ) extends ZoneTreeF[Nothing]

//   case class ZNode[A](
//     as: List[A],
//     label: Option[Label],
//     zoneId: Int@@ZoneID
//   ) extends ZoneTreeF[A]


//   implicit def ZoneTreeTraverse: Traverse[ZoneTreeF] = new Traverse[ZoneTreeF] {
//     def traverseImpl[G[_], A, B](fa: ZoneTreeF[A])(f: A => G[B])(implicit G: Applicative[G]): G[ZoneTreeF[B]] = fa match {
//       case fa: ZLeaf                => G.point(fa.copy())
//       case fa@ ZNode(as, l, id)     => as.traverse(f).map(ZNode(_, l, id))
//     }
//   }

//   implicit def ZoneTreeShow: Delay[Show, ZoneTreeF] = new Delay[Show, ZoneTreeF] {
//     def apply[A](show: Show[A]) = Show.show {
//       case fa@ ZLeaf(region, l, id)    => fa.toString()
//       case fa@ ZNode(as, l, id)        => fa.toString()
//     }
//   }
// }

// object ZoneTrees {
//   import matryoshka.implicits._
//   import ZoneTreeF._

//   type ZoneTree = Fix[ZoneTreeF]
//   type Zone = ZoneTree
//   type ZoneTreeT = ZoneTreeF[Fix[ZoneTreeF]]

//   def leaf(r: Int@@RegionID): ZoneTree = {
//     fixf(ZLeaf(r, None, ZoneID(0)))
//   }

//   def node(children: Seq[ZoneTree]): ZoneTree =
//     fixf(ZNode(children.toList, None, ZoneID(0)))

//   def role(label: Label, a: ZoneTree): ZoneTree = fixf {
//     a.project match {
//       case z@ ZLeaf(region, optl, id) if optl.isEmpty => z.copy(label=Option(label))
//       case z@ ZNode(as, optl, id)     if optl.isEmpty => z.copy(label=Option(label))
//       case z => sys.error(s"Cannot assign new label to ${z}")
//     }
//   }

//   def ref(zoneId: Int@@ZoneID, a: ZoneTree): ZoneTree = fixf {
//     a.project match {
//       case z@ ZLeaf(region, l, optid) => z.copy(zoneId=zoneId)
//       case z@ ZNode(as, l, optid)     => z.copy(zoneId=zoneId)
//     }
//   }

//   import utils.ScalazTreeImplicits._

//   def prettyPrintTree(zoneTree: ZoneTree): TB.Box = {
//     zoneTree.cata(toTree).drawBox
//   }

// }

// trait ZoneTreeFunctions {
//   import ZoneTreeF._
//   import matryoshka.implicits._

//   def getZoneId(zoneTree: Zone): Int@@ZoneID = {
//     zoneTree.project match {
//       case z@ ZLeaf(region, l, optid) => optid
//       case z@ ZNode(as, l, optid)     => optid
//     }
//   }
//   def getZoneLabel(zoneTree: Zone): Option[Label] = {
//     zoneTree.project match {
//       case z@ ZLeaf(region, l, optid) => l
//       case z@ ZNode(as, l, optid)     => l
//     }
//   }

//   def getZoneRegions(zone: Zone): Seq[Int@@RegionID] = {
//     zone.universe.map(_.project).toList.collect {
//       case ZLeaf(region, optl, id) => region
//     }
//   }
// }

// object ZoneTreeSyntax extends ZoneTreeFunctions {
//   import matryoshka.implicits._
//   import ZoneTrees._

//   implicit class RicherZoneTree(val theZoneTree: Zone) extends AnyVal {

//     def getId(): Int@@ZoneID = theZoneTree.project.zoneId
//     def id(): Int@@ZoneID = theZoneTree.project.zoneId
//     def label(): Option[Label] = theZoneTree.project.label

//     def getRegionIds(): Seq[Int@@RegionID] = getZoneRegions(theZoneTree)

//   }

// }
