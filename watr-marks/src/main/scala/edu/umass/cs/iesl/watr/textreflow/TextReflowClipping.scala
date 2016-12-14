package edu.umass.cs.iesl.watr
package textreflow

import scalaz._, Scalaz._

import spindex._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import matryoshka.patterns.EnvT

import utils.EnrichNumerics._

trait TextReflowClipping extends StructuredRecursion {
  import TextReflowF._
  import utils.SlicingAndDicing._

  import ComponentTypeEnrichments._

  type ReflowAndRange     = (TextReflow, RangeInt)
  type AtomOrInsert       = ReflowAndRange \/ ReflowAndRange
  type OptAtomOrInsert    = Option[AtomOrInsert]
  // type OptAtomsOrInserts  = List[OptAtomOrInsert]
  // type AtomsOrInserts     = List[AtomOrInsert]

  def anAtom(a: TextReflow, r:RangeInt) = -\/((a, r))
  def anInsert(a: TextReflow, r: RangeInt) = \/-((a, r))
  def someAtom(a: TextReflow, r:RangeInt): OptAtomOrInsert = Option(-\/((a, r)))
  def someInsert(a: TextReflow, r: RangeInt): OptAtomOrInsert = Option(\/-((a, r)))
  def neither: OptAtomOrInsert = None
  def isAnAtom(v: AtomOrInsert) = v.isLeft
  def isAnInsert(v: AtomOrInsert) = v.isRight
  def hasAnAtom(as: List[AtomOrInsert]) = as.exists(isAnAtom(_))

  def toReflowAndRange(as: AtomOrInsert): ReflowAndRange = as.getOrElse(as.swap.toOption.get)

  def toReflowsAndRanges(as: List[AtomOrInsert]): List[ReflowAndRange] = as.map(toReflowAndRange(_))

  def toReflow(as: AtomOrInsert): TextReflow = toReflowAndRange(as)._1
  def toReflows(as: List[AtomOrInsert]): List[TextReflow] = toReflowsAndRanges(as).map(_._1)
  def toRanges(as: List[AtomOrInsert]): List[RangeInt] = toReflowsAndRanges(as).map(_._2)
  def toRange(as: AtomOrInsert): RangeInt = toReflowAndRange(as)._2

  def reduceRanges(as: List[RangeInt]) = as.reduce(_ union _)
  def reduceRangesRR(as: List[ReflowAndRange]) = reduceRanges(as.map(_._2))
  def reduceRangesAI(as: List[AtomOrInsert]) = reduceRangesRR(toReflowsAndRanges(as))

  def pruneAtomsAndInserts(as: List[List[OptAtomOrInsert]]): List[List[AtomOrInsert]] = for {
    atomsOrInserts      <- as
    atomsAndInserts      = for {
      maybeAtomOrInsert <- atomsOrInserts
      atomOrInsert      <- maybeAtomOrInsert
    } yield atomOrInsert
    if hasAnAtom(atomsAndInserts)
  } yield {
    atomsAndInserts
      .trimLeftRightBy(isAnInsert(_))
      .toList
  }

  def clipReflowToTargetRegion(theReflow: TextReflow, targetRegion: TargetRegion): Seq[(TextReflow, RangeInt)] = {

    def filterToAtomsAndInserts[A](fa: TextReflowF[List[OptAtomOrInsert]])
        : TextReflowF[List[OptAtomOrInsert]] = fa match {
      case Flow(childAtomsOrInserts) =>
        Flow(pruneAtomsAndInserts(childAtomsOrInserts)
          .map(_.map(Option(_))))

      case _ => fa
    }


    def retainAtoms(wfa: EnvT[Offsets, TextReflowF, List[OptAtomOrInsert]]): List[OptAtomOrInsert] = {
      val Offsets(cbegin, clen, _, _) = wfa.ask
      val envRange = RangeInt(cbegin, clen)

      val fa: TextReflowF[List[OptAtomOrInsert]] = wfa.lower
      val faFiltered: TextReflowF[List[OptAtomOrInsert]] = filterToAtomsAndInserts(fa)
      val maybeFa: Option[TextReflowF[List[AtomOrInsert]]] = faFiltered.traverse(_.sequence)

      for {
        textReflowF        <- maybeFa
      } yield {
        textReflowF match {
          case Atom(c, ops) =>
            val pageAtom = c.asInstanceOf[PageAtom]
            val pageAtomTargetRegion = pageAtom.targetRegion
            val intersects = pageAtomTargetRegion.intersects(targetRegion)

            if (pageAtom.targetRegion.intersects(targetRegion))
              List(someAtom(atom(c, new TextReflowAtomOps(ops.toString)), envRange))
            else List(neither)

          case Insert(value) =>  List(someInsert(insert(value), envRange))

          case Rewrite(fromAttrs, to)     =>
            val children = toReflowsAndRanges(fromAttrs)
            children.map({case (tr, trRange) =>
              someAtom(rewrite(tr, to), envRange)
            })

          case Bracket(pre, post, aAttrs) =>
            val children = toReflowsAndRanges(aAttrs)

            val paddingLen = pre.length+post.length
            val childRangeUnion = children.map(_._2).reduce(_ union _)
            if (childRangeUnion.len + paddingLen == envRange.len) {
              // the entirety of the children is included, so include the padding:
              children.map(a => someAtom(bracket(pre, post, a._1), envRange))
            } else {
              children.map(a => someAtom(bracket("", "", a._1), childRangeUnion))
            }

          case Mask(mL, mR, aAttrS)       => ??? // Mask(mL, mR, a.get)

          case Flow(childAtomsOrInserts) =>
            val in: List[List[AtomOrInsert]] = childAtomsOrInserts

            val all: List[(List[AtomOrInsert], RangeInt)] = for {
              atomsOrInserts <- childAtomsOrInserts
            } yield {
              val reducedRange = reduceRangesAI(atomsOrInserts)

              // val atomsOrInserts = atomsOrInserts
              //   .map(x => (someAtom(toReflow(x), toRange(x))))

              // val optList[AtomOrInsert] = atomsOrInserts
              //   .map(x => (someAtom(toReflow(x), toRange(x))))

              // (optList[AtomOrInsert], reducedRange)
              (atomsOrInserts, reducedRange)
            }

            val reducedRange = reduceRanges(all.map(_._2))

            val reducedFlows = all.map(x => flows(toReflows(x._1)))

            // someAtom(flow(childList[AtomOrInsert].map(Option(_))), reduceRange)
            // childList[AtomOrInsert].map

            def res: List[OptAtomOrInsert] = ???

            res

          case Labeled(labels, aAttrs)    =>
            // range.intersect(attr).map(xr => ((labeled(labels, a), xr)))
            ???
          case CachedText(aAttrs, text)   =>
            ??? // cache(a, attr)
        }

      }

      ???
    }

    val res: List[OptAtomOrInsert] = theReflow
      .annotateCharRanges
      .cata(retainAtoms)

    val pruned = pruneAtomsAndInserts(List(res)).head
    toReflowsAndRanges(pruned)

  }

}








// def clipToTargetRegionXXX(targetRegion: TargetRegion): Option[(TextReflow, RangeInt)] = {
//   def retain(wfa: EnvT[Offsets, TextReflowF, Option[(TextReflow, RangeInt)]]): Option[(TextReflow, RangeInt)] = {
//     val Offsets(cbegin, clen, _, _) = wfa.ask
//     val range = RangeInt(cbegin, clen)

//     val fa: TextReflowF[Option[(TextReflow, RangeInt)]] =
//       trimFlow[Option[(TextReflow, RangeInt)]](_.isDefined)(wfa.lower)

//     val t1: Option[TextReflowF[(TextReflow, RangeInt)]] = fa.sequence

//     val tz: Option[(TextReflow, RangeInt)] = t1.map(trF => (fixf(trF.map(_._1)), range) )

//     // println(s"retain on ${tz} for targetRegion=${targetRegion.bbox.prettyPrint}")
//     if (cbegin < 0) tz else {
//       val r = RangeInt(cbegin, clen)

//       for {
//         (textReflow, range) <- tz
//         textReflowF        <- t1
//         m <- textReflowF match {
//           case Atom(c, ops) =>
//             val pageAtom = c.asInstanceOf[PageAtom]
//             val pageAtomTargetRegion = pageAtom.targetRegion
//             val intersects = pageAtomTargetRegion.intersects(targetRegion)

//             // println(s"Atom:  ${pageAtomTargetRegion}.intersects(${targetRegion})=$intersects")
//             // if (intersects) { println(s"Atom: range ${range} intersects=$intersects") }

//             if (pageAtom.targetRegion.intersects(targetRegion)) Some(
//               (atom(c, new TextReflowAtomOps(ops.toString)), range)
//             ) else None

//           case Insert(value)                 => Some((insert(value), range))
//           case Rewrite((from, attr), to)     => // range.intersect(attr).map(xr => (rewrite(from, to), xr))
//             Some((rewrite(from, to), range))
//           case Bracket(pre, post, (a, attr)) => range.intersect(attr).map(xr => ((bracket(pre, post, a), xr)))
//           case Mask(mL, mR, (a, attr))       => ??? // Mask(mL, mR, a.get)
//           case Flow(atomsAndattrs)           =>
//             val filtered = atomsAndattrs.filter(_._2.intersect(range).isDefined)
//             if (filtered.isEmpty) None else {
//               val totalRange: RangeInt = filtered.map(_._2).reduce(_.union(_))
//               Some((flows(filtered.map(_._1)), totalRange))
//             }
//           case Labeled(labels, (a, attr))    => range.intersect(attr).map(xr => ((labeled(labels, a), xr)))
//           case CachedText((a, attr), text)   => ??? // cache(a, attr)
//         }
//       } yield {
//         // println(s"yield : ${m}")
//         m
//       }

//     }
//   }

    //   theReflow
    //     .annotateCharRanges
    //     .cata(retain)
    // }

// val hasAnAtom = (for {
//   maybeAtomOrInserts <- atomsOrInsertsSeq
//   maybeAtomOrInsert <- maybeAtomOrInserts
// } yield maybeAtomOrInsert.exists(isAnAtom(_))).exists(b => b)


// def filterToAtomsAndInserts[A](fa: TextReflowF[List[OptAtomOrInsert]]): TextReflowF[List[OptAtomOrInsert]] = {
//   fa match {
//     case tr@ Flow(atomsOrInsertsSeq) =>

//       val removedNones = for {
//         maybeAtomOrInserts <- atomsOrInsertsSeq
//         maybeAtomOrInsert <- maybeAtomOrInserts
//         atomOrInsert      <- maybeAtomOrInsert
//       } yield atomOrInsert

//       val trimmedInserts: List[OptAtomOrInsert] = removedNones
//         .trimLeftRightBy(isAnInsert(_))
//         .map(Option(_)).toList

//       if (trimmedInserts.isEmpty) {
//         Flow(List(List(neither)))
//       } else {
//         Flow(List(trimmedInserts))
//       }
//     case _ => fa
//   }
// }
