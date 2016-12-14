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

  import ComponentTypeEnrichments._

  type ReflowAndRange     = (TextReflow, RangeInt)
  type AtomOrInsert       = ReflowAndRange \/ ReflowAndRange
  type AtomOrInsertOrGap  = AtomOrInsert \/ RangeInt

  def anAtom(a: TextReflow, r:RangeInt): AtomOrInsertOrGap = -\/(-\/((a, r)))
  def anInsert(a: TextReflow, r: RangeInt): AtomOrInsertOrGap = -\/(\/-((a, r)))
  def aGap(r: RangeInt): AtomOrInsertOrGap = \/-(r)

  def isAnAtom(v: AtomOrInsertOrGap) = v.isLeft && v.left.isLeft
  def isAnInsert(v: AtomOrInsertOrGap) = v.isLeft && v.left.isRight
  def isAGap(v: AtomOrInsertOrGap) = v.isRight
  def hasAnAtom(as: List[AtomOrInsertOrGap]) = as.exists(isAnAtom(_))
  def allGaps(as: List[AtomOrInsertOrGap]) = as.all(isAGap(_))

  def toReflowAndRange(aig: AtomOrInsertOrGap): ReflowAndRange = {
    aig.fold(
      ai => ai.fold(
        a => a,
        i => i
      ),
      g => sys.error("toReflowAndRange error")
    )
  }

  def toReflowsAndRanges(as: List[AtomOrInsertOrGap]): List[ReflowAndRange] = as.map(toReflowAndRange(_))

  def toReflow(as: AtomOrInsertOrGap): TextReflow = toReflowAndRange(as)._1
  def toReflows(as: List[AtomOrInsertOrGap]): List[TextReflow] = toReflowsAndRanges(as).map(_._1)

  def toRange(aig: AtomOrInsertOrGap): RangeInt = {
    aig.fold(
      ai => ai.fold(a => a._2, i => i._2),
      g => g)
  }

  def toRanges(as: List[AtomOrInsertOrGap]): List[RangeInt] = as.map(toRange(_))

  def reduceRanges(as: List[RangeInt]) = as.reduce(_ union _)
  def reduceRangesRR(as: List[ReflowAndRange]) = reduceRanges(as.map(_._2))
  def reduceRangesAI(as: List[AtomOrInsertOrGap]) = reduceRangesRR(toReflowsAndRanges(as))

  def bubbleUpAIGs(
    aigs: List[AtomOrInsertOrGap],
    envRange: RangeInt,
    aif: TextReflowF[AtomOrInsertOrGap]
  ): AtomOrInsertOrGap = {
    if (allGaps(aigs)) {
      aGap(envRange)
    } else {
      if (hasAnAtom(aigs)) {
        anAtom(aif, envRange)
      } else {
        anInsert(aif, envRange)
      }
    }
  }

  def clipReflowToTargetRegion(theReflow: TextReflow, targetRegion: TargetRegion): Seq[ReflowAndRange] = {

    def retainAtoms(wfa: EnvT[Offsets, TextReflowF, List[AtomOrInsertOrGap]]): List[AtomOrInsertOrGap] = {
      val Offsets(cbegin, clen, _, _) = wfa.ask
      val envRange = RangeInt(cbegin, clen)

      val fa: TextReflowF[AtomOrInsertOrGap] = wfa.lower

      fa match {
        case Atom(c, ops) =>
          val pageAtom = c.asInstanceOf[PageAtom]
          val pageAtomTargetRegion = pageAtom.targetRegion
          val intersects = pageAtomTargetRegion.intersects(targetRegion)

          if (intersects) List(anAtom(atom(c, new TextReflowAtomOps(ops.toString)), envRange))
          else            List(aGap(envRange))

        case Insert(value) =>  List(anInsert(insert(value), envRange))

        case Rewrite(aigs, to)     =>
          List(bubbleUpAIGs(aigs,
            envRange,
            rewrite(aigs, to)
          ))

        case Bracket(pre, post, aigs) =>
          val childRange = toRange(a)
          val paddingLen = pre.length+post.length

          // if (childRange.len + paddingLen == envRange.len) {
          //   // the entirety of the child is included, so include the padding:
          //   // anAtom(bracket(pre, post, toReflow(a)), envRange)
          //   bubbleUpAIGs(aigs, (rr) => bracket(pre, post, rr._1, to), rr._2)
          // } else {
          //   bubbleUpAIGs(aigs, (rr) => bracket(pre, post, rr._1, to), rr._2)
          // }
          List(
            bubbleUpAIGs(aigs,
              envRange,
              bracket(pre, post, aigs)
            )
          )

        case Mask(mL, mR, aAttrS)       => ??? // Mask(mL, mR, a.get)

        case Flow(childAIGs) =>
          List(bubbleUpAIGs(childAIGs.flatten,
            envRange,
            flow(childAigs.flatten)
          ))
          // val childRange = reduceRangesAI(childAtomsOrInsertsOrGaps)
          // anAtom(flows(toReflows(childAtomsOrInsertsOrGaps)), childRange)

        case Labeled(labels, aigs)    =>
          List(bubbleUpAIGs(aigs,
            envRange,
            labeled(labels, aigs)
          ))

        case CachedText(a, text)   =>
          ??? // cache(a, attr)
      }


    }


    val res: AtomOrInsertOrGap =
      theReflow
        .annotateCharRanges
        .cata(retainAtoms)

    // val pruned = pruneAtomsAndInserts(List(res))
    //   .headOption
    //   .toList

    // toReflowsAndRanges(pruned)

    ???
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

// def anAtom(a: TextReflow, r:RangeInt) = -\/((a, r))
// def anInsert(a: TextReflow, r: RangeInt) = \/-((a, r))
// def someAtom(a: TextReflow, r:RangeInt): OptAtomOrInsert = Option(-\/((a, r)))
// def someInsert(a: TextReflow, r: RangeInt): OptAtomOrInsert = Option(\/-((a, r)))
// def neither: OptAtomOrInsert = None

// def pruneAtomsAndInserts(as: List[AtomOrInsertOrGap]): List[AtomOrInsertOrGap] =  {
//   if (hasAnAtom(as)) {
//    as
//       .trimLeftRightBy(isAnInsert(_))
//       .toList
//   } else List()
// }
// def filterToAtomsAndInserts[A](fa: TextReflowF[AtomOrInsertOrGap])
//     : TextReflowF[OptAtomOrInsert] = fa match {
//   case Flow(childAtomsOrInserts) =>
//     Flow(pruneAtomsAndInserts(childAtomsOrInserts)
//       .map(Option(_)))

//   case _ => fa
// }
