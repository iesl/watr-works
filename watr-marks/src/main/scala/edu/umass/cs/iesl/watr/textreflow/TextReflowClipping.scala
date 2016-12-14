package edu.umass.cs.iesl.watr
package textreflow

import scalaz._, Scalaz._

import spindex._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import matryoshka.patterns.EnvT

import utils.EnrichNumerics._
import scala.{Range => _}


trait TextReflowClipping extends TextReflowBasics {
  import TextReflowF._

  import ComponentTypeEnrichments._

  type ReflowRange = RangeInt
  type AtomOrInsertOrGap  = (ReflowRange \/ ReflowRange) \/ ReflowRange

  def ReflowRange(b: Int, len: Int) = RangeInt(b, len)

  def anAtom(r:ReflowRange): AtomOrInsertOrGap = -\/(-\/(r))
  def anInsert(r: ReflowRange): AtomOrInsertOrGap = -\/(\/-(r))
  def aGap(r: ReflowRange): AtomOrInsertOrGap = \/-(r)

  def isAnAtom(v: AtomOrInsertOrGap) = v.isLeft && v.left.isLeft
  def isAnInsert(v: AtomOrInsertOrGap) = v.isLeft && v.left.isRight
  def isAGap(v: AtomOrInsertOrGap) = v.isRight
  def hasAnAtom(as: List[AtomOrInsertOrGap]) = as.exists(isAnAtom(_))
  def allGaps(as: List[AtomOrInsertOrGap]) = as.all(isAGap(_))

  def pp(aig: AtomOrInsertOrGap): String = aig.fold(
    ai => ai.fold(
      a => s"atm(${a.min}, ${a.len})",
      i => s"ins(${i.min}, ${i.len})"
    ),
    g => s"gap(${g.min}, ${g.len})"
  )

  def setRange(aig: AtomOrInsertOrGap, r: ReflowRange): AtomOrInsertOrGap = {
    aig.fold(ai => ai.fold(_ => anAtom(r), _ => anInsert(r)), _ => aGap(r))
  }

  def setRanges(aigs: List[AtomOrInsertOrGap], r: ReflowRange): List[AtomOrInsertOrGap] =
    aigs.map(setRange(_, r))


  def toReflowRange(aig: AtomOrInsertOrGap): ReflowRange = {
    aig.fold(
      ai => ai.fold(a => a, i => i),
      g => g)
  }

  def toReflowRanges(as: List[AtomOrInsertOrGap]): List[ReflowRange] = as.map(toReflowRange(_))
  def reduceRanges(as: List[ReflowRange]) = as.reduce(_ union _)

  def clipReflowToTargetRegion(textReflow: TextReflow, targetRegion: TargetRegion): Seq[(TextReflow, RangeInt)] = {

    def retainAtoms(wfa: EnvT[Offsets, TextReflowF, List[AtomOrInsertOrGap]]): List[AtomOrInsertOrGap] = {
      val Offsets(cbegin, clen, _, _) = wfa.ask
      val envRange = ReflowRange(cbegin, clen)

      val fa: TextReflowF[List[AtomOrInsertOrGap]] = wfa.lower

      fa match {
        case Atom(c, ops) =>
          val tr = c.asInstanceOf[PageAtom].targetRegion
          val intersects = tr.intersects(targetRegion)

          if (intersects) List(anAtom(envRange))
          else            List(aGap(envRange))

        case Insert(value)            => List(anInsert(envRange))
        case Rewrite(aigs, to)        => setRanges(aigs, envRange)
        case Bracket(pre, post, aigs) => aigs
        case Mask(mL, mR, aAttrS)     => ??? // Mask(mL, mR, a.get)
        case Flow(childAIGs)          => childAIGs.flatten
        case Labeled(labels, aigs)    => aigs
        case CachedText(aigs, text)   => aigs
      }
    }


    val res: List[AtomOrInsertOrGap] =
      textReflow.annotateCharRanges
        .cata(retainAtoms)

    // find contiguous runs of atoms/inserts, divided by gaps

    println("res: " + res.map(pp(_)).mkString(", "))

    val firstRun = res
      .dropWhile(isAGap)
      .takeWhile(a => !isAGap(a))

    println("slice: " + firstRun.map(pp(_)).mkString(", "))

    val ranges = toReflowRanges(firstRun)
    val reducedRange = reduceRanges(ranges)

    val maybeSlice = textReflow.slice(reducedRange.min, reducedRange.max)

    maybeSlice
      .map( slice => List((slice, reducedRange)))
      .getOrElse(List())

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

// def reduceRanges(as: List[RangeInt]) = as.reduce(_ union _)
// def reduceRangesRR(as: List[Range]) = reduceRanges(as.map(_._2))
// def reduceRangesAI(as: List[AtomOrInsertOrGap]) = reduceRangesRR(toReflowsAndRanges(as))

// def bubbleUpAIGs(
//   aigs: List[AtomOrInsertOrGap],
//   envRange: ReflowRange,
//   aif: TextReflowF[AtomOrInsertOrGap]
// ): AtomOrInsertOrGap = {
//   if (allGaps(aigs)) {
//     aGap(envRange)
//   } else {
//     if (hasAnAtom(aigs)) {
//       anAtom(envRange)
//     } else {
//       anInsert(envRange)
//     }
//   }
// }
