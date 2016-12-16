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

  def isAnAtom(v: AtomOrInsertOrGap)   = v.fold(_.fold(_ => true, _  => false), _ => false)
  def isAnInsert(v: AtomOrInsertOrGap) = v.fold(_.fold(_ => false, _ => true),  _ => false)
  def isAGap(v: AtomOrInsertOrGap)     = v.fold(_.fold(_ => false, _ => false), _ => true)
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
        case Bracket(pre, post, aigs) => setRanges(aigs, envRange)
        case Mask(mL, mR, aAttrS)     => ??? // Mask(mL, mR, a.get)
        case Flow(childAIGs)          => childAIGs.flatten
        case Labeled(labels, aigs)    => aigs
        case CachedText(aigs, text)   => aigs
      }
    }


    val res: List[AtomOrInsertOrGap] =
      textReflow.annotateCharRanges
        .cata(retainAtoms)

    // println("res: " + res.map(pp(_)).mkString(", "))

    def splitLoop(aigs: List[AtomOrInsertOrGap]): List[List[AtomOrInsertOrGap]] = {
      val start = aigs.dropWhile(isAGap)
      if (start.isEmpty) Nil else {
        val (h, rest) = start.span(a => !isAGap(a))
        h :: splitLoop(rest)
      }
    }

    val nonGaps = splitLoop(res).filter(hasAnAtom)

    val clippedRanges = nonGaps.map({nonGap =>
      // println("  no gaps:" + nonGap.toList.map(pp(_)).mkString(", "))
      val ranges = toReflowRanges(nonGap.toList)
      val aggRange = reduceRanges(ranges)

      textReflow.slice(aggRange.min, aggRange.max)
        .map(slice => (slice, aggRange))
    })

    clippedRanges.flatten
  }

}
