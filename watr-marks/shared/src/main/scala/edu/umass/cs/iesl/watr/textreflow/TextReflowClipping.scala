package edu.umass.cs.iesl.watr
package textreflow

import scalaz._, Scalaz._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import matryoshka.patterns.EnvT

import utils.EnrichNumerics._
import scala.{Range => _}

import geometry._
import watrmarks.Label

trait TextReflowClipping extends TextReflowBasics {
  import TextReflowF._
  import GeometryImplicits._

  type ReflowRange = RangeInt
  type AtomOrInsertOrGap  = (ReflowRange \/ ReflowRange) \/ ReflowRange

  protected object helpers {

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
  }

  import helpers._

  def clipReflowToTargetRegion(textReflow: TextReflow, targetRegion: TargetRegion): Seq[(TextReflow, RangeInt)] = {

    def retainAtoms(wfa: EnvT[Offsets, TextReflowF, List[AtomOrInsertOrGap]]): List[AtomOrInsertOrGap] = {
      val Offsets(cbegin, clen, _, _) = wfa.ask
      val envRange = ReflowRange(cbegin, clen)

      val fa: TextReflowF[List[AtomOrInsertOrGap]] = wfa.lower

      fa match {
        case Atom(c) =>
          val tr = c.bbox
          val intersects = tr.intersects(targetRegion.bbox)

          if (intersects) List(anAtom(envRange))
          else            List(aGap(envRange))

        case Insert(value)            => List(anInsert(envRange))
        case Rewrite(aigs, to)        => setRanges(aigs, envRange)
        case Bracket(pre, post, aigs) => setRanges(aigs, envRange)
        // case Mask(mL, mR, aAttrS)     => ??? // Mask(mL, mR, a.get)
        case Flow(childAIGs)          => childAIGs.flatten
        case Labeled(labels, aigs)    => aigs
          // case CachedText(aigs, text)   => aigs
      }
    }

    val res: List[AtomOrInsertOrGap] =
      annotateReflowCharRanges(textReflow)
        .cata(retainAtoms)

    def splitLoop(aigs: List[AtomOrInsertOrGap]): List[List[AtomOrInsertOrGap]] = {
      val start = aigs.dropWhile(isAGap)
      if (start.isEmpty) Nil else {
        val (h, rest) = start.span(a => !isAGap(a))
        h :: splitLoop(rest)
      }
    }

    val nonGaps = splitLoop(res).filter(hasAnAtom)

    val clippedRanges = nonGaps.map({nonGap =>
      val ranges = toReflowRanges(nonGap.toList)
      val aggRange = reduceRanges(ranges)

      sliceTextReflow(textReflow, aggRange.min, aggRange.max)
        .map(slice => (slice, aggRange))
    })

    clippedRanges.flatten
  }


  def labelSplit(tr: TextReflow, label: Label): Seq[(TextReflow, Option[Label])] = {
    import scalaz.std.list._

    def visit(
      t: TextReflowF[(TextReflow, List[(TextReflow, Option[Label])])]
    ): List[(TextReflow, Option[Label])] = orBubblePara[List[(TextReflow, Option[Label])]](t) {
      case l @ Labeled (labels, (a, attr)) =>
        if (labels.exists(_ == label)) {
          (labeled(labels, a), Option(label)) :: attr
        } else {
          (labeled(labels, a), None) :: attr
        }
    }

    tr.cata(attributePara(visit))
      .toPair._1

  }

  def labeledSlices(tr: TextReflow, label: Label): Seq[TextReflow] = {
    import scalaz.std.list._

    def visit(
      t: TextReflowF[(TextReflow, List[TextReflow])]
    ): List[TextReflow] = orBubblePara[List[TextReflow]](t) {
      case l @ Labeled (labels, (a, attr))
          if (labels.exists(_ == label)) =>
        labeled(labels, a) :: attr
    }

    tr.cata(attributePara(visit))
      .toPair._1
  }

  // def extractVisualLineTargetRegions(tr: TextReflow): Seq[TargetRegion] = for {
  //   vline <- labeledSlices(tr, LB.VisualLine)
  //   tr    <- extractVisualLineTargetRegion(vline)
  // } yield tr
  // def extractVisualLineTargetRegion(vline: TextReflow): Option[TargetRegion] = {
  //   vline.unFix match {
  //     case Labeled(labels, _) if labels.contains(LB.VisualLine) =>
  //       labels
  //         .filter(_ == LB.VisualLine)
  //         .head.value
  //         .map(TargetRegion.fromUri(_))

  //     case _ => None
  //   }
  // }
}
