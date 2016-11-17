package edu.umass.cs.iesl.watr
package textflow


import TextReflow._
import TextReflowF._

import spindex._
import watrmarks._
import utils.Ranges

object TextReflowOps {
  import matryoshka._
  import matryoshka.data._
  import Recursive.ops._

  import scalaz._, Scalaz._


  implicit class RicherReflowU(val theReflow: TextReflowU) extends AnyVal  {

    def hasLabel(l: Label): Boolean = theReflow match {
      case Labeled(labels, _) if labels.contains(l) => true
      case _ => false
    }
  }

  def countChars: TextReflow => Int = tr => {

    val res = tr.project match {
      case Atom(c, ops)               => ops.toString.length
      case Insert(value)              => value.length
      case Rewrite(from, to)          => to.length
      case Bracket(pre, post, a)      => pre.length + post.length
      case Flow(ls, atoms)            => 0
      case Labeled(ls, _)             => 0
      case _ =>
        println(s"""ERR: countChars: ${tr} => ?  """)
        0
    }

    res
  }

  def countCharsF[A]: TextReflowF[A] => Int = tr => {

    val res = tr match {
      case Atom(c, ops)               => ops.toString.length
      case Insert(value)              => value.length
      case Rewrite(from, to)          => to.length
      case Bracket(pre, post, a)      => pre.length + post.length
      case Flow(ls, atoms)            => 0
      case Labeled(ls, _)             => 0
      case _ =>
        println(s"""ERR: countChars: ${tr} => ?  """)
        0
    }

    res
  }

  def charCount(fw: TextReflowF[(TextReflow, Int)]): Int = {
    fw.map(e=>countChars(e._1)).suml
  }


  def countAtoms: GAlgebra[(TextReflow, ?), TextReflowF, Int] = {
    trF => trF.foldRight(
      charCount(trF)
    )({case z => (z._2 + z._1._2) })
  }

  // (cbegin, clen, ctotalLen)
  type RangeAccum = (Int, Int, Int)

  type TextReflowCR = TextReflowF[Cofree[TextReflowF, RangeAccum]]

  def charStarts[A](i: RangeAccum, t: TextReflowCR): State[RangeAccum, RangeAccum] = {
    val chars = countCharsF(t)
    State.modify[RangeAccum]({
      case r => (r._3, chars, r._3+chars)
    }) *> State.get[RangeAccum]
  }

  def hideChar: TextReflow => TextReflow = {tr =>
    fixf {
      tr.project match {
        case a @ Atom(c, ops)  =>
          println(s"hideChar: Atom: ${a}")
          Rewrite(fixf(a), "")
        case f =>
          println(s"hideChar: pass ${f}")
          f
      }
    }
  }


  def setRangeLen(rng: Ranges.Ints, l: Int): Ranges.Ints = {
    rng.copy(max=l)
  }

  def rlen(l: Int): Ranges.Ints = {
    Ranges.Ints(min=0, max=l)
  }

  val evalLengths: ElgotAlgebraM[(Ranges.Ints, ?), Option, TextReflowF, Ranges.Ints] = {
    case (rng, Atom(c, ops))          => setRangeLen(rng, ops.toString.length).some
    case (rng, Insert(value))         => setRangeLen(rng, value.length).some
    case (rng, Rewrite(from, to))     => setRangeLen(rng, to.length).some
    // case (rng, Bracket(pre, post, a)) => setRangeLen(rng, pre.length+post.length()).some
    // case (rng, Flow(ls, atoms))       => setRangeLen(rng, to.length).some
    // case (rng, Labeled(ls, _))        => setRangeLen(rng, to.length).some
    case (rng, _)                     => rng.some
  }

  // def aggregateLengths: GAlgebra[(TextReflow, ?), TextReflowF, Ranges.Ints] = {
  //   case Atom    (ac, ops)               => RangesInts.zero
  //   case Insert  (value)                 => RangesInts.zero
  //   case Rewrite ((from, attr), to)      => RangesInts.zero
  //   case Bracket (pre, post, (a, attr))  => RangesInts.zero
  //   case Flow    (labels, atomsAndattrs) => RangesInts.zero
  //   case Labeled (labels, (a, attr))     => attr
  //   // case _  => RangesInts.zero
  // }

  def initLengths: GAlgebra[(TextReflow, ?), TextReflowF, RangeAccum] = {
    case Atom    (ac, ops)               => (0, ops.toString.length, 0)
    case Insert  (value)                 => (0, value.toString.length, 0)
    case Rewrite ((from, attr), to)      => (0, to.toString.length, 0)
    case Bracket (pre, post, (a, attr))  => (0, pre.length+post.length+attr._2, 0)
    case Flow    (labels, atomsAndattrs) => (0, atomsAndattrs.map(_._2._2).sum, 0)
    case Labeled (labels, (a, attr))     => attr
  }

  implicit class RicherReflow(val theReflow: TextReflow) extends AnyVal  {
    def RF = implicitly[Recursive[Cofree[?[_], RangeAccum]]]
    def CoRF[F[_]] = implicitly[Corecursive[Cofree[?[_], RangeAccum]]]
    def FuncT = implicitly[FunctorT[Cofree[?[_], RangeAccum]]]

    def annotateCharRanges(): Cofree[TextReflowF, RangeAccum] = {
      // annotate as: ((0, len), reflow)
      val init  = theReflow.cata(attributePara(initLengths))

      // top-down state-driven char-starts
      // init.attributeTopDownM[State[RangeAccum, ?], RangeAccum](RangesInts.empty)(charStarts).eval(RangesInts.empty)
      val withStarts = RF.attributeTopDownM[TextReflowF, State[RangeAccum, ?], RangeAccum](
        init, (0, 0, 0)
      )(charStarts(_, _)).eval((0, 0, 0))

      val converted = RF.convertTo[TextReflowF, Fix](withStarts)
      println("converted")
      println(prettyPrintTree(converted))


      withStarts
    }


    def modifyCharAtom[T](i: Int)(func : TextReflow => TextReflow): TextReflow = {
      val cRanges: scalaz.Cofree[TextReflowF, RangeAccum] = theReflow.annotateCharRanges

      def mod: Cofree[TextReflowF, RangeAccum] => Cofree[TextReflowF, RangeAccum] =
        tr => {
          val ranges = tr.toPair._1
          if (ranges._1 == i) {
            val cof = tr.toPair._2
            cof match {
              // case a : Atom[_]  =>
              case a @ Atom(c, ops)  =>
                println(s"hidingChar: Atom: ${a}")
                val ca = Cofree[TextReflowF, RangeAccum](ranges, a)
                Cofree[TextReflowF, RangeAccum](ranges, Rewrite(ca, ""))
              case f =>
                println(s"(pass) hideChar: pass ${f}")
                Cofree[TextReflowF, RangeAccum](ranges, f)
            }
          } else {
            tr
          }
        }

      val trans = FuncT.transCataT(cRanges)(mod)
      val converted = RF.convertTo[TextReflowF, Fix](trans)
      converted

      // // type   AlgebraicTransform[T[_[_]],                  F[_],        G[_]]        = F[T[G]] => G[T[G]]
      // def mod2: AlgebraicTransform[Cofree[?[_], RangeAccum], TextReflowF, TextReflowF] = {
      //   // F[T[G]] => G[T[G]]
      //   ft: TextReflowF[Cofree[TextReflowF, RangeAccum]] =>
      //   ft match {
      //     case a @ Atom(c, ops)  =>
      //       println(s"hideChar: Atom: ${a}")
      //       Rewrite(fixf(a), "")
      //     case f =>
      //       println(s"hideChar: pass ${f}")
      //       f
      //   }
      //   val res: TextReflowF[Cofree[TextReflowF, RangeAccum]] = ???
      //   res
      // }
      // FuncT.transCata(cRanges)(mod2)

      // ???
    }

    def charCount: Int = {
      theReflow.para(countAtoms)
    }

    def annotateOffsets(): Cofree[TextReflowF, Int] = {
      theReflow.cata(attributePara(countAtoms))
    }

    def slice(begin: Int, end:Int): TextReflow = ???

    def targetRegions(): Seq[TargetRegion] = ???

    def intersect(other: TextReflow): TextReflow = ???

    def intersectPage(other: PageIndex): Seq[Component] = {
      ???
    }

    def clipToTargetRegion(targetRegion: TargetRegion): Option[(TextReflow, Int@@Offset, Int@@Length)] = {
      ???
    }

  }

}

// type GAlgebra[W[_], F[_], A]            = F[W[A]]   => A         // GAlgebraM[W, Id, F, A]
// type AlgebraM[M[_], F[_], A]            = F[A]      => M[A]      // GAlgebraM[Id, M, F, A]
// type Algebra[F[_], A]                   = F[A]      => A         // GAlgebra[Id, F, A]
// type GAlgebraM[W[_], M[_], F[_], A]     = F[W[A]]   => M[A]
// type GCoalgebra[N[_], F[_], A]          = A         => F[N[A]]   // GCoalgebraM[N, Id, F, A]
// type CoalgebraM[M[_], F[_], A]          = A         => M[F[A]]   // GCoalgebraM[Id, M, F, A]
// type Coalgebra[F[_], A]                 = A         => F[A]      // GCoalgebra[Id, F, A]
// type ElgotAlgebraM[W[_], M[_], F[_], A] = W[F[A]]   => M[A]
// type ElgotAlgebra[W[_], F[_], A]        = W[F[A]]   => A           // ElgotAlgebraM[W, Id, F, A]



// import matryoshka.data.cofree.cofreeRecursive
// import matryoshka.data.cofree.cofreeCorecursive
// implicitly[Monoid[Ranges.Ints]]

// import matryoshka.FunctorT.recCorecFunctorT

// implicit def cof = cofreeRecursive
// scalaz.Cofree[TextReflowF, Tuple2]
// bottom-up transform of cofree:
// theReflow.ana({ tr: TextReflowF[_] =>
//   ???
// })

// val RF = implicitly[Recursive[Cofree[?[_], Ranges.Ints]]]
// val CRF2 = implicitly[Corecursive[Cofree[?[_], Ranges.Ints]]]
// val CRF = implicitly[Corecursive[Cofree[?[_], (Int, Int)]]]
// val CFT = recCorecFunctorT[Cofree[?[_], Ranges.Ints]]


// // cof: Cofree[TextReflowF, (Int, Int)]
// CFT.transCataT(cRanges)({  cof =>
//   val charN = cof.head._1
//   if (i == charN) {
//     cof match {
//       case ll @ Cofree(h, t) =>
//         t.map({case uio =>
//         })
//     }
//     cof
//   } else {
//     cof
//   }
// })

    // def annotateCharRangesX(): Cofree[TextReflowF, Ranges.Ints] = {
    //   // 0. Init Cofree
    //   val inits = theReflow.attributeTopDownM[State[Ranges.Ints, ?], Ranges.Ints](RangesInts.empty)(charStarts).eval(RangesInts.empty)
    //   // 1. Run initLengths
    //   // val lengths: Cofree[TextReflowF, Ranges.Ints] = RF.cata(inits)(attributePara(initLengths))

    //   // val maybeRes = lengths.cofCataM(attributeElgotM[(Ranges.Ints, ?), Option](evalLengths))
    //   val maybeRes = inits.cofCataM(attributeElgotM[(Ranges.Ints, ?), Option](evalLengths))
    //   val res = maybeRes.getOrElse { sys.error("annotateCharRanges error") }

    //   // RF.attributeTopDown[State[Ranges.Ints, ?], Ranges.Ints](res, RangesInts.empty)(charStarts).eval(RangesInts.empty)
    //   // RF.attributeTopDownM(res, RangesInts.empty)(charStarts2).eval(RangesInts.empty)

    //   // val cRanges = theReflow.attributeTopDownM[State[Ranges.Ints, ?], Ranges.Ints](RangesInts.empty)(charStarts).eval(RangesInts.empty)
    //   // val res0 = cRanges.cofCataM(attributeElgotM[(Ranges.Ints, ?), Option](evalLengths))

    //   // val res2 = RF.cata(res)(attributePara(aggregateLengths))
    //   res
    //   // inits
    // }

    // // def annotateCharRanges2(): Cofree[TextReflowF, Ranges.Ints] = {
    // //   val cRanges = theReflow.attributeTopDownM[State[Ranges.Ints, ?], Ranges.Ints](RangesInts.empty)(ranges).eval(RangesInts.empty)
    // //   val res0 = cRanges.cofCataM(attributeElgotM[(Ranges.Ints, ?), Option](evalLengths))

    // //   val res = res0.getOrElse { sys.error("annotateCharRanges error") }

    // //   val RF = implicitly[Recursive[Cofree[?[_], Ranges.Ints]]]
    // //   val res2 = RF.cata(res)(attributePara(aggregateLengths))

    // //   res2
    // // }
