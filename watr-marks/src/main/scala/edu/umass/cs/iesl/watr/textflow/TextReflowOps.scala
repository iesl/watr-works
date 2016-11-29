package edu.umass.cs.iesl.watr
package textflow

import TextReflow._
import TextReflowF._

import spindex._
import watrmarks._
import utils.Ranges

object TextReflowOps {

  import scalaz._, Scalaz._

  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._

  case class CharOffsetState(cbegin: Int, clen: Int)

  implicit object CharOffsetStateInst extends Show[CharOffsetState] {
    def zero: CharOffsetState = CharOffsetState(0, 0)
    override def shows(f: CharOffsetState): String = s"[${f.cbegin}-${f.clen}]"
  }

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



  type TextReflowCR = TextReflowF[Cofree[TextReflowF, CharOffsetState]]

  def charStarts[A](i: CharOffsetState, t: TextReflowU): State[CharOffsetState, CharOffsetState] = {
    val chars = countCharsF(t)
    State.modify[CharOffsetState]({
      case r => CharOffsetState(r.cbegin+r.clen, chars)
    }) *> State.get[CharOffsetState]
  }


  type CharLoc = TreeLoc[CharOffsetState]
  type CharLocState = State[CharLoc, CharLoc]

  def charRangeState[A](
    i: CharLoc, t: TextReflowU
  ): CharLocState = {
    State.get[CharLoc] <* State.modify[CharLoc]({
      case r => r.right
          .orElse(r.firstChild)
          .getOrElse(sys.error("char range state out of sync"))
    })
  }

  def hideChar: TextReflow => TextReflow = {tr =>
    fixf {
      tr.project match {
        case a @ Atom(c, ops)  => Rewrite(fixf(a), "")
        case f                 => f
      }
    }
  }


  def setRangeLen(rng: Ranges.Ints, l: Int): Ranges.Ints = {
    rng.copy(max=l)
  }

  def rlen(l: Int): Ranges.Ints = {
    Ranges.Ints(min=0, max=l)
  }


  implicit class RicherReflow(val theReflow: TextReflow) extends AnyVal  {

    def annotateCharRanges(): Cofree[TextReflowF, CharOffsetState] = {

      val withStarts = theReflow.attributeTopDownM[State[CharOffsetState, ?], CharOffsetState](
        CharOffsetStateInst.zero)({ case e =>
          charStarts(e._1, e._2)
        }).eval(CharOffsetStateInst.zero)

      withStarts
    }

    // def modifyCharAt(i: Int)(fn: (Char, Int) => ): TextReflow = {

    def modifyCharAt(i: Int)(fn: (Char, Int) => Option[Char]): TextReflow = {
      val cRanges: scalaz.Cofree[TextReflowF, CharOffsetState] = theReflow.annotateCharRanges

      val offsetTree = cofreeToTree(cRanges)
      val allLocs  = offsetTree.loc.cojoin.toTree.levels.flatten
      theReflow

      offsetTree.loc.map({ offsetLoc =>

      })
      // cRanges.head

      // cRanges.transCata()

      // trans.cata(deattribute[TextReflowF, CharOffsetState, TextReflow](f => fixf(f)))
      ???
    }


    def modifyCharAtom(i: Int)(func : TextReflow => TextReflow): TextReflow = {
      // val cRanges: scalaz.Cofree[TextReflowF, CharOffsetState] = theReflow.annotateCharRanges
      // // println("modifyCharAtom")
      // // println(prettyPrintTree(theReflow))
      // // println("pre-mod")
      // // println(printCofree(cRanges))

      // def mod: Cofree[TextReflowF, CharOffsetState] => Cofree[TextReflowF, CharOffsetState] =
      //   tr => {
      //     val ranges = tr.toPair._1
      //     // println(s"mod: @${ranges}")
      //     if (ranges._1 == i) {
      //       val cof = tr.toPair._2
      //       cof match {
      //         case a @ Atom(c, ops)  =>
      //           println(s"hidingChar: Atom: ${a}")
      //           val ca = Cofree[TextReflowF, CharOffsetState](ranges, a)
      //           Cofree[TextReflowF, CharOffsetState](ranges, Rewrite(ca, ""))
      //         case f =>
      //           println(s"(pass) hideChar: pass ${f}")
      //           Cofree[TextReflowF, CharOffsetState](ranges, f)
      //       }
      //     } else {
      //       tr
      //     }
      //   }

      // // val trans = FuncT.transCataT(cRanges)(mod)
      // val trans = cRanges.transCata(mod)
      // println("post-mod")
      // println(printCofree(trans))


      // // RRF.cata(trans)(deattribute[TextReflowF, CharOffsetState, TextReflow](f => fixf(f)))
      // trans.cata(deattribute[TextReflowF, CharOffsetState, TextReflow](f => fixf(f)))
      ???
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
