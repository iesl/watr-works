package edu.umass.cs.iesl.watr
package textreflow

 //import TextReflow._
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

  implicit class RicherTextReflowT(val theReflow: TextReflowT) extends AnyVal  {

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

  def charStarts[A](i: CharOffsetState, t: TextReflowT): State[CharOffsetState, CharOffsetState] = {
    val chars = countCharsF(t)
    State.modify[CharOffsetState]({
      case r => CharOffsetState(r.cbegin+r.clen, chars)
    }) *> State.get[CharOffsetState]
  }


  type CharLoc = TreeLoc[CharOffsetState]
  type CharLocState = State[CharLoc, CharLoc]

  def charRangeState[A](
    i: CharLoc, t: TextReflowT
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

    //  :: W[F[A]] => M[A]
    def transChars(begin: Int, len: Int)(
      fn: (Char, Int) => Option[String]
    ): ElgotAlgebraM[(CharOffsetState, ?), Option, TextReflowF, TextReflow] = {
      case (charOffs, a@ Atom(c, ops))
          if begin <= charOffs.cbegin &&  charOffs.cbegin < begin+len =>

        for {
          ch  <- ops.chars.headOption
          mod <- fn(ch, begin+len)
                    .map(rewrite(fixf(a), _))
                    .orElse(Option(fixf(a)))
        } yield mod

      case (_,      f)                 => Some(fixf(f))
    }

    def modifyCharAt(i: Int)(fn: (Char, Int) => Option[String]): TextReflow = {
      modifyChars(i, 1)(fn)
    }

    def modifyChars(begin: Int, len: Int)(fn: (Char, Int) => Option[String]): TextReflow = {

      val trans = liftTM(
        attributeElgotM[(CharOffsetState, ?), Option](transChars(begin, len)(fn))
      )

      val res = theReflow.annotateCharRanges.cataM(trans)
      res.get.head
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
