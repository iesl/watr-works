package edu.umass.cs.iesl.watr
package textreflow //;import acyclic.file

import scalaz._, Scalaz._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import matryoshka.patterns.EnvT

import utils.EnrichNumerics._
import scala.{Range => _}
import utils.ScalazTreeImplicits._

trait TextReflowBasics extends StructuredRecursion {
  import TextReflowF._
  import TextReflowRendering._


  def countChars: GAlgebra[(TextReflow, ?), TextReflowF, Int] = _ match {
    case Atom(c)                        =>  c.char.length
    case Insert(value)                  =>  value.length
    case Rewrite ((from, attr), to)     =>  to.length
    case Bracket (pre, post, (a, attr)) =>  pre.length + post.length + attr
    // case Mask    (mL, mR, (a, attr))    =>  attr - mL - mR
    case Flow(atomsAndattrs)            =>  atomsAndattrs.map(_._2).sum
    case Labeled(labels, (a, attr))     =>  attr
    // case CachedText((a, attr), text)    =>  attr
  }

  // Bottom-up initial evaluator for char-begin/len offsets
  def aggregateLengths: GAlgebra[(TextReflow, ?), TextReflowF, Offsets] = fwa => {
    // case Bracket (pre, post, (a, attr)) => Offsets(0,     attr.len+pre.length+post.length,   -(pre.length+post.length), post.length)
    fwa match {
      //                                     Offsets(begin, len,                               total,                     pad)
      case Atom(c)                        => Offsets(0,     c.char.length,                     0,                         0)
      case Insert(value)                  => Offsets(0,     value.length,                      0,                         0)
      case Rewrite ((fromA, attr), to)    => Offsets(0,     to.length,                         0,                         -attr.len)
      case Bracket (pre, post, (a, attr)) => Offsets(0,     pre.length,                        attr.len,                  post.length)
      // case Mask    (mL, mR, (a, attr))    => Offsets(0,     attr.len-mL-mR,                    0,                         -(mL+mR))
      case Flow(atomsAndattrs)            => Offsets(0,     atomsAndattrs.map(_._2.len).sum,   0,                         0)
      case Labeled(labels, (a, attr))     => Offsets(0,     attr.len,                          0,                         0)
      // case CachedText((a, attr), text)    => Offsets(0,     attr.len,                          0,                         0)
    }
  }


  // (A, FT) => M[A] applied top-down
  def adjustOffsets(offs: Offsets, ft:TextReflowF[_]): State[Offsets, Offsets] = {
    def modS  = State.modify[Offsets] _

    // Offsets(begin: Int, len: Int, total: Int, pad: Int)
    def adjustOverwrite(st: Offsets) = offs.copy(
      begin = st.pad,
      total = st.total,
      pad   = st.pad+offs.len
    )

    def incTotalLen() = modS(st =>
      if (st.pad < 0) adjustOverwrite(st)
      else offs.copy(
        begin = st.total,
        total = st.total+offs.len
      ))

    def setTotalLen() = modS(st =>
      if (st.pad < 0) adjustOverwrite(st)
      else offs.copy(
        begin = st.total,
        total = st.total
      ))

    for {
      sprev <- State.get[Offsets]
      _ <- ft match {
        case Atom(c2)              => incTotalLen()
        case Insert(value)         => incTotalLen()
        case Rewrite(from, to)     => incTotalLen()
        case Bracket(pre, post, a) => incTotalLen()
        // case Mask(mL, mR, a)       => incTotalLen()
        case Flow(atoms)           => setTotalLen()
        case Labeled(ls, a)        => setTotalLen()
        // case CachedText(a, text)   => setTotalLen()
      }
      sfin <- State.get[Offsets]

    } yield {
      sfin
    }
  }

  var doDebugPrinting = false

  def annotateReflowCharRanges(textReflow: TextReflow): Cofree[TextReflowF, Offsets] = {
    // bottom-up, fully annotate w/(0, ch-len)
    val charCountAttr:Cofree[TextReflowF, Offsets] =
      textReflow.cata(attributePara(aggregateLengths))

    // Top down adjustment of attributes:
    val adjustBegins = charCountAttr
      .attributeTopDownM[State[Offsets, ?], Offsets](OffsetsInst.zero)({
        case e => adjustOffsets(e._2.ask, e._2.lower)
      })

    val asCofree:Cofree[TextReflowF, Offsets] = adjustBegins
      .eval(OffsetsInst.zero)
      .mapBranching(stripEnv)

    {// Debugging print code
      if (doDebugPrinting) {
        val rbox = prettyPrintTree(textReflow)
        val aggLens = cofreeAttrToTree(charCountAttr.map(coff => (coff.begin, coff.len))).drawBox
        val withOffs = cofreeAttrToTree(asCofree.map(coff => (coff.begin, coff.len))).drawBox
        println(aggLens besideS withOffs besideS rbox)
      }
    }///////

    asCofree
  }



  def trimFlow[A](b: (A) => Boolean)(fa: TextReflowF[A]): TextReflowF[A] = {
    fa match {
      case tr@ Flow(as) if as.exists(b) => Flow(as filter b)
      case _ => fa
    }
  }

  def sliceTextReflow(textReflow: TextReflow, begin: Int, until:Int): Option[TextReflow] = {
    val sliceRange = RangeInt(begin, until-begin)

    def retain(wfa: EnvT[Offsets, TextReflowF, Option[(TextReflow, String)]]): Option[(TextReflow, String)] = {
      val Offsets(cbegin, clen, _, _) = wfa.ask

      val fa : TextReflowF[Option[(TextReflow, String)]] = wfa.lower

      // filter out None values for Flows, only traverse to None iff !exists(_.isDefined)
      val fa2 = trimFlow[Option[(TextReflow, String)]](_.isDefined)(fa)

      val t1: Option[TextReflowF[(TextReflow, String)]] = fa2.sequence


      val t3: Option[String] = t1.map(renderText(_))

      val tz: Option[(TextReflow, String)] = (t1 |@| t3).apply({
        case (tr, str) =>
          (fixf(tr.map(_._1)), str)
      })

      if (cbegin < 0) {
        tz
      } else {
        val r = RangeInt(cbegin, clen)

        for {
          (textReflow, text) <- tz
          textReflowF        <- t1
          irange             <- sliceRange.intersect(r)
        } yield {
          val trange = irange.translate(-r.min)
          // println(s"  keep: ${fa2}, $sliceRange intersect $r = ${irange} => $trange")
          def clip(s: String) = s.slice(trange.min, trange.max)
          val ctext = clip(text)

          val tf = textReflowF match {
            case Atom(c)                  => atom(c)
            case Insert(value)                 => insert(ctext)
            case Rewrite((from, attr), to)     => rewrite(from, ctext)
            case Bracket(pre, post, (a, attr)) =>
              val pre1 = clip(pre)
              val post1 = ctext.substring(pre.length+attr.length, ctext.length)
              bracket(pre1, post1, a)

            // case Mask(mL, mR, (a, attr))       => ??? // Mask(mL, mR, a.get)
            case Flow(atomsAndattrs)           => flows(atomsAndattrs.map(_._1))
            case Labeled(labels, (a, attr))    => labeled(labels, a)
            // case CachedText((a, attr), text)   => cache(a, attr)
          }
          (tf, ctext)
        }

      }
    }

    annotateReflowCharRanges(textReflow)
      .cata(retain)
      .map(_._1)

  }
}
