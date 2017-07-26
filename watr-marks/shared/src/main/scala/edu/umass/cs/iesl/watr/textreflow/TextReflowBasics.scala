package edu.umass.cs.iesl.watr
package textreflow

import scalaz._, Scalaz._

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import matryoshka.patterns._
import matryoshka.patterns.EnvT
// import matryoshka.patterns.EnvT._
// import Birecursive.nonInheritedOps._

import utils.EnrichNumerics._
import scala.{Range => _}
import utils.ScalazTreeImplicits._
import geometry._
import watrmarks._

import textboxing.{TextBoxing => TB}
import watrmarks.{StandardLabels => LB}

case class Offsets(begin: Int, len: Int, total: Int, pad: Int)

trait TextReflowBasics {
  import TextReflowF._


  // Natural Transformation  from EnvT ~> TextReflow
  def stripEnv = new (EnvT[Offsets, TextReflowF, ?] ~> TextReflowF[?]) {
    def apply[A](env: EnvT[Offsets, TextReflowF, A]): TextReflowF[A] = {
      env.lower
    }
  }

  def fixf = Fix[TextReflowF](_)

  def atom(c: CharAtom) = fixf(Atom(c))
  def rewrite(t: TextReflow, s: String) = fixf(Rewrite(t, s))
  def flow(as:TextReflow*) = flows(as)
  def flows(as: Seq[TextReflow]) = fixf(Flow(as.toList))
  // def cache(a: TextReflow, text: String) = fixf(CachedText(a, text))

  def bracket(pre: Char, post: Char, a:TextReflow) = fixf(
    Bracket(pre.toString, post.toString, a)
  )
  def bracket(pre: String, post: String, a:TextReflow) = fixf(
    Bracket(pre, post, a)
  )

  def labeled(l: Label, a:TextReflow) = fixf(Labeled(Set(l), a))
  def labeled(ls: Set[Label], a:TextReflow) = fixf(Labeled(ls, a))
  def insert(s: String) = fixf(Insert(s))
  def space() = insert(" ")


  def bubbleUpAttrs[A: Monoid]: PartialFunction[TextReflowF[(TextReflow, A)], A] = {
    val A: Monoid[A] = implicitly[Monoid[A]]

    _ match {
      case Atom(c)                        => A.zero
      case Insert(value)                  => A.zero
      case Rewrite ((a, attr), to)        => attr
      case Bracket (pre, post, (a, attr)) => attr
      case Labeled(labels, (a, attr))     => attr
      case Flow(atomsAndattrs)            =>
        atomsAndattrs.map(_._2)
          .foldLeft(A.zero){ A.append(_, _) }
    }
  }

  def orBubblePara[A: Monoid](
    v: TextReflowF[(TextReflow, A)]
  )(
    pf: PartialFunction[TextReflowF[(TextReflow, A)], A]
  ): A = {
    val cf: PartialFunction[TextReflowF[(TextReflow, A)], A] =
      pf.orElse(bubbleUpAttrs)

    cf.apply(v)
  }

  def orBubbleAttr[A: Monoid](v: TextReflowF[A])(
    ffa: PartialFunction[TextReflowF[A], A]
  ): A = {
    val A: Monoid[A] = implicitly[Monoid[A]]
    val cf: PartialFunction[TextReflowF[A], A] =
      ffa.orElse(_ match {
        case Atom    (ac)              => A.zero
        case Insert  (value)           => A.zero
        case Rewrite (attr, to)        => attr
        case Bracket (pre, post, attr) => attr
        case Labeled (labels, attr)    => attr
        case Flow    (attrs)           => attrs.foldLeft(A.zero)(A.append(_, _))
      })

    cf.apply(v)
  }
  def countChars: GAlgebra[(TextReflow, ?), TextReflowF, Int] = _ match {
    case Atom(c)                        =>  c.char.length
    case Insert(value)                  =>  value.length
    case Rewrite ((from, attr), to)     =>  to.length
    case Bracket (pre, post, (a, attr)) =>  pre.length + post.length + attr
    case Flow(atomsAndattrs)            =>  atomsAndattrs.map(_._2).sum
    case Labeled(labels, (a, attr))     =>  attr
  }

  // Bottom-up initial evaluator for char-begin/len offsets
  def aggregateLengths: GAlgebra[(TextReflow, ?), TextReflowF, Offsets] = fwa => {
    fwa match {
      //                                     Offsets(begin, len,                               total,                     pad)
      case Atom(c)                        => Offsets(0,     c.char.length,                     0,                         0)
      case Insert(value)                  => Offsets(0,     value.length,                      0,                         0)
      case Rewrite ((fromA, attr), to)    => Offsets(0,     to.length,                         0,                         -attr.len)
      case Bracket (pre, post, (a, attr)) => Offsets(0,     pre.length,                        attr.len,                  post.length)
      case Flow(atomsAndattrs)            => Offsets(0,     atomsAndattrs.map(_._2.len).sum,   0,                         0)
      case Labeled(labels, (a, attr))     => Offsets(0,     attr.len,                          0,                         0)
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
        case Flow(atoms)           => setTotalLen()
        case Labeled(ls, a)        => setTotalLen()
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
      .attributeTopDownM[
        State[Offsets, ?],
        Cofree[EnvT[Offsets, TextReflowF, ?], Offsets],
        Offsets
      ](OffsetsInst.zero)({
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

  def hasLabel(l: Label): TextReflowT => Boolean = _ match {
    case Labeled(labels, _) if labels.contains(l) => true
    case _ => false
  }


  def trimFlow[A](b: (A) => Boolean)(fa: TextReflowF[A]): TextReflowF[A] = {
    fa match {
      case tr@ Flow(as) if as.exists(b) => Flow(as filter b)
      case _ => fa
    }
  }

  //  if (l.hasLabel(LB.Sup)) { Bracket("^{", "}",  a) }
  def escapeLineFormatting: TextReflowT => TextReflowT = {
    case l @ Labeled (labels, a)     =>
      val esc = if (hasLabel(LB.Sup)(l)) Option("^")
                else if (hasLabel(LB.Sub)(l))      Option("_")
                else None

      esc.map(e =>
        flow(insert(s"$e{"), a, insert("}")).unFix
      ) getOrElse (l)

    case t      => t
  }

  def renderText(t: TextReflowF[(TextReflow, String)]): String = t match {
    case Atom    (ac)                    => ac.char
    case Insert  (value)                 => value
    case Rewrite ((from, attr), to)      => to
    case Bracket (pre, post, (a, attr))  => s"$pre${attr}$post"
    case Flow    (atomsAndattrs)         => atomsAndattrs.map(_._2).mkString
    case Labeled (labels, (a, attr))     => attr
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

      if (cbegin < 0) { tz } else {
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

            case Flow(atomsAndattrs)           => flows(atomsAndattrs.map(_._1))
            case Labeled(labels, (a, attr))    => labeled(labels, a)
          }
          (tf, ctext)
        }

      }
    }

    annotateReflowCharRanges(textReflow)
      .cata(retain)
      .map(_._1)

  }

  import utils.ScalazTreeImplicits._

  def boxTF[T, F[_]: Foldable: Functor](
    tf: T
  )(implicit
    TR: Recursive.Aux[T, F],
    FShow: Delay[Show, F]
  ): TB.Box = {
    tf.cata(toTree).drawBox
  }

  def prettyPrintTree(reflow: TextReflow): TB.Box = {
    reflow.cata(toTree).drawBox
  }

  def prettyPrintCofree[B](cof: Cofree[TextReflowF, B])(implicit
    BS: Show[B],
    CS: Delay[Show, Cofree[TextReflowF, ?]]
  ): String = {
    CS(BS).shows(cof)
  }

  def cofreeBox[B](cof: Cofree[TextReflowF, B])(implicit
    BS: Show[B]
  ): TB.Box = {
    cofreeAttrToTree(cof).drawBox
  }

  def cofreeAttrToTree[A](c: Cofree[TextReflowF, A]): Tree[A] = {
    // val cname = c.tail.getClass.getSimpleName
    Tree.Node(
      c.head,
      c.tail.toStream.map(cofreeAttrToTree(_))
    )
  }

  implicit object OffsetsInst extends Show[Offsets] with Monoid[Offsets] {
    def zero: Offsets = Offsets(0, 0, 0, 0)
    def append(f1: Offsets, f2: => Offsets): Offsets =
      sys.error("meaningless op")

    override def shows(f: Offsets): String = s"(${f.begin} ${f.len}) ${f.total} +:${f.pad}"
  }


  implicit class RicherTextReflowT(val theReflow: TextReflowF.TextReflowT)  {

    def hasLabel(l: Label): Boolean = theReflow match {
      case Labeled(labels, _) if labels.contains(l) => true
      case _ => false
    }
  }
}
