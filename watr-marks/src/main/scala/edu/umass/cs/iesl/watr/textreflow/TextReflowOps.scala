package edu.umass.cs.iesl.watr
package textreflow

import scalaz._, Scalaz._

import spindex._
import watrmarks._
import utils.Ranges
import textboxing.{TextBoxing => TB}

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._

case class Offsets(begin: Int, len: Int, total: Int, pad: Int)

trait TextReflowFunctions extends StructuredRecursion {
  import TextReflowF._
  import utils.SlicingAndDicing._

  def fixf = Fix[TextReflowF](_)

  def atom[AtomT](c: AtomT, ops:TextReflowAtomOps) = fixf(Atom(c, ops))
  def rewrite(t: TextReflow, s: String) = fixf(Rewrite(t, s))
  def flow(as:TextReflow*) = flows(as)
  def flows(as: Seq[TextReflow]) = fixf(Flow(as.toList))

  def bracket(pre: Char, post: Char, a:TextReflow) = fixf(
    Bracket(pre.toString, post.toString, a)
  )
  def bracket(pre: String, post: String, a:TextReflow) = fixf(
    Bracket(pre, post, a)
  )

  def labeled(l: Label, a:TextReflow) = fixf(Labeled(Set(l), a))
  def insert(s: String) = fixf(Insert(s))
  def space() = insert(" ")

  private def mkPad(s: String): TextReflow = insert(s)

  def addLabel(l: Label): TextReflow => TextReflow = tr => fixf(tr.unFix match {
    case f @ Labeled(ls, s)  => f.copy(labels = ls + l)
    case r                   => labeled(l, fixf(r)).unFix
  })


  def join(sep:String)(bs:TextReflow*): TextReflow =
    joins(sep)(bs.toSeq)

  def joins(sep:String)(bs:Seq[TextReflow]): TextReflow =
    concat(bs.toList intersperse mkPad(sep))

  def concat(bs: Seq[TextReflow]): TextReflow = {
    flows(bs)
  }

  def groupByPairs(reflow: TextReflowT)(
    groupf: (TextReflowT, TextReflowT, Int) => Boolean,
    onGrouped: List[TextReflowT] => List[TextReflowT] = (w => w)
  ): TextReflowT = {
    reflow match {
      case f @ Flow(as) =>
        val grouped = as
          .groupByPairsWithIndex({
            case (a, b, i) => groupf(a.unFix, b.unFix, i)
          })
          .map(g =>Flow(g.toList))
          .toList

        f.copy(as = onGrouped(grouped).map(fixf(_)))

      case x =>
        println(s"unmatched ${x}")
        x

    }
  }

  def hasLabel(l: Label): TextReflowT => Boolean = _ match {
    case Labeled(labels, _) if labels.contains(l) => true
    case _ => false
  }


  def everyLabel(l: Label, r: TextReflow)(f: TextReflow => TextReflow): TextReflow = {
    def ifLabeled(r:TextReflowT): TextReflowT =  {
      if (hasLabel(l)(r)) holes(r) match {
        case Labeled(labels, (a, fWhole)) => fWhole(f(a))
        case _ => r
      } else r
    }

    r.transCata(ifLabeled)
  }

  def everywhere(r: TextReflow)(f: TextReflowT => TextReflowT): TextReflow = {
    r.transCata(f)
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

  implicit object OffsetsInst extends Show[Offsets] {
    def zero: Offsets = Offsets(0, 0, 0, 0)
    override def shows(f: Offsets): String = s"(${f.begin} ${f.len}) ${f.total} +:${f.pad}"
  }

  implicit class RicherTextReflowT(val theReflow: TextReflowT)  {

    def hasLabel(l: Label): Boolean = theReflow match {
      case Labeled(labels, _) if labels.contains(l) => true
      case _ => false
    }
  }

  type TextReflowCR = TextReflowF[Cofree[TextReflowF, Offsets]]

  type CharLoc = TreeLoc[Offsets]
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


  implicit class RicherReflow(val theReflow: TextReflow) extends TextReflowJsonFormats {
    import play.api.libs.json._
    import TextReflowRendering._
    def toJson(): JsValue = {
      textReflowToJson(theReflow)
    }

    def toText(): String = {
      val res = theReflow.cata(attributePara(renderText))
      res.toPair._1
    }

    def toFormattedText(): String = {
      val res = theReflow.transCata(escapeLineFormatting)
      res.toText
    }

    // def charStarts: (i: CharOffsetState, t: TextReflowT) =>  State[CharOffsetState, CharOffsetState] = {
    def charStarts: (Offsets, TextReflowT) =>  State[Offsets, Offsets] = {
      (i, tf) =>
      val charCount = tf.embed.charCount
      State.modify[Offsets]({
        case r => Offsets(r.begin+r.len, charCount, 0, 0)
      }) *> State.get[Offsets]
    }

    // def localCharCount: TextReflowF[_] => Int= _ match {
    //   case Atom(c, ops)                   =>  ops.toString.length
    //   case Insert(value)                  =>  value.length
    //   case Rewrite ((from, attr), to)     =>  to.length
    //   case Bracket (pre, post, (a, attr)) =>  pre.length + post.length + attr
    //   case Mask    (mL, mR, (a, attr))    =>  attr - mL - mR
    //   case Flow(atomsAndattrs)            =>  atomsAndattrs.map(_._2).sum
    //   case Labeled(labels, (a, attr))     =>  attr
    // }
    def countChars: GAlgebra[(TextReflow, ?), TextReflowF, Int] = _ match {
      case Atom(c, ops)                   =>  ops.toString.length
      case Insert(value)                  =>  value.length
      case Rewrite ((from, attr), to)     =>  to.length
      case Bracket (pre, post, (a, attr)) =>  pre.length + post.length + attr
      case Mask    (mL, mR, (a, attr))    =>  attr - mL - mR
      case Flow(atomsAndattrs)            =>  atomsAndattrs.map(_._2).sum
      case Labeled(labels, (a, attr))     =>  attr
    }


    // Bottom-up initial evaluator for char-begin/len offsets
    def aggregateLengths: GAlgebra[(TextReflow, ?), TextReflowF, Offsets] = fwa => {
      fwa match {
        case Atom(c, ops)                   => Offsets(0, ops.toString.length, 0, 0)
        case Insert(value)                  => Offsets(0, value.length, 0, 0)
        case Rewrite ((fromA, attr), to)    => Offsets(0, to.length, 0, -attr.len)
        case Bracket (pre, post, (a, attr)) => Offsets(0, attr.len+pre.length+post.length, pre.length, post.length)
        case Mask    (mL, mR, (a, attr))    => Offsets(0, attr.len-mL-mR, 0, -(mL+mR))
        case Flow(atomsAndattrs)            => Offsets(0, atomsAndattrs.map(_._2.len).sum, 0, 0)
        case Labeled(labels, (a, attr))     => Offsets(0, attr.len, 0, 0)
      }
    }

    // (A, FT) => M[A] applied top-down
    def attrBegins(offs: Offsets, ft:TextReflowF[_]): State[Offsets, Offsets] = {

      def modS  = State.modify[Offsets] _

      //              (s.pl+s.pr l) s.l  s.l-l_s.r-r
      // def adjust()     = modS(st => offs.copy(begin= st.total,  len= aggLen,  total= st.total+aggLen ))
      // def adjustRW()   = modS(st => offs.copy(begin= st.total,  len= aggLen,  total= st.total+aggLen ))
      // def adjustFlow() = modS(st => offs.copy(begin= st.begin, len= aggLen,  total= st.total        ))

      def adjustFlow() = modS(st =>
        if (st.pad < 0) offs.copy(
          begin = st.pad,
          total = st.total,
          pad   = st.pad+offs.len
        ) else offs.copy(
          begin = st.total,
          total = st.total
        )
      )
      def adjust() = modS(st =>
        if (st.pad < 0) offs.copy(
          begin = st.pad,
          total = st.total,
          pad   = st.pad+offs.len
        ) else offs.copy(
          begin = st.total,
          total = st.total+offs.len
        )
      )
      for {
        sprev <- State.get[Offsets]
        _ <- ft match {
          case Atom(c2, ops2)        => adjust()
          case Insert(value)         => adjust()
          case Rewrite(from, to)     => adjust()
          case Bracket(pre, post, a) => adjust()
          case Mask(mL, mR, a)       => adjust()
          case Flow(atoms)           => adjustFlow()
          case Labeled(ls, a)        => adjustFlow()
        }
        sfin <- State.get[Offsets]
      } yield {

        println(s"@${offs}  $sprev  ->  $sfin")
        sfin
      }
    }


    def annotateCharRanges(): Cofree[TextReflowF, Offsets] = {
      val reflowBox = prettyPrintTree(theReflow)
      // bottom-up, fully annotate w/(0, ch-len)
      val charCountAttr:Cofree[TextReflowF, Offsets] =
        theReflow.cata(attributePara(aggregateLengths))

      // println(cofreeAttrToTree(ranges.map(coff => (coff.begin, coff.len))).drawBox besideS rbox)
      println("charCountAttrs")
      println(cofreeAttrToTree(charCountAttr).drawBox besideS reflowBox)

      // Top down adjustment of attributes:
      val adjustBegins = charCountAttr.attributeTopDownM[State[Offsets, ?], Offsets](
        OffsetsInst.zero
      )({case e => attrBegins(e._2.ask, e._2.lower)})

      val asCofree:Cofree[TextReflowF, Offsets] = adjustBegins
        .eval(OffsetsInst.zero)
        .mapBranching(stripEnv)

      println("adjusted begins")
      println(cofreeAttrToTree(asCofree).drawBox besideS reflowBox)

      // withStarts
      asCofree
    }

    //  :: W[F[A]] => M[A]
    def transChars(begin: Int, len: Int)(
      fn: (Char, Int) => Option[String]
    ): ElgotAlgebraM[(Offsets, ?), Option, TextReflowF, TextReflow] = {
      case (charOffs, a@ Atom(c, ops))
          if begin <= charOffs.begin &&  charOffs.begin < begin+len =>

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
        attributeElgotM[(Offsets, ?), Option](transChars(begin, len)(fn))
      )

      val res = theReflow.annotateCharRanges.cataM(trans)
      res.get.head
    }

    def charCount: Int = {
      theReflow.para(countChars)
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

// def charCount(fw: TextReflowF[(TextReflow, Int)]): Int = {
//   fw.map(e=>countChars(e._1)).suml
// }

// def countAtoms: GAlgebra[(TextReflow, ?), TextReflowF, Int] =
//   trF => trF.foldRight(charCount(trF))({
//     case z => (z._2 + z._1._2)
//   })

// val charLen = fwa match {
//   case Atom(c, ops)                   =>  ops.toString.length
//   case Insert(value)                  =>  value.length
//   case Rewrite ((from, attr), to)     =>  to.length
//   case Bracket (pre, post, (a, attr)) =>  pre.length + post.length
//   case Mask    (mL, mR, (a, attr))    =>  - (mL+mR)
//   case Flow(atomsAndattrs)            =>  0
//   case Labeled(labels, (a, attr))     =>  0
// }

    // def attrBegins: ElgotAlgebraM[(Offsets, ?), State[Offsets, ?], TextReflowF, Offsets] = {
    //   wfa => {
    //     for {
    //       _ <- wfa match {
    //         case (coff , Atom(c2, ops2))           =>
    //           State.modify[Offsets](st =>
    //             Offsets(begin=st.total, len=coff.len, total=st.total+coff.len))

    //         case (coff , Insert(value2))           =>
    //           State.modify[Offsets](st => coff.copy(begin=st.begin + st.len))
    //         case (coff , Rewrite(from2, to2))      =>
    //           State.modify[Offsets](st => coff.copy(begin=st.begin))
    //         case (coff , Bracket(pre2, post2, a2)) =>
    //           State.modify[Offsets](st => coff.copy(begin=st.begin))
    //         case (coff , Mask(maskL2, maskR2, a2)) =>
    //           State.modify[Offsets](st => coff.copy(begin=st.begin))
    //         case (coff , Flow(atoms2))             =>
    //           State.modify[Offsets](st =>
    //             Offsets(begin=st.total, len=coff.len, total=st.total+coff.len))

    //         case (coff , Labeled(ls2, a2))         =>
    //           State.modify[Offsets](st =>
    //             Offsets(begin=st.total, len=coff.len, total=st.total+coff.len))
    //       }
    //       snext <- State.get[Offsets]
    //     } yield {
    //       snext
    //     }
    //     // State.get[CharOffsetState]
    //   }}

    // def attrBegins(sinit: CharOffsetState, t: TextReflowT): State[CharOffsetState, CharOffsetState] = {
    //   State.modify[CharOffsetState]({
    //     case r => CharOffsetState(sinit.begin+r.len, r.len)
    //   }) *> State.get[CharOffsetState]
    // }
