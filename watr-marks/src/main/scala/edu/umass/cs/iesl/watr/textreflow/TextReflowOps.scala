package edu.umass.cs.iesl.watr
package textreflow

import scalaz._, Scalaz._

import spindex._
import watrmarks._
import textboxing.{TextBoxing => TB}

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import matryoshka.patterns.EnvT

import utils.EnrichNumerics._
// import EnrichGeometricFigures._

case class Offsets(begin: Int, len: Int, total: Int, pad: Int)

trait TextReflowFunctions extends StructuredRecursion {
  import TextReflowF._
  import utils.SlicingAndDicing._

  def fixf = Fix[TextReflowF](_)

  def atom[AtomT](c: AtomT, ops:TextReflowAtomOps) = fixf(Atom(c, ops))
  def atomStr[AtomT](c: AtomT, ops:TextReflowAtomOps) = fixf(Atom(c, ops))
  def rewrite(t: TextReflow, s: String) = fixf(Rewrite(t, s))
  def flow(as:TextReflow*) = flows(as)
  def flows(as: Seq[TextReflow]) = fixf(Flow(as.toList))
  def cache(a: TextReflow, text: String) = fixf(CachedText(a, text))

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

  implicit class RicherReflow(val theReflow: TextReflow) extends TextReflowJsonFormats {
    import play.api.libs.json._
    import TextReflowRendering._

    def toJson(): JsValue = {
      textReflowToJson(theReflow)
    }

    def cacheText(): String = {
      val res = theReflow.cata(attributePara(renderText))
      res.toPair._1
    }

    def toText(): String = {
      val res = theReflow.cata(attributePara(renderText))
      res.toPair._1
    }

    def toFormattedText(): String = {
      val res = theReflow.transCata(escapeLineFormatting)
      res.toText
    }

    def countChars: GAlgebra[(TextReflow, ?), TextReflowF, Int] = _ match {
      case Atom(c, ops)                   =>  ops.toString.length
      case Insert(value)                  =>  value.length
      case Rewrite ((from, attr), to)     =>  to.length
      case Bracket (pre, post, (a, attr)) =>  pre.length + post.length + attr
      case Mask    (mL, mR, (a, attr))    =>  attr - mL - mR
      case Flow(atomsAndattrs)            =>  atomsAndattrs.map(_._2).sum
      case Labeled(labels, (a, attr))     =>  attr
      case CachedText((a, attr), text)    =>  attr
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
        case CachedText((a, attr), text)    => Offsets(0, attr.len, 0, 0)
      }
    }


    // (A, FT) => M[A] applied top-down
    def attrBegins(offs: Offsets, ft:TextReflowF[_]): State[Offsets, Offsets] = {

      def modS  = State.modify[Offsets] _

      def adjustOverwrite(st: Offsets) = offs.copy(
        begin = st.pad,
        total = st.total,
        pad   = st.pad+offs.len
      )


      def adjust() = modS(st =>
        if (st.pad < 0) adjustOverwrite(st)
        else offs.copy(
          begin = st.total,
          total = st.total+offs.len
        ))

      def adjustFlow() = modS(st =>
        if (st.pad < 0) adjustOverwrite(st)
        else offs.copy(
          begin = st.total,
          total = st.total
        ))

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
          case CachedText(a, text)   => adjustFlow()
        }
        sfin <- State.get[Offsets]

      } yield {
        sfin
      }
    }


    def annotateCharRanges(): Cofree[TextReflowF, Offsets] = {
      // bottom-up, fully annotate w/(0, ch-len)
      val charCountAttr:Cofree[TextReflowF, Offsets] =
        theReflow.cata(attributePara(aggregateLengths))

      // Top down adjustment of attributes:
      val adjustBegins = charCountAttr
        .attributeTopDownM[State[Offsets, ?], Offsets](OffsetsInst.zero)({
          case e => attrBegins(e._2.ask, e._2.lower)
        })

      val asCofree:Cofree[TextReflowF, Offsets] = adjustBegins
        .eval(OffsetsInst.zero)
        .mapBranching(stripEnv)

      asCofree
    }


    def modifyCharAt(i: Int)(fn: (Char, Int) => Option[String]): TextReflow = {
      modifyChars(i, 1)(fn)
    }

    def modifyChars(begin: Int, len: Int)(fn: (Char, Int) => Option[String]): TextReflow = {
      //  :: W[F[A]] => M[A]
      def transChars: ElgotAlgebraM[(Offsets, ?), Option, TextReflowF, TextReflow] = {
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

      val trans = liftTM(
        attributeElgotM[(Offsets, ?), Option](transChars)
      )

      val res = theReflow.annotateCharRanges.cataM(trans)
      res.get.head
    }

    def charCount: Int = {
      theReflow.para(countChars)
    }

    def trimFlow[A](b: (A) => Boolean)(fa: TextReflowF[A]): TextReflowF[A] = {
      fa match {
        case tr@ Flow(as) if as.exists(b) => Flow(as filter b)
        case _ => fa
      }
    }


    def slice(begin: Int, until:Int): Option[TextReflow] = {
      val sliceRange = RangeInt(begin, until-begin)

      def retain(wfa: EnvT[Offsets, TextReflowF, Option[(TextReflow, String)]]): Option[(TextReflow, String)] = {
        val Offsets(cbegin, clen, _, _) = wfa.ask

        val fa : TextReflowF[Option[(TextReflow, String)]] = wfa.lower

        // filter out None values for Flows, only traverse to None iff !exists(_.isDefined)
        val fa2 = trimFlow[Option[(TextReflow, String)]](_.isDefined)(fa)

        val t1: Option[TextReflowF[(TextReflow, String)]] = fa2.sequence

        // val t2: Option[(List[TextReflow], TextReflowF[String])] =
        //   t1.map({ t10 : TextReflowF[(TextReflow, String)] =>
        //     t10.traverse(p => (List(p._1), p._2))
        //   })

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
              case Atom(c, ops)                  => atom(c, new TextReflowAtomOps(ops.toString))
              case Insert(value)                 => insert(ctext)
              case Rewrite((from, attr), to)     => rewrite(from, ctext)
              case Bracket(pre, post, (a, attr)) =>
                val pre1 = clip(pre)
                val post1 = ctext.substring(pre.length+attr.length, ctext.length)
                bracket(pre1, post1, a)

              case Mask(mL, mR, (a, attr))       => ??? // Mask(mL, mR, a.get)
              case Flow(atomsAndattrs)           => flows(atomsAndattrs.map(_._1))
              case Labeled(labels, (a, attr))    => labeled(labels, a)
              case CachedText((a, attr), text)   => cache(a, attr)
            }
            (tf, ctext)
          }

        }
      }

      theReflow
        .annotateCharRanges
        .cata(retain)
        .map(_._1)

    }

    def lines: Seq[TextReflow] = ???


    def targetRegions(): Seq[TargetRegion] = {
      def regions(t: TextReflowF[Seq[TargetRegion]]): Seq[TargetRegion] = t match {
        case Atom    (ac, ops)               => Seq(ac.asInstanceOf[CharAtom].targetRegion)
        case Insert  (value)                 => Seq()
        case Rewrite (attr, to)             => attr
        case Bracket (pre, post, attr)  => attr
        case Mask    (mL, mR, attr)     => attr
        case Flow    (asAndattrs)            => asAndattrs.flatten
        case Labeled (labels, attr)     => attr
        case CachedText(attr, text)     => attr
      }

      theReflow.cata(regions)
    }

    def intersect(other: TextReflow): TextReflow = ???

    def intersectPage(other: PageIndex): Seq[Component] = {
      ???
    }

    import ComponentTypeEnrichments._

    // this is a tricky function, but I'll start with a few assumptions that will work in the short-term:
    //   - The targetRegion does not divide the given text reflow into non-contiguous spans of text
    //   - This means I can just take the first span of text that intersects the target region
    def clipToTargetRegion(targetRegion: TargetRegion): Option[(TextReflow, RangeInt)] = {

      // def bubbleUpAttrs: GAlgebra[(TextReflow, ?), TextReflowF, Int] = _ match {

      def retain(wfa: EnvT[Offsets, TextReflowF, Option[(TextReflow, RangeInt)]]): Option[(TextReflow, RangeInt)] = {
        val Offsets(cbegin, clen, _, _) = wfa.ask
        val range = RangeInt(cbegin, clen)

        val fa: TextReflowF[Option[(TextReflow, RangeInt)]] =
          trimFlow[Option[(TextReflow, RangeInt)]](_.isDefined)(wfa.lower)

        val t1: Option[TextReflowF[(TextReflow, RangeInt)]] = fa.sequence

        val tz: Option[(TextReflow, RangeInt)] = t1.map(trF => (fixf(trF.map(_._1)), range) )

        // println(s"retain on ${tz} for targetRegion=${targetRegion.bbox.prettyPrint}")
        if (cbegin < 0) tz else {
          val r = RangeInt(cbegin, clen)

          for {
            (textReflow, range) <- tz
            textReflowF        <- t1
            // irange              <- sliceRange.intersect(r)
            m <- textReflowF match {
              case Atom(c, ops) =>
                val pageAtom = c.asInstanceOf[PageAtom]
                val pageAtomTargetRegion = pageAtom.targetRegion
                val intersects = pageAtomTargetRegion.intersects(targetRegion)

                // println(s"Atom:  ${pageAtomTargetRegion}.intersects(${targetRegion})=$intersects")
                // println(s"Atom: intersects=$intersects")

                if (pageAtom.targetRegion.intersects(targetRegion)) Some(
                  (atom(c, new TextReflowAtomOps(ops.toString)), range)
                ) else None

              case Insert(value)                 => None // insert(value)
              case Rewrite((from, attr), to)     => Some((rewrite(from, to), attr))
              case Bracket(pre, post, (a, attr)) => Some((bracket(pre, post, a), attr))
              case Mask(mL, mR, (a, attr))       => ??? // Mask(mL, mR, a.get)
              case Flow(atomsAndattrs)           =>
                val totalRange: RangeInt = atomsAndattrs.map(_._2).reduce(_.union(_))
                Some((flows(atomsAndattrs.map(_._1)), totalRange))
              case Labeled(labels, (a, attr))    => Some((labeled(labels, a), attr))
              case CachedText((a, attr), text)   => ??? // cache(a, attr)
            }
          } yield {
            // println(s"yield : ${m}")
            m
          }

        }
      }

      theReflow
        .annotateCharRanges
        .cata(retain)
    }


  }
}
