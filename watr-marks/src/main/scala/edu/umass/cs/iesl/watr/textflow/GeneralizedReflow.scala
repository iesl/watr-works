package edu.umass.cs.iesl.watr
package textflow

import spindex._
import watrmarks._
import textboxing.{TextBoxing => TB}

import scalaz._, Scalaz.{fix => _, _}

import matryoshka._
import matryoshka.data._
// import matryoshka.data.cofree._

import Recursive.ops._
// import Corecursive.ops._
// import TraverseT.ops._
// import ShowT.ops._
import FunctorT.ops._


class TextReflowAtomOps(
  val chars: Seq[Char]
) {
  override def toString = chars.mkString
}


sealed trait TextReflowF[+A]

object TextReflowF {
  case class Atom[T](c: T, ops: TextReflowAtomOps)        extends TextReflowF[Nothing]
  case class Insert(value: String)                        extends TextReflowF[Nothing]
  case class Rewrite[A](from: A, to: String)              extends TextReflowF[A]
  case class Bracket[A](pre: String, post: String, a: A)  extends TextReflowF[A]
  case class Flow[A](labels: Set[Label], as: List[A])     extends TextReflowF[A]
  case class Labeled[A](labels: Set[Label], a: A)         extends TextReflowF[A]

  implicit val ReflowTraverse: Traverse[TextReflowF] = new Traverse[TextReflowF] {
    def traverseImpl[G[_], A, B](fa: TextReflowF[A])(f: A => G[B])(implicit G: Applicative[G]): G[TextReflowF[B]] = fa match {
      case Atom(c, ops)               => G.point(Atom(c, ops))
      case Insert(value)              => G.point(Insert(value))
      case Rewrite(from, to)          => f(from).map(Rewrite(_, to))
      case Bracket(pre, post, a)      => f(a).map(Bracket(pre, post, _))
      case Flow(labels, atoms)        => atoms.traverse(f).map(Flow(labels, _))
      case Labeled(labels, a)         => f(a).map(Labeled(labels, _))
    }
  }

  implicit val show: Delay[Show, TextReflowF] = new Delay[Show, TextReflowF] {
    def apply[A](show: Show[A]) = Show.show {
      case Atom(c, ops)               => ops.toString
      case Insert(value)              => s"+'$value'"
      case Rewrite(from, to)          => s"-+'${to}"
      case Bracket(pre, post, a)      => s"""${pre}`${a.toString}`{post} """
      case Flow(ls, atoms)            => s"""flow${ls.mkString(":#", " #", "")}"""
      case Labeled(ls, _)             => s"""#${ls.mkString(" #")}"""
    }
  }

}


// TODO rename this to avoid object name/type clashes
object TextReflow {
  import TextReflowF._

  type TextReflow = Fix[TextReflowF]

  type TextReflowU = TextReflowF[Fix[TextReflowF]]

  def fixf = Fix[TextReflowF](_)


  def atom[AtomT](c: AtomT, ops:TextReflowAtomOps) = fixf(Atom(c, ops))

  def flow(as:TextReflow*) = flows(as)
  def flows(as: Seq[TextReflow]) = fixf(Flow(Set(), as.toList))

  def bracket(pre: Char, post: Char, a:TextReflow) = fixf(
    Bracket(pre.toString, post.toString, a)
  )
  def bracket(pre: String, post: String, a:TextReflow) = fixf(
    Bracket(pre, post, a)
  )

  def labeled(l: Label, a:TextReflow) = fixf(Labeled(Set(l), a))
  def insert(s: String) = fixf(Insert(s))
  def space() = insert(" ")



  def addLabel(l: Label): TextReflow => TextReflow = tr => fixf(tr.unFix match {
    case f @ Flow(ls, as)    => f.copy(labels = ls + l)
    case f @ Labeled(ls, s)  => f.copy(labels = ls + l)
    case r                   => labeled(l, fixf(r)).unFix
  })

  import utils.SlicingAndDicing._


  def groupByPairs(reflow: TextReflowU)(
    groupf: (TextReflowU, TextReflowU, Int) => Boolean,
    onGrouped: List[TextReflowU] => List[TextReflowU] = (w => w)
  ): TextReflowU = {
    reflow match {
      case f @ Flow(labels, as) =>
        val grouped = as
          .groupByPairsWithIndex({
            case (a, b, i) => groupf(a.unFix, b.unFix, i)
          })
          .map(g =>Flow(labels, g.toList))
          .toList

        f.copy(as = onGrouped(grouped).map(fixf(_)))

      case x =>
        println(s"unmatched ${x}")
        x

    }
  }


  def hasLabel(l: Label): TextReflowU => Boolean = _ match {
    case Labeled(labels, _) if labels.contains(l) => true
    case _ => false
  }


  def everySequence(r: TextReflow)(f: List[TextReflow] => List[TextReflow]): TextReflow = {
    def atFlows: TextReflowU => TextReflowU = r => r match {
      case fl @ Flow(labels: Set[Label], as: List[TextReflow]) =>
        fl.copy(as = f(as))
      case fl => fl
    }

    r.transCata(atFlows)
  }

  def everyLabel(l: Label, r: TextReflow)(f: TextReflow => TextReflow): TextReflow = {
    def ifLabeled(r:TextReflowU): TextReflowU =  {
      if (hasLabel(l)(r)) holes(r) match {
        case Labeled(labels, (a, fWhole)) => fWhole(f(a))
        case _ => r
      } else r
    }

    r.transCata(ifLabeled)
  }

  def everywhere(r: TextReflow)(f: TextReflowU => TextReflowU): TextReflow = {
    r.transCata(f)
  }

  def prettyPrintTree(reflow: TextReflow): TB.Box = {
    import utils.ScalazTreeImplicits._
    reflow.cata(toTree).draw
  }
  def prettyPrintCofree[B](cof: Cofree[TextReflowF, B])(implicit
    BS: Show[B],
    CS: Delay[Show, Cofree[TextReflowF, ?]]
  ): String = {
    CS(BS).shows(cof)
  }
  // Tree.Node(c.head, c.tail.map(from(_)))
  def cofreeToTree[A](c: Cofree[TextReflowF, A]): Tree[A] = {
    Tree.Node(
      c.head,
      c.tail.toStream.map(cofreeToTree(_))
    )
  }
  def printCofree[B](cof: Cofree[TextReflowF, B])(implicit
    BS: Show[B],
    CS: Delay[Show, Cofree[TextReflowF, ?]]
  ): TB.Box = {
    import utils.ScalazTreeImplicits._
    // CS(BS).shows(cof)
    cofreeToTree(cof).draw
  }

  private def mkPad(s: String): TextReflow = insert(s)

  def join(sep:String)(bs:TextReflow*): TextReflow =
    joins(sep)(bs.toSeq)

  def joins(sep:String)(bs:Seq[TextReflow]): TextReflow =
    concat(bs.toList intersperse mkPad(sep))

  def concat(bs: Seq[TextReflow]): TextReflow = {
    flows(bs)
  }

  implicit class RicherReflowU(val theReflow: TextReflowU) extends AnyVal  {

    def hasLabel(l: Label): Boolean = theReflow match {
      case Labeled(labels, _) if labels.contains(l) => true
      case _ => false
    }
  }

  // def countX[T[_[_]]: Recursive, F[_]: Functor: Foldable](form: T[F]): GAlgebra[(T[F], ?), F, Int] =
  //   e => e.foldRight(if (e ∘ (_._1) == form.project) 1 else 0)(_._2 + _)

  // // Evaluate as usual, but trap 0*0 as a special case
  // def peval[T[_[_]]: Recursive](t: Exp[(T[Exp], Int)]): Int = t match {
  //   case Mul((Embed(Num(0)), _), (Embed(Num(0)), _)) => -1
  //   case Mul((_,             x), (_,             y)) => x * y
  //   case Num(x)                                      => x
  //   case _                                           => Predef.???
  // }

  def countChars: TextReflow => Int = tr => {

    val res =tr.project match {
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
    println(s"""countChars: ${tr} => $res """)

    res
  }

  def charCount(fw: TextReflowF[(TextReflow, Int)]): Int = {
    fw.map(e=>countChars(e._1)).suml
  }

  def charCount2(fw: TextReflowF[(TextReflow, Int)]): Int = {
    fw.map(e=>countChars(e._1)).suml
  }

  def countAtoms: GAlgebra[(TextReflow, ?), TextReflowF, Int] = {
    trF => trF.foldRight({
      val res = charCount(trF)
      println(s"foldRight z @ ${trF}: $res")
      res
    })({case z => (z._2 + z._1._2) })
  }

  def sequential2: (Int, TextReflow) => State[Int, Int] =
    (_, _) => State.get[Int] <* State.modify[Int](_ + 1)

  def starts(i: Int, t: TextReflowU): State[Int, Int] = {
    val chars = countChars(t.embed)
    State.get[Int] <* State.modify[Int](_ + chars)
  }

  type IRange = (Int, Int)

  def ranges(i: (Int, Int), t: TextReflowU): State[IRange, IRange] = {
    val chars = countChars(t.embed)
    println(s"eval ranges(${i}, ${t}) => ${chars}")
    State.get[IRange] <* State.modify[IRange]({
      case (rbegin, rlen) => (rbegin+chars, rlen+1)
    })
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


  implicit class RicherReflow(val theReflow: TextReflow) extends AnyVal  {


    // import matryoshka.data.cofree.cofreeRecursive
    // import matryoshka.data.cofree.cofreeCorecursive
    // implicitly[Monoid[IRange]]
    // implicit val RF = implicitly[Recursive[Cofree[?[_], IRange]]]
    // implicit val CRF = implicitly[Corecursive[Cofree[?[_], IRange]]]

    import matryoshka.FunctorT.recCorecFunctorT

    // implicit def cof = cofreeRecursive
    // scalaz.Cofree[TextReflowF, Tuple2]
    // bottom-up transform of cofree:
    def modifyCharAtom[T](i: Int)(func : TextReflow => TextReflow): TextReflow = {
      val cRanges: scalaz.Cofree[TextReflowF, IRange] = theReflow.annotateCharRanges

      val CFT = recCorecFunctorT[Cofree[?[_], IRange]]


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

      ???
    }

    def charCount: Int = {
      theReflow.para(countAtoms)
    }

    // def transCataT[F[_]: Functor](t: T[F])(f: T[F] => T[F]): T[F] =
    //   f(map(t)(_.map(transCataT(_)(f))))

    // type AlgebraicTransform[T[_[_]], F[_], G[_]]               = F[T[G]] => G[T[G]]

    // def transCata[F[_]: Functor, G[_]: Functor](t: T[F])(f: AlgebraicTransform[T, F, G]): T[G] =
    //   map(t)(ft => f(ft.map(transCata(_)(f))))


    def annotateCharRanges(): Cofree[TextReflowF, IRange] = {
      theReflow.attributeTopDownM[State[IRange, ?], IRange]((0, 0))(ranges).eval((0, 0))
    }

    def annotateCharLengths(): Cofree[TextReflowF, IRange] = {
      theReflow.attributeTopDownM[State[IRange, ?], IRange]((9, 9))(ranges).eval((0, 0))
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

    // def modifyCharAtom[T](i: Int)(func : TextReflow => TextReflow): TextReflow = {
    //   val cRanges: scalaz.Cofree[TextReflowF, IRange] = theReflow.annotateCharRanges

    //   val CFT = recCorecFunctorT[Cofree[?[_], IRange]]

    //   // CFT.transCata(t: Cofree[F, (Int, Int)]) { (_0:  => Cofree[α$5$, (Int, Int)], _1: F) => G }
    //   val scanRes = cRanges.scanr[TextReflow]({ case (r, trc:TextReflowF[Cofree[TextReflowF, TextReflow]]) =>
    //     val range1 = r._1
    //     val range2 = r._2
    //     val scr = if (range1 == i) {
    //       println("(modifying)")
    //       // val qwer = trc.traverse(_.head.project)

    //       val sdf =trc.map({ rf =>
    //         func(rf.head)
    //       })
    //       fixf(sdf)

    //     } else {
    //       val sdf =trc.map({ rf =>
    //         rf.head
    //       })
    //       fixf(sdf)
    //     }
    //     println(s"scanr:  ${r}")
    //     println(prettyPrintTree(scr))

    //     scr
    //   })

    //   scanRes.head
    //   // // cof: Cofree[TextReflowF, (Int, Int)]
    //   // CFT.transCataT(cRanges)({  cof =>
    //   //   val charN = cof.head._1
    //   //   if (i == charN) {
    //   //     cof match {
    //   //       case ll @ Cofree(h, t) =>
    //   //         t.map({case uio =>
    //   //         })
    //   //     }
    //   //     cof
    //   //   } else {
    //   //     cof
    //   //   }
    //   // })

    // }

// def clipToTargetRegion(targetRegion: TargetRegion): Option[(TextReflow, Int@@Offset, Int@@Length)] = {
//   // val clippedFlow = theReflow.flow
//   //   .zipWithIndex
//   //   .dropWhile({case (funit, _) =>
//   //     val intersects = flowUnitTargetRegion(funit).exists(_ intersects targetRegion)
//   //       !intersects
//   //   })
//   //   .reverse
//   //   .dropWhile({case (funit, _) =>
//   //     val intersects = flowUnitTargetRegion(funit).exists(_ intersects targetRegion)
//   //       !intersects
//   //   })
//   //   .reverse

//   // if (clippedFlow.isEmpty) None else {
//   //   val start = clippedFlow.head._2
//   //   val end = clippedFlow.last._2

//   //   Some((TextFlow(clippedFlow.map(_._1)), Offset(start), Length(end-start)))
//   // }
//   ???
// }
