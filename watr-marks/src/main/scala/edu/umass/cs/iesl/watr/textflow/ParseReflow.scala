package edu.umass.cs.iesl.watr
package textflow

import watrmarks._

import fastparse._
import fastparse.utils._
import fastparse.core.{ParserApi, ParserApiImpl}
import fastparse.parsers._
import Terminals._

import GeneralizedReflow._
import Reflow._

// object Helper {

//   implicit def atomBitSetHelper[C](implicit cHelper: ElemSetHelper[C]) = new ElemSetHelper[Atom[C]] {
//     def toInt(a: Atom[C]): Int = cHelper.toInt
//     def ordering = implicitly[Ordering[Atom]]
//     def toLowerCase(in: Atom) = in.toLower
//     val allValues = Atom.MinValue to Atom.MaxValue
//   }
//   // implicit object AtomBitSetHelper extends ElemSetHelper[Atom] {
//   //   def toInt(a: Atom): Int = a
//   //   def ordering = implicitly[Ordering[Atom]]
//   //   def toLowerCase(in: Atom) = in.toLower
//   //   val allValues = Atom.MinValue to Atom.MaxValue
//   // }
// }

// import Helper._

// class TextFlowApi[A]() extends Api[Atom[A], Reflow[A]](
//   implicitly, AtomBitSetHelper, StringReprOps, AtomBitSetHelper.ordering
// ) {

//   // val AnyAtom = parsers.Terminals.AnyElem[Atom, String]("AnyAtom")
//   // def AnyAtoms(count: Int) = AnyElems[Atom, String]("AnyAtoms", count)

//   // val AnyElem = AnyAtom
//   // def AnyElem(count: Int) = AnyAtoms(count)
//   // def AtomPred(pred: Atom => Boolean): P0 = Intrinsics.ElemPred("AtomPred", pred)
//   // def AtomIn(strings: Seq[Atom]*) = Intrinsics.ElemIn[Atom, String]("AtomIn", strings.map(_.toIndexedSeq): _*)
//   // def AtomsWhile(pred: Atom => Boolean, min: Int = 1) = Intrinsics.ElemsWhile[Atom, String]("AtomsWhile", pred, min)


//   // def ElemPred(pred: Atom => Boolean) = AtomPred(pred)
//   // def ElemIn(strings: Seq[Atom]*) = AtomIn(strings:_*)
//   // def ElemsWhile(pred: Atom => Boolean, min: Int = 1) = AtomsWhile(pred, min)


//   // def StringIn(strings: String*) = SeqIn(strings: _*)
//   // def StringInIgnoreCase(strings: String*) =
//   //   Intrinsics.StringInIgnoreCase[Atom, String](strings: _*)

//   // val AtomPredicates = fastparse.AtomPredicates
//   // val IgnoreCase = parsers.Terminals.IgnoreCase

//   // implicit def LiteralStr(s: String): P0 =
//   //   if (s.length == 1) parsers.Terminals.ElemLiteral(s(0))
//   //   else parsers.Terminals.Literal(s)
// }

// object flowparser extends TextFlowApi {
//   implicit def parserApi[T, V](p: T)(implicit c: T => core.Parser[V, Atom, String]): ParserApi[V, Atom, String] =
//     new ParserApiImpl[V, Atom, String](p)
// }
// object  extends StringApi
