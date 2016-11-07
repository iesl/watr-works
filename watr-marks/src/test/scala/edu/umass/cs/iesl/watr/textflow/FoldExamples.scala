package edu.umass.cs.iesl.watr
package textflow

import org.scalatest._

class ExprExamples extends FlatSpec with Matchers {
  import scalaz._
  // import Scalaz.{none}
  import scalaz._, Scalaz.{fix => _, _}

  import textboxing.{TextBoxing => TB}
  // import TB._
  import utils.ScalazTreeImplicits._

  import matryoshka._
  import matryoshka.data._
  import Recursive.ops._
  // import TraverseT.nonInheritedOps._
  // import Corecursive.ops._
  // import FunctorT.ops._
  // import ShowT.ops._
  // import TraverseT.ops._
  // import FunctorT.ops._

  // abstract class FuncRunner[F[_], G[_]] {
  //   def run[T[_[_]]: FunctorT: Corecursive](implicit Eq: Equal[T[G]], S: Show[T[G]]):
  //       T[F] => MatchResult[T[G]]
  // }


  def prettyPrintTree[T[_[_]]: Recursive, F[_]: Traverse, A](exp: T[F])(implicit
    ShowU: Show[F[Unit]],
    ShowT: Show[T[F]],
    ShowF: Show[F[A]],
    ShowA: Show[A]
  ): TB.Box = {
    // exp.transCata(toTree)
    // exp.map(_.toString)
    exp.cata(toTree).draw
  }

  def ppfix[F[_]: Traverse, A](exp: Fix[F])(
    implicit ShowA: Show[F[Unit]]
  ): TB.Box = {
    // exp.transCata(toTree)
    exp.cata(toTree).draw
  }

  // def ppfp: Fix[Exp] => TB.Box = { exp =>
  //   exp.cata(toTree).draw
  // }

  def printlnx(s: String): Unit = {
    val r = s.replaceAll("Fix", "_")
    // println(r)
  }

  // def addOneOptƒ[T[_[_]]]: Exp[Fix[Exp]] => Option[Exp[Fix[Exp]]] = { exp =>
  def addOneOptƒ[T[_[_]]]: Exp[T[Exp]] => Option[Exp[T[Exp]]] = { exp =>
    printlnx(s"+addOneOptƒ: ${exp} ~> ")
    // val before = pp(exp)

    val res:Option[Exp[T[Exp]]]   = exp match {
      case Num(n) => Num(n+1).some
      case _      => none
    }
    printlnx(s"${res}/addOneOptƒ")
    res
  }

  def addOneƒ[T[_[_]]]: Exp[T[Exp]] => Exp[T[Exp]] = { exp =>
    printlnx(s"+addOneƒ: ${exp} => ")

    val res =  addOneOptƒ(exp) getOrElse {
      exp
    }

    printlnx(s"${res}/addOneƒ")
    res
  }

  def addOneExpExp2ƒ[T[_[_]]]: Exp[T[Exp2]] => Exp2[T[Exp2]] = { exp =>
    printlnx(s"+addOneExpExp2ƒ: ${exp} => ")

    val res: Exp2[T[Exp2]] = addOneOptExpExp2ƒ(exp).getOrElse {
      Exp2.Const()
    }

    printlnx(s"${res}/addOneExpExp2ƒ")
    res
  }


  def addOneOptExpExp2ƒ[T[_[_]]]: Exp[T[Exp2]] => Option[Exp2[T[Exp2]]] = { exp =>
    printlnx(s"+addOneOptExpExp2ƒ: ${exp} => ")

    val res: Option[Exp2[T[Exp2]]] = exp match {
      case Num(n) => Exp2.Num2(n+1).some
      case _      => none
    }

    printlnx(s"${res}/addOneOptExpExp2ƒ")
    res
  }

  val MinusThree: Exp ~> Exp =
    new (Exp ~> Exp) {
      def apply[A](exp: Exp[A]): Exp[A] = {
        printlnx(s"MinusThree: ${exp} ~> ")
        val res = exp match {
          case Num(x) => Num(x-3)
          case t      => t
        }
        printlnx(s"${res}/MinusThree")
        res
      }
    }


  import Exp._
  it should "apply ~> in original space" in {
    println("apply ~> in original space")
    // val example = mul(num(1), mul(num(12), num(8)))
    val example =
      mul(
        mul(
          mul(
            mul(num(0), num(0)),
            num(0)
          ),
          num(0)
        ),
        num(0)
      )
    println("input")
    // println(ppfp(example))
    println(prettyPrintTree(example))

    // val res = example.transPrepro(MinusThree, addOneƒ)

    // println("result")
    // println(formatFixType[Exp, Fix](res))

    // res shouldEqual {
    //   mul(num(-1), mul(num(7), num(3)))
    // }
  }

  // it should "apply ~> in changed space" in {
  //   printlnx("apply ~> in changed space")

  //   val example = num(2)//mul(num(1), mul(num(12), num(8)))
  //   printlnx("input")
  //   ppfp(example)

  //   val res = example.transPrepro(MinusThree, addOneExpExp2ƒ)
  //   printlnx("result")
  //   printlnx(res.toString)
  //   // ppfp(res)

  //   // example.transPostpro(MinusThree, addOneExp2Expƒ ) shouldEqual {
  //   //   mul(num(-1), mul(num(7), num(3)))
  //   // }
  // }
}
