package edu.umass.cs.iesl.watr
package textflow

import org.scalatest._


class ExprExamples extends FlatSpec with Matchers {

  import scalaz.{Apply => _, _ }
  import Scalaz.{fix => _, _}


  import textboxing.{TextBoxing => TB}
  // import TB._
  import utils.ScalazTreeImplicits._

  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._


  import Exp._

  // (implicit TR: RecursiveT[T], TC: CorecursiveT[T], ShowF: Delay[Show, F]): TB.Box = {
  // def prettyPrintTree[T[_[_]]: RecursiveT :CorecursiveT, F[_]]
  // def prettyPrintTree[T[_[_]], F[_]]
  //   (exp: T[F])
  //   (implicit TR: Recursive.Aux[T, F], TC: Corecursive.Aux[T, F], ShowF: Delay[Show, F]): TB.Box = {
  //   // TR.recursive[T, F].cata(exp)(toTree).draw
  //   exp.cata(toTree).draw
  // }

  // def prettyPrintTree[T[_[_]]: FunctorT, F[_]: Functor: Foldable](exp: T[F])(
  def prettyPrintTree[F[_]: Functor: Foldable](exp: Fix[F])(
    implicit ShowF: Delay[Show, F]
  ): TB.Box = {
    exp.cata(toTree).draw
  }

  def printlnx(s: String): Unit = {
    val r = s.replaceAll("Fix", "_")
    println(r)
  }

  val example1f: Exp[Option[Int]] => Option[Int] = {exp =>
    printlnx(s"+example1f: ${exp} ~> ")
    val res = exp match {
      case Num(v)           => Option(v)
      case Mul(left, right) => (left ⊛ right)(_ * _)
      case Var(v)           => None
      case Lambda(_, b)     => b
      case Apply(func, arg) => None
      case Let(_, _, i)     => i
    }
    printlnx(s"${res}/example1f")
    res
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


  it should "apply ~> in original space" in {
    println("apply ~> in original space")
    val example = mul(num(1), mul(num(12), num(8)))

    // val example =
    //   mul(
    //     mul(
    //       mul(
    //         mul(num(0), num(0)),
    //         num(0)
    //       ),
    //       num(0)
    //     ),
    //     num(0)
    //   )
    println("input")
    println(prettyPrintTree(example))
    // val res2 = example.transPrepro(MinusThree, addOneƒ)

    // println("result: transPrepro")
    // println(prettyPrintTree(res2))

    // val res = example.transPostpro(MinusThree, addOneƒ)

    // println("result: transPostpro")
    // println(prettyPrintTree(res))

    val res3 = example.prepro(MinusThree, example1f)

    println("result: prepro")
    println(res3)

    // res shouldEqual {
    //   mul(num(-1), mul(num(7), num(3)))
    // }
  }

  // it should "apply ~> in changed space" in {
  //   printlnx("apply ~> in changed space")

  //   val example = num(2)//mul(num(1), mul(num(12), num(8)))
  //   println("input")
  //   println(prettyPrintTree(example))

  //   val res = example.transPrepro(MinusThree, addOneExpExp2ƒ)
  //   println("result")
  //   println(prettyPrintTree(res))

  //   // example.transPostpro(MinusThree, addOneExp2Expƒ ) shouldEqual {
  //   //   mul(num(-1), mul(num(7), num(3)))
  //   // }
  // }
}
