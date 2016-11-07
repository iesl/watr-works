package edu.umass.cs.iesl.watr
package textflow

import org.scalatest._


class ExprExamples extends FlatSpec with Matchers {

  // import scalaz.{
  //   Scalaz,
  //   Cord,
  //   Functor,
  //   Foldable,
  //   Traverse,
  //   Show,
  //   ~>
  // }
  import scalaz._

  import Scalaz.{
    fix => _,
    _
  }

  import textboxing.{TextBoxing => TB}
  // import TB._
  import utils.ScalazTreeImplicits._

  import matryoshka._
  import Recursive.ops._, FunctorT.ops._, TraverseT.nonInheritedOps._
  import matryoshka.data._

  // import Recursive.ops._
  // import Corecursive.ops._
  // // import TraverseT.nonInheritedOps._
  // import FunctorT.ops._
  // import ShowT.ops._
  // import TraverseT.ops._
  // import FunctorT.ops._

  import Exp._

  // import ShowFA._
  // import ShowF.ops._


  def prettyPrintTree[T[_[_]]: Recursive:Corecursive:ShowT, F[_]: Functor:Foldable](exp: T[F])(
    implicit ShowF: Delay[Show, F]
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

    val res = example.transPostpro(MinusThree, addOneƒ)
    val res2 = example.transPrepro(MinusThree, addOneƒ)

    println("result")
    println(prettyPrintTree(res))
    println("result 2")
    println(prettyPrintTree(res2))

    // res shouldEqual {
    //   mul(num(-1), mul(num(7), num(3)))
    // }
  }

  it should "apply ~> in changed space" in {
    printlnx("apply ~> in changed space")

    val example = num(2)//mul(num(1), mul(num(12), num(8)))
    println("input")
    println(prettyPrintTree(example))

    val res = example.transPrepro(MinusThree, addOneExpExp2ƒ)
    println("result")
    println(prettyPrintTree(res))

    // example.transPostpro(MinusThree, addOneExp2Expƒ ) shouldEqual {
    //   mul(num(-1), mul(num(7), num(3)))
    // }
  }
}
