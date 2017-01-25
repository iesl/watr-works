package edu.umass.cs.iesl.watr
package textreflow

import watrmarks._

import scalaz.{Traverse, Applicative, Equal, Show}
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.equal._

import matryoshka._
import matryoshka.data._
import geometry._


sealed trait TextReflowF[+A]

object TextReflowF {

  type TextReflow = Fix[TextReflowF]
  type TextReflowT = TextReflowF[Fix[TextReflowF]]


  case class Atom(c: CharAtom)                            extends TextReflowF[Nothing]
  case class Insert(value: String)                        extends TextReflowF[Nothing]
  case class Rewrite[A](from: A, to: String)              extends TextReflowF[A]
  case class Bracket[A](pre: String, post: String, a: A)  extends TextReflowF[A]  // { val _ = sys.error("disabled"); }
  case class Flow[A](as: List[A])                         extends TextReflowF[A]
  case class Labeled[A](labels: Set[Label], a: A)         extends TextReflowF[A]


  implicit def TextReflowTraverse: Traverse[TextReflowF] = new Traverse[TextReflowF] {
    def traverseImpl[G[_], A, B](fa: TextReflowF[A])(f: A => G[B])(implicit G: Applicative[G]): G[TextReflowF[B]] = fa match {
      case Atom(c)                    => G.point(Atom(c))
      case Insert(value)              => G.point(Insert(value))
      case Rewrite(fromA, to)         => f(fromA).map(Rewrite(_, to))
      case Bracket(pre, post, a)      => f(a).map(Bracket(pre, post, _))
      case Flow(as)                   => as.traverse(f).map(Flow(_))
      case Labeled(labels, a)         => f(a).map(Labeled(labels, _))
    }
  }

  implicit def TextReflowShow: Delay[Show, TextReflowF] = new Delay[Show, TextReflowF] {
    def apply[A](show: Show[A]) = Show.show {
      case Atom(c)                    => c.char
      case Insert(value)              => s"+'$value'"
      case Rewrite(from, to)          => s"/'${to}'"
      case Bracket(pre, post, a)      => s"""${pre}..${post} """
      case Flow(atoms)                => s"""flow"""
      case Labeled(ls, _)             => s"""#${ls.mkString(" #")}"""
    }
  }

  implicit val equal: Delay[Equal, TextReflowF] = new Delay[Equal, TextReflowF] {
    def apply[α](eq: Equal[α]) = Equal.equal((a, b) => { implicit val ieq = eq;
      (a, b) match {
        case (Atom(c)               , Atom(c2))                 => c.char==c2.char
        case (Insert(value)         , Insert(value2))           => value == value2
        case (Rewrite(from, to)     , Rewrite(from2, to2))      => from === from2 && to == to2
        case (Bracket(pre, post, a) , Bracket(pre2, post2, a2)) => pre==pre && post==post && a===a2
        case (Flow(atoms)           , Flow(atoms2))             => atoms === atoms2
        case (Labeled(ls, a)        , Labeled(ls2, a2))         => ls == ls2 && a === a2
        case (_                     , _)                        => false
      }
    })
  }

}
