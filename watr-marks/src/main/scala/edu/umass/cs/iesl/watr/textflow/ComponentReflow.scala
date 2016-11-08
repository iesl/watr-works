package edu.umass.cs.iesl.watr
package textflow

import matryoshka._ ,  Recursive.ops._, FunctorT.ops._
import matryoshka.data._

object ComponentReflow {

  import TextReflowF._
  // import TextReflow._
  val TR = TextReflow
  import watrmarks.{StandardLabels => LB}

  import TR._
  val BracketTexFormatting: TextReflowU => TextReflowU = {
    case l @ Labeled (labels, a)     =>
      if (l.hasLabel(LB.Sup))      { Bracket("^{", "}",  a) }
      else if (l.hasLabel(LB.Sub)) { Bracket("_{", "}",  a) }
      else                         { l }

    case t      => t

  }

  def evalFlatText(t: TextReflowF[(TextReflow, String)]): String = t match {
    case Atom    (ac)                    => ac.toString
    case Insert  (value)                 => s"$value"
    case Rewrite ((from, attr), to)      => s"$to"
    case Bracket (pre, post, (a, attr))  => s"$pre${attr}$post"
    case Flow    (labels, atomsAndattrs) => s"""${atomsAndattrs.map(_._2).mkString}"""
    case Labeled (labels, (a, attr))     => s"${attr}"
  }

  def evalFormattedText(t: TextReflowF[(TextReflow, String)]): String = t match {
    case Atom    (ac)                    => ac.toString
    case Insert  (value)                 => s"$value"
    case Rewrite ((from, attr), to)      => s"$to"
    case Bracket (pre, post, (a, attr))  => s"$pre${attr}$post"
    case Flow    (labels, atomsAndattrs) => s"""${atomsAndattrs.map(_._2).mkString}"""
    case Labeled (labels, (a, attr))     => s"${attr}"
  }

  implicit class RicherComponentReflow(val theReflow: TR.TextReflow) extends AnyVal  {

    def toText(): String = {
      val res = theReflow.cata(attributePara(evalFlatText))
      res.toPair._1
    }

    def toFormattedText(): String = {
      val res = theReflow.transCata(BracketTexFormatting)
      res.toText
    }
  }

}
