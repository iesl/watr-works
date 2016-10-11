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
import scala.collection.mutable



sealed trait ParseElem {
  def intVal: Int
}

sealed trait ParseableElem extends ParseElem {
  def textFlow: TextFlowLeaf
}

object ParseElem {
  private [textflow] class Elem(val intVal: Int) extends ParseElem

  case class CharElem(
    c: Char,
    override val textFlow: TextFlowLeaf,
    index: Int = 0
  ) extends ParseableElem {
    def intVal: Int = c.toInt
  }

  case class FmtElem(
    override val textFlow: TextFlowLeaf,
    override val intVal: Int = -1
  ) extends ParseableElem

  def charElems: Seq[ParseElem] =
    (Char.MinValue to Char.MaxValue).map({
      c => new ParseElem.Elem(c.toInt)
    })

  def nonCharElems: Seq[ParseElem] = Seq() // TODO

  lazy val elemUniverse: mutable.ArrayBuffer[ParseElem] = {
    val tmp = mutable.ArrayBuffer[ParseElem]()
    tmp ++= charElems
    tmp ++= nonCharElems

  }

  def textFlowLeafToParseElems(leaf: TextFlowLeaf): Seq[ParseElem] = {
    val reflow = leaf.asInstanceOf[ReflowF]
    val str = asString.shows(reflow)
    str.zipWithIndex
      .map{ case (c, i) => CharElem(c, leaf, i) }

  }


}

sealed trait TextFlowParseInput { self =>
  def parseElems: Array[ParseElem]

  def slice(start: Int, end: Int): TextFlowParseInput = {
    new TextFlowParseInput {
      def parseElems: Array[ParseElem] = self.parseElems.slice(start, end)
    }

  }
  def at(i: Int): ParseElem  = parseElems(i)
  def length: Int = parseElems.length
  def toArray: Array[ParseElem] = parseElems
  def prettyPrint(): String = {
    parseElems.map(_.intVal).mkString(",")
  }

}

object TextFlowParseInput {
  case object EmptyInput extends TextFlowParseInput {
    def reflow: ReflowF = sys.error("empty parse input")
    def parseElems: Array[ParseElem] = Array()
  }

  def apply(reflow: ReflowF): TextFlowParseInput= {
    new TextFlowParseInput {
      def reflow: ReflowF = reflow
      def parseElems: Array[ParseElem] =
        toLeafList(reflow)
          .flatMap(ParseElem.textFlowLeafToParseElems(_))
          .toArray
    }
  }

  def flatten(input: Seq[TextFlowParseInput]): TextFlowParseInput = {
    if (input.isEmpty) EmptyInput else {

      new TextFlowParseInput {
        def parseElems: Array[ParseElem] =
          input.flatMap(_.parseElems).toArray
      }
    }

  }

  def apply(elems: Array[ParseElem]): TextFlowParseInput= {
    new TextFlowParseInput {
      def parseElems: Array[ParseElem] = elems
    }
  }

  // class RootParseInput(
  //   val rs: List[ReflowF]
  // ) extends TextFlowParseInput {
  //   def inputElems: List[ParseElem] = {
  //     rs.map({ reflow =>


  //     })

  //   }

  //   def prettyPrint(): String = {
  //     parseInput.map({
  //       case ParseElem.CharElem(c, _, _) => c.toString()
  //       case _ => ""
  //     }).mkString(",")
  //   }

  //   def slice(start: Int, end: Int): TextFlowParseInput = {
  //     // List[Int]().slice(from: Int, until: Int)
  //     rs.slice(start, end)
  //     // TextFlowParseInput()
  //       ???
  //   }

  //   def at(i: Int): ParseElem = ???
  //   def length: Int = ???
  //   def toArray: Array[ParseElem] = ???

  // }

  // class CompoundParseInput(
  //   inputs: List[TextFlowParseInput]
  // ) extends TextFlowParseInput {
  // }

  // class ChildParseInput(
  //   parentAndOffset: TextFlowParseInput,
  //   offset: Int, length: Int
  // ) extends TextFlowParseInput {
  // }

}


object TextFlowReprOps extends ReprOps[ParseElem, TextFlowParseInput] {
  type Repr = TextFlowParseInput
  type Elem = ParseElem

  def prettyPrint(input: Repr): String = input.prettyPrint()
  def literalize(input: Repr): String = input.prettyPrint()

  def errorMessage(input: ParserInput[Elem, Repr], expected: String, idx: Int): String = {
    val locationCode = {
      // val first = input.slice(idx - 20, idx)
      // val last = input.slice(idx, idx + 20)
      // val emptyString = ""
      // val lastSnippet: String = last.es.toSeq.headOption.getOrElse(emptyString)
      // val firstSnippet: String = first.reverse.lines.toSeq.headOption.getOrElse(emptyString).reverse

      // prettyPrint(firstSnippet) + prettyPrint(lastSnippet) + "\n" + (" " * firstSnippet.length) + "^"
      "TODO"
    }
    val literal = literalize(input.slice(idx, idx + 20))
    s"found $literal, expected $expected at index $idx\n$locationCode"
  }


  def prettyIndex(input: ParserInput[Elem, Repr], index: Int): String = {
    input match {
      case IndexedParserInput(data) => prettyPrint(data)
      case _ => String.valueOf(index)
    }
  }

  def slice(input: Repr, start: Int, end: Int): Repr = input.slice(start, end)
  def apply(input: Repr, i: Int): Elem = input.at(i)
  def length(input: Repr): Int = input.length
  def fromArray(input: Array[Elem]): Repr = TextFlowParseInput(input)
  def fromSeq(input: Seq[Elem]): Repr = TextFlowParseInput(input.toArray)
  def fromSingle(input: Elem): Repr = TextFlowParseInput(Array(input))
  def toArray(input: Repr): Array[Elem] = input.toArray
  def flatten(input: Seq[Repr]): Repr = TextFlowParseInput.flatten(input)

}

object Helpers {
  type Elem = ParseElem

  implicit object ParseElemSetHelper extends ElemSetHelper[Elem] {
    def toInt(a: Elem): Int = a.intVal
    def ordering: Ordering[Elem] = Ordering.by(_.intVal)
    def toLowerCase(in: Elem): Elem = in match {
      case t @ ParseElem.CharElem(c, _, _) => t.copy(c=c.toLower)
      case _ => in
    }

    val allValues: Seq[Elem] = {
      val charRange: Seq[ParseElem] = (Char.MinValue to Char.MaxValue).map({
        c => new ParseElem.Elem(c.toInt)
      }).toSeq

      val nonCharRange: Seq[ParseElem] = Seq() // TODO

      charRange ++ nonCharRange
    }

  }
}

import Helpers._

class TextFlowApi extends Api[ParseElem, TextFlowParseInput](
  implicitly, ParseElemSetHelper, TextFlowReprOps, ParseElemSetHelper.ordering
) {

  val AnyParseElem = parsers.Terminals.AnyElem[ParseElem, TextFlowParseInput]("AnyParseElem")
  def AnyParseElems(count: Int) = AnyElems[ParseElem, TextFlowParseInput]("AnyParseElems", count)

  val AnyElem = AnyParseElem
  def AnyElem(count: Int) = AnyParseElems(count)
  def ParseElemPred(pred: ParseElem => Boolean): P0 = Intrinsics.ElemPred("ParseElemPred", pred)
  def ParseElemIn(strings: Seq[ParseElem]*) = Intrinsics.ElemIn[ParseElem, TextFlowParseInput]("ParseElemIn", strings.map(_.toIndexedSeq): _*)
  def ParseElemsWhile(pred: ParseElem => Boolean, min: Int = 1) = Intrinsics.ElemsWhile[ParseElem, TextFlowParseInput]("ParseElemsWhile", pred, min)

  def ElemPred(pred: ParseElem => Boolean) = ParseElemPred(pred)
  def ElemIn(strings: Seq[ParseElem]*) = ParseElemIn(strings:_*)
  def ElemsWhile(pred: ParseElem => Boolean, min: Int = 1) = ParseElemsWhile(pred, min)


  // TODO these are high-speed versions of common patterns that should be implemented at some point
  // def TextFlowParseInputIn(strings: TextFlowParseInput*) =
  // def TextFlowParseInputInIgnoreCase(strings: TextFlowParseInput*) =
  // val ParseElemPredicates = fastparse.ParseElemPredicates
  val IgnoreCase = parsers.Terminals.IgnoreCase

  def LiteralInput(s: TextFlowParseInput): P0 = parsers.Terminals.Literal(s)
  def LiteralElem(s: ParseElem): P0 = parsers.Terminals.ElemLiteral(s)

  implicit def CharToLiteral(c: Char): P0 = {
    LiteralElem(ParseElem.elemUniverse(c.toInt))
  }

  implicit def StringToLiteral(s: String): P0 = {
    val elems = s.map(CharToElem(_)).toArray
    LiteralInput(TextFlowParseInput(elems))
  }

  def CharToElem(c: Char): ParseElem = {
    ParseElem.elemUniverse(c.toInt)
  }

  // implicit def StringToElems(s: String): Seq[ParseElem] = {
  //   s.map(CharToElem(_))
  // }

  // implicit def LiteralStr(s: String): P0 =
  //   if (s.length == 1) parsers.Terminals.ElemLiteral(s(0))
  //   else parsers.Terminals.Literal(s)

}


object textFlowParser extends TextFlowApi {
  type Repr = TextFlowParseInput
  type Elem = ParseElem

  implicit def parserApi[T, V](p: T)(
    implicit c: T => core.Parser[V, Elem, Repr]
  ): ParserApi[V, Elem, Repr] =
    new ParserApiImpl[V, Elem, Repr](p)



}
