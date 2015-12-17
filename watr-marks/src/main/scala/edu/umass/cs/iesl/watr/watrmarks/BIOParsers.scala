package edu.umass.cs.iesl.watr
package watrmarks

import java.io.StringReader
import scala.util.parsing.combinator._

trait ParserCommon extends Parsers {
  def read(s: String) = new StringReader(s)

  def toEither[L, R](pr: ParseResult[R]) =
    pr.map(Right(_)).getOrElse(Left(pr.toString))

}


sealed trait RowType

case class PinRow(
  c: Char,
  biostr: String,
  obj: BioObj
) extends RowType

case class RuleRow(
) extends RowType

case class Ruler(
)

case class TextRow(
  text: String
) extends RowType

case class BioBlock(
  pinRows: List[PinRow],
  ruler: Ruler,
  text: Option[TextRow]
)


object bioParsers extends JavaTokenParsers with ParserCommon {

  def parseBioBlock(str: String): Either[String, BioBlock] = {
    toEither(parseAll(block, str))
  }

  override val whiteSpace = "".r

  def continuationChar: Parser[Char] =
    "[a-z ]".r ^^ { _.head }

  def pinChar: Parser[Char] =
    """[a-zA-Z~ $]""".r ^^ { _.head }

  def pinrow: Parser[PinRow] =
    "|" ~> continuationChar ~ "|" ~ rep(pinChar) ~ "|" ~ ".*".r ^^ {
      case (c ~ _ ~ bio ~ _ ~ defs) =>
        val res = biomapParsers.parseAll(biomapParsers.obj, new StringReader(defs))
        PinRow(c, bio.mkString, res.get)
    }

  def rulerRow: Parser[RuleRow] =
    "|[0-9]|".r ~> rep("[0-9 ]".r) <~ "|" ^^ {
      case bio => RuleRow()
    }

  def textRow: Parser[TextRow] =
    "[^>]*>".r ~> ".*<[^<]*$".r ^^ {
      case bio =>
        val lastGT = "<[^<]*$".r
        val cleaned = lastGT.replaceFirstIn(bio, "")
        TextRow(cleaned)
    }

  def emptyRow: Parser[Unit] =
    "^[\\s]*$".r ^^ (_ => (():Unit))

  def row: Parser[RowType] =
    repsep(emptyRow, "\n") ~> (
      pinrow | rulerRow | textRow
    ) <~ repsep(emptyRow, "\n")


  def block: Parser[BioBlock] =
    rep1sep(row, "\n") ^^ {
      case rows =>
        val pins = rows.takeWhile(_.isInstanceOf[PinRow]).map(_.asInstanceOf[PinRow])
        val rules = rows.dropWhile(_.isInstanceOf[PinRow]).takeWhile(_.isInstanceOf[PinRow])
        val text:Option[TextRow] = if(rows.last.isInstanceOf[TextRow]){
          Some(rows.last.asInstanceOf[TextRow])
        } else None

        BioBlock(
          pins,
          Ruler(),//  rules,
          text
        )
    }

}


sealed trait BioVal
case class BioObj(kvs: Map[String, BioVal]=Map()) extends BioVal
case class BioIdent(ident: List[String]=List()) extends BioVal

object biomapParsers extends JavaTokenParsers with ParserCommon {

  def parseBiomapBlock(str: String): Either[String, BioVal] = {
    parse(obj, str)
  }

  def parse(p: Parser[BioVal], str: String): Either[String, BioVal] = {
    val result = parseAll(p, new StringReader(str))
    result.map(Right(_)).getOrElse(Left(result.toString))
  }

  def obj: Parser[BioObj] =
    "{"~> repsep(keyval, ",") <~"}" ^^ (
      kvs => BioObj(Map[String, BioVal](kvs:_*))
    )


  def bioident: Parser[BioIdent] =
    rep1sep(ident, ".") ^^ { BioIdent(_) }

  def keyval: Parser[(String, BioVal)] =
    ident~":"~value ^^ {
      case name~":"~value => (name, value)
    }

  def value: Parser[BioVal] =
    ( obj
    | bioident
    )

}

sealed trait Transform

case class Scale(
  m: Array[Double]
) extends Transform

case class Matrix(
  m: Array[Double]
) extends Transform

case class Translate(
  m: Array[Double]
) extends Transform

object transformParser extends JavaTokenParsers with ParserCommon {

  def scale: Parser[Scale] =
    "scale" ~> "(" ~> rep1sep(floatingPointNumber, ',') <~ ")" ^^ {
      case ns =>
        Scale(ns.toArray.map(_.toDouble))
    }

  def translate: Parser[Translate] =
    "matrix" ~> "(" ~> repN(2, floatingPointNumber) <~ ")" ^^ {
      case ns =>
        Translate(ns.toArray.map(_.toDouble))
    }

  def matrix: Parser[Matrix] =
    "matrix" ~> "(" ~> repN(6, floatingPointNumber) <~ ")" ^^ {
      case ns =>
        Matrix(ns.toArray.map(_.toDouble))
    }

  def transform: Parser[List[Transform]] =
    rep(scale|translate|matrix) // ^^ { case ts => ts }

  def parse(str: String): Either[String, List[Transform]] = {
    val result = parseAll(transform, new StringReader(str))
    result.map(Right(_)).getOrElse(Left(result.toString))
  }
}
