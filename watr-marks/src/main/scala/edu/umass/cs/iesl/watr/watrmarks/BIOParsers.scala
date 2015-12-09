
package edu.umass.cs.iesl.watr.watrmarks

import java.io.StringReader
import scala.util.parsing.combinator._

trait ParserCommon extends Parsers {
  def read(s: String) = new StringReader(s)

  def toEither[L, R](pr: ParseResult[R]) =
    pr.map(Right(_)).getOrElse(Left(pr.toString))

}


object bioParsers extends JavaTokenParsers with ParserCommon {

  def parseBioBlock(str: String): Either[String, List[(Char, String, BioObj)]] = {
    toEither(parseAll(block, str))
  }

  override val whiteSpace = "".r


  def continuationChar: Parser[Char] =
    "[a-z ]".r ^^ { _.head }

  def pinChar: Parser[Char] =
    """[a-zA-Z~ $]""".r ^^ { _.head }


  def row: Parser[(Char, String, BioObj)] =
    "|" ~> continuationChar ~ "|" ~ rep(pinChar) ~ "|" ~ ".*".r ^^ {
      case (c ~ _ ~ bio ~ _ ~ defs) =>
        val res = biomapParsers.parseAll(biomapParsers.obj, new StringReader(defs))
        (c, bio.mkString, res.get)
    }

  def block: Parser[List[(Char, String, BioObj)]] =
    rep1sep(row, "\n")

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

