package edu.umass.cs.iesl.watr
package watrmarks

import java.io.StringReader
import scala.reflect.ClassTag
import scala.util.parsing.combinator._

trait ParserCommon extends Parsers {
  def read(s: String) = new StringReader(s)

  def toEither[R](pr: ParseResult[R]): Either[String, R] =
    pr.map(Right(_)).getOrElse(Left(pr.toString))

}


sealed trait RowType

case class PinRow(
  continuationChar: Char,
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

case class BioBrick(
  pinRows: List[PinRow],
  ruler: Ruler,
  text: Option[TextRow]
)


object bioParsers extends JavaTokenParsers with ParserCommon {
  import scalaz.std.list._
  import scalaz.std.either._
  import scalaz.syntax.traverse._


  def parseBioBrick(str: String): Either[String, BioBrick] = {

    val nonEmptyLines = str.split("\n")
      .flatMap(_.split("%"))
      .map(_.trim)
      .dropWhile(_.length==0)
      .takeWhile(_.length>0)
      .toList

    val parsed = nonEmptyLines
      .map{ line => toEither(parseAll(row, line)) }
      .toList.sequenceU
      .left.map(err => sys.error(s"""error parsing bio brick: ${err};\nbrick was\n${nonEmptyLines.mkString("\n")}"""))
      .right.map({ rows =>
        val pins = rows.takeWhile(_.isInstanceOf[PinRow]).map(_.asInstanceOf[PinRow])
        val rules = rows.dropWhile(_.isInstanceOf[PinRow]).takeWhile(_.isInstanceOf[PinRow])
        val text:Option[TextRow] =
          if(rows.last.isInstanceOf[TextRow]){
            Some(rows.last.asInstanceOf[TextRow])
          } else None

        BioBrick(
          pins,
          Ruler(),//  rules,
          text
        )
      })

    parsed
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

  // def emptyRow: Parser[Unit] =
  //   "^[\\s]*$".r ^^ (_ => (():Unit))

  def row: Parser[RowType] =
      pinrow | rulerRow | textRow


  def brick: Parser[BioBrick] =
    rep1sep(row, "\n") ^^ {
      case rows =>
        val pins = rows.takeWhile(_.isInstanceOf[PinRow]).map(_.asInstanceOf[PinRow])
        val rules = rows.dropWhile(_.isInstanceOf[PinRow]).takeWhile(_.isInstanceOf[PinRow])
        val text:Option[TextRow] = if(rows.last.isInstanceOf[TextRow]){
          Some(rows.last.asInstanceOf[TextRow])
        } else None

        BioBrick(
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

sealed trait Transform {
  def toMatrix: Matrix
}

case class Scale(
  m0: Double, m1: Double
) extends Transform {
  override def toString = s"""scale[$m0,$m1]"""

  override val toMatrix = Matrix(m0, 0.0, 0.0, m1, 0.0, 0.0)
}

case class Matrix(
  m0: Double, m1: Double, m2: Double,
  m3: Double, m4: Double, m5: Double
) extends Transform {

  override def toString = s"""mat[$m0,$m1,$m2,$m3,$m4,$m5]"""

  override val toMatrix = this
}

case class Translate(
  m0: Double, m1: Double
) extends Transform {
  override def toString = s"""tr[$m0,$m1]"""

  override val toMatrix = Matrix(1.0, 0.0, 0.0, 1.0, m0, m1)
}

object transformParser extends JavaTokenParsers with ParserCommon {

  def exactlyNDoubles(n: Int, arr:Seq[String]): Seq[Double] =  {
    val arrd = arr.map(_.toDouble)
    if(arrd.length!=n)
      sys.error(s"incorrect # of doubles found in ${arr}; wanted ${n}")
    else { arrd }
  }

  def scale: Parser[Scale] =
    "scale" ~> "(" ~> rep1sep(floatingPointNumber, ',') <~ ")" ^^ {
      case ns =>
        val arrn = exactlyNDoubles(2, ns)
        Scale(arrn(0), arrn(1))
    }

  def translate: Parser[Translate] =
    "translate" ~> "(" ~> repN(2, floatingPointNumber) <~ ")" ^^ {
      case ns =>
        val arrn = exactlyNDoubles(2, ns)
        Translate(arrn(0), arrn(1))
    }

  def matrix: Parser[Matrix] =
    "matrix" ~> "(" ~> repN(6, floatingPointNumber) <~ ")" ^^ {
      case ns =>
        val arrn = exactlyNDoubles(6, ns)
        Matrix(
          arrn(0), arrn(1),
          arrn(2), arrn(3),
          arrn(4), arrn(5)
        )
    }

  def transform: Parser[List[Transform]] =
    rep(scale|translate|matrix) // ^^ { case ts => ts }

  def parse(str: String): Either[String, List[Transform]] = {
    val result = parseAll(transform, new StringReader(str))
    result.map(Right(_)).getOrElse(Left(result.toString))
  }
}
