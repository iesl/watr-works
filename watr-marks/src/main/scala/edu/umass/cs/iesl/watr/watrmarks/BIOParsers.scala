// package edu.umass.cs.iesl.watr
// package watrmarks

// import java.io.StringReader
// import scala.util.parsing.combinator._
// import utils._

// trait ParserCommon extends Parsers {
//   def read(s: String) = new StringReader(s)

//   def toEither[R](pr: ParseResult[R]): Either[String, R] =
//     pr.map(Right(_)).getOrElse(Left(pr.toString))

// }

// object transformParser extends JavaTokenParsers with ParserCommon {

//   def exactlyNDoubles(n: Int, arr:Seq[String]): Seq[Double] =  {
//     val arrd = arr.map(_.toDouble)
//     if(arrd.length!=n)
//       sys.error(s"incorrect # of doubles found in ${arr}; wanted ${n}")
//     else { arrd }
//   }

//   def scale0: Parser[Scale] =
//     "scale" ~> "(" ~> repN(2, floatingPointNumber) <~ ")" ^^ {
//       case ns =>
//         val arrn = exactlyNDoubles(2, ns)
//         Scale(arrn(0), arrn(1))
//     }
//   def scale: Parser[Scale] =
//     "scale" ~> "(" ~> rep1sep(floatingPointNumber, ',') <~ ")" ^^ {
//       case ns =>
//         val arrn = exactlyNDoubles(2, ns)
//         Scale(arrn(0), arrn(1))
//     }

//   def translate: Parser[Translate] =
//     "translate" ~> "(" ~> repN(2, floatingPointNumber) <~ ")" ^^ {
//       case ns =>
//         val arrn = exactlyNDoubles(2, ns)
//         Translate(arrn(0), arrn(1))
//     }

//   def matrix: Parser[Matrix] =
//     "matrix" ~> "(" ~> repN(6, floatingPointNumber) <~ ")" ^^ {
//       case ns =>
//         val arrn = exactlyNDoubles(6, ns)
//         Matrix(
//           arrn(0), arrn(1),
//           arrn(2), arrn(3),
//           arrn(4), arrn(5)
//         )
//     }

//   def transform: Parser[List[Transform]] =
//     rep(scale|scale0|translate|matrix) // ^^ { case ts => ts }

//   def parse(str: String): Either[String, List[Transform]] = {
//     val result = parseAll(transform, new StringReader(str))
//     result.map(Right(_)).getOrElse(Left(result.toString))
//   }
// }
