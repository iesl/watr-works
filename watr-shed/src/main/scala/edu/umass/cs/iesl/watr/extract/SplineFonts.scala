package edu.umass.cs.iesl.watr
package extract

object SplineQuickParser {
  import SplineFont._


  def readSplines(strs: List[String]): (Option[SplineSet], List[String]) = {
    val (pre, splineStart) =  strs.span(_ != "SplineSet")
    val (splineSet, post) = splineStart.span(_ != "EndSplineSet")

    if (splineSet.isEmpty) { (None, strs) } else {
      val splines = splineSet.drop(1).map(readSpline(_))
      (Option(SplineSet(splines)), (pre ++ post.drop(1)))
    }
  }


  def readSpline(s: String): Spline = {
    val toks = s.split(" ").map(_.trim).filterNot(_.isEmpty()).toList
    val (nums, rest) = toks.span(t => !"cml".contains(t.head))
    if (rest.length != 2) {
      sys.error(s"malformed spline line: ${s} => nums:${nums}, rest=${rest}")
    }
    Spline(nums.map(_.toFloat), rest(0).head, rest(1))
  }

  def readKeyValPair(str: String): Option[GlyphStanza] = {
    str.split(":", 2) match {
      case Array("StartChar"  , rest) => Some(StartChar(rest))
      case Array("Encoding"   , rest) => Some(Encoding(rest)) // 13 169 0
      case Array("Width"      , rest) => Some(Width(rest)) // 1027
      case Array("Flags"      , rest) => Some(Flags(rest)) // MW
      case Array("HStem"      , rest) => Some(HStem(rest)) // -216 43<400 626.5 400 638> 673 43<400.5 627>
      case Array("VStem"      , rest) => Some(VStem(rest)) // 57 43<133.5 367.5 133.5 378.5> 927 43<132.5 366.5>
      case Array("LayerCount" , rest) => Some(LayerCount(rest)) // 2
      case _ => None
    }
  }

  def readKeyValPairs(strs: Seq[String]): Seq[GlyphStanza] = {
    val glyphsWithIndex = strs
      .filter(str => str.matches("""^\\w+ *:"""))
      .map(str => readKeyValPair(str))

    glyphsWithIndex.flatten
  }


  def parser(glyphStr: String): Glyph = {
    val nonEmptyLines = glyphStr.split("\n").map(_.trim).filterNot(_.isEmpty()).toList

    val (splines, remaining) = readSplines(nonEmptyLines)
    val kvPairs = readKeyValPairs(remaining)

    splines.map({spl =>
      Glyph(spl +: kvPairs)
    }).getOrElse(
      Glyph(kvPairs)
    )
  }
}

case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
  def apply(t: T) = f(t)
  override def toString() = name

}

object SplineLexer {
  // Whitespace sensitive parsers
  import fastparse.all._


  //Numbers and digits

  val digits = "0123456789"
  val Digit = P( CharIn(digits) )
  val hexDigits = digits + "abcdefABCDEF"
  val HexDigit = P( CharIn(hexDigits) )
  val HexDigits = P(CharsWhile(hexDigits.contains(_)) )
  val HexNum = P( "0x" ~ HexDigits)
  val DecNum = P( CharsWhile(digits.contains(_)) ).!.map(_.toInt)
  val DecNumSigned = P( CharIn("+-").? ~ DecNum )


  val Exp = P( CharIn("Ee") ~ CharIn("+-").? ~ DecNum )
  val FloatType = P( CharIn("fFdD") )

  val WSChars = P( CharsWhile("\u0020\u0009".contains(_)) )
  val Newline = P( StringIn("\r\n", "\n") )

  val Letter = P( CharPred(c => c.isLetter) )
  val Lower = P( CharPred(c => c.isLower) )
  val Upper = P( CharPred(_.isUpper) )




  val SplineChar: P[Char] = P(CharIn("cml").! ~ CharPred(_.isSpaceChar)).map(_.head)
}

// object SplineFontParsers {
//   import SplineLexer._

//   import SplineFont._
//   // import fastparse.CharPredicates._
//   val White = fastparse.WhitespaceApi.Wrapper{
//     import fastparse.all._
//     NoTrace(" ".rep)
//   }
//   import fastparse.noApi._
//   import White._

//   /**
//     * Parses all whitespace, excluding newlines. This is only
//     * really useful in e.g. {} blocks, where we want to avoid
//     * capturing newlines so semicolon-inference would work
//     */
//   val WS = P( NoCut(NoTrace(WSChars.rep)) )

//   /**
//     * Parses whitespace, including newlines.
//     * This is the default for most things
//     */
//   val WL0 = P( NoTrace((WSChars | Newline).rep) )(sourcecode.Name("WL"))
//   val WL = P( NoCut(WL0) )

//   // val Newline = P( WL ~ Basic.Newline )

//   val NotNewline: P0 = P( &( WS ~ !Newline ) )

//   val NewLine = P(CharsWhile("\n\r".contains(_)))
//   val EverythingToNewLine = P(CharsWhile(!"\n\r".contains(_)))

//   val SplineCmdHints = P((digits ~ ("x" ~ HexDigits).?)).!

//   val splineLine: P[Spline] ={
//     val parser = (
//       DecNum.rep
//         ~ SplineChar
//         ~ SplineCmdHints
//     )

//     parser.map({ case (nums, code, flags) =>
//       Spline(nums, code, flags)
//     })
//   }

//   val Alpha = CharPred((_:Char).isLetter)
//   val AlphaNumeric = CharPred((_:Char).isLetterOrDigit)

//   val Identifier: P[String] =
//     P( Letter.rep ).!.filter(!keywordList.contains(_))

//   val keywordList = Set(
//     "Encoding"
//   )

//   def Keyword(keyword: String) = P(keyword)

//   // Encoding: 13 169 0
//   val EncodingLine = (
//     Keyword("Encoding") ~ ":" ~ DecNum.rep(3)
//   )

//   // Generic parsers as standins until full impl
//   val KeyValLine: P[GenKeyVal] = (
//     (Identifier.! ~ ":" ~ EverythingToNewLine.!).map{ case (k, v) =>
//       GenKeyVal(k, v)
//     }
//   )

//   val validKeys = List(
//     "Fore"
//   )

//   val KeyLine: P[GenKey] = {
//     Identifier
//       .filter(validKeys.contains(_)).!
//       .map({ case k =>
//         GenKey(k)
//       })
//   }


//   val SplineSetParser: P[SplineSet] = {
//     val parser = (
//       Keyword("SplineSet") ~ NewLine ~
//         (splineLine ~ NewLine).rep ~
//         Keyword("EndSplineSet") ~ NewLine
//     )

//     parser.map { ss: Seq[Spline] =>
//       SplineSet(ss)
//     }
//   }


//   val CharBlockSection: P[GlyphStanza] = (
//     (KeyValLine ~ NewLine)
//       | SplineSetParser
//       | KeyLine
//   )



//   val CharBlockParser: P[Glyph] = {
//     val parser = (
//       Keyword("StartChar") ~ ":" ~ Identifier ~ Newline ~
//       CharBlockSection.rep ~
//       Keyword("EndChar") ~
//       (NewLine | End)
//     )

//     parser.map { case (s, stanzas) =>
//       Glyph(
//         stanzas.toSeq
//       )
//     }

//   }


//   def parseGlyph(glyphStr: String): Either[String, Glyph] = {
//     parseOrError(glyphStr, CharBlockParser)
//   }

//   def parseCharData(dataBlock: String): Unit = {

//   }

//   import fastparse.core.Parsed.Failure

//   def outputFailure(input: String, failExtra: Failure.Extra): String = {
//     val failedLine = input.split("\n").drop(failExtra.line-1).take(1).head
//     val marker = ("_"*failExtra.col) + "^"
//     s"""|Parse failed: ${failExtra.traced.trace}
//         |   >${failedLine}
//         |   >${marker}
//         |""".stripMargin
//   }

//   def parseOrError[T](input: String, p: P[T]): Either[String, T] = {
//     p.parse(input) match {
//       case Parsed.Success(value, successIndex) =>
//         Right(value)
//       case Parsed.Failure(parser, failIndex, failExtra) =>
//         Left(outputFailure(input, failExtra))
//     }
//   }
// }

object SplineFont {
  sealed trait GlyphStanza

  case class GenKeyVal(key: String, v: String) extends GlyphStanza
  case class GenKey(key: String) extends GlyphStanza

  case class Spline(ns: Seq[Float], code: Char, flags: String)

  case class SplineSet(splines: Seq[Spline]) extends GlyphStanza

  case class StartChar(
    glyphName: String
  ) extends GlyphStanza

  case class Encoding(
    other: String
  ) extends GlyphStanza

  case class Width(
    other: String
  ) extends GlyphStanza

  case class Flags(
    other: String
  ) extends GlyphStanza

  case class HStem(
    other: String
  ) extends GlyphStanza

  case class VStem(
    other: String
  ) extends GlyphStanza

  case class LayerCount(
    other: String
  ) extends GlyphStanza


  case class Props(
  )

  case class Dir(
    props: Props,
    glyphs: Seq[Glyph]
  )
  case class Glyph(
    stanzas: Seq[GlyphStanza]
  )
}


// object SampleJsonParser {


//   // Here is the parser
//   val Whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")
//   val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
//   val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

//   val space         = P( CharsWhile(Whitespace).? )
//   val digits        = P( CharsWhile(Digits))
//   val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
//   val fractional    = P( "." ~ digits )
//   val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )

//   val number = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
//     x => x.toDouble
//   )

//   val `null`        = P( "null" ).map(_ => null)
//   val `false`       = P( "false" ).map(_ => true)
//   val `true`        = P( "true" ).map(_ => false)

//   val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
//   val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
//   val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )

//   val strChars = P( CharsWhile(StringChars) )
//   val string =
//     P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(s => s)

//   // val array =
//   //   P( "[" ~/ jsonExpr.rep(sep=",".~/) ~ space ~ "]").map(Js.Arr(_:_*))

//   // val pair = P( string.map(id => id) ~/ ":" ~/ jsonExpr )

//   // val obj =
//   //   P( "{" ~/ pair.rep(sep=",".~/) ~ space ~ "}").map(xx => xx)

//   // val jsonExpr: P[Js.Val] = P(
//   //   space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
//   // )



// }

// object SampleParser {
//   def eval(tree: (String, Seq[Int])) = tree match{
//     case ("+", nums) => nums.reduceLeft(_+_)
//     case ("-", nums) => nums.reduceLeft(_-_)
//     case ("*", nums) => nums.reduceLeft(_*_)
//     case ("/", nums) => nums.reduceLeft(_/_)
//   }

//   class Parser(indent: Int){
//     val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )

//     val deeper: P[Int] = P( " ".rep(indent + 1).!.map(_.length) )

//     val blockBody: P[Seq[Int]] = "\n" ~ deeper.flatMap(i =>
//       new Parser(indent = i).factor.rep(1, sep = ("\n" + " " * i).~/)
//     )

//     val block: P[Int] = P( CharIn("+-*/").! ~/ blockBody).map(eval)

//     val factor: P[Int] = P( number | block )

//     val expr: P[Int]   = P( block ~ End )



//   }

//   val expr = new Parser(indent = 0).expr
// }
