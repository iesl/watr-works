package edu.umass.cs.iesl.watr
package extract
package fonts

object SplineQuickParser {
  import SplineFont._

  def nonemptyLines(s: String): List[String] = {
    s.split("\n").map(_.trim).filterNot(_.isEmpty()).toList
  }


  implicit class RicherString(val os: Option[String]) extends AnyVal {

    def getI: Int = os.map(_.toInt).getOrElse(sys.error(s"expected Int from '${os}'"))
    def getD: Double = os.map(_.toDouble).getOrElse(sys.error(s"expected Double from '${os}'"))
    def getS: String = os.getOrElse(sys.error(s"expected String from '${os}'"))
  }

  def readFontProps(propStr: String): Seq[FontProp] = {
    val props = nonemptyLines(propStr).map { line =>
      val (key, value) = if (line.contains(":")) {
        val Array(k, v) = line.split(":", 2)
        (k.trim, Option(v.trim))
      } else {
        val kvarr = line.split(" ", 2)
        if (kvarr.length == 0) {
          sys.error(s"invalid line format in font.props: Line ${line}")
        } else if (kvarr.length == 1) {
          (kvarr(0).trim, None)
        } else {
          (kvarr(0).trim(), Option(kvarr(1).trim()))
        }
      }

      key match {
        case "SplineFontDB"             =>  FontProp.SplineFontDB             (value.getS) // 3.0
        case "FontName"                 =>  FontProp.FontName                 (value.getS) // FLEEJP+CorantoSC
        case "FullName"                 =>  FontProp.FullName                 (value.getS) // Coranto SC
        case "FamilyName"               =>  FontProp.FamilyName               (value.getS) // Coranto SC
        case "Weight"                   =>  FontProp.Weight                   (value.getS) // Regular
        case "Copyright"                =>  FontProp.Copyright                (value.getS) // (c) Gerard Unger, 1999. ALL RIGHTS RESERVED. Produced by Visualogik Technology & Design,  2000. This software is licensed, not sold. Unauthorised use prohibited.
        case "Version"                  =>  FontProp.Version                  (value.getS) // 001.001
        case "ItalicAngle"              =>  FontProp.ItalicAngle              (value.getD) // 0
        case "UnderlinePosition"        =>  FontProp.UnderlinePosition        (value.getI) // -154
        case "UnderlineWidth"           =>  FontProp.UnderlineWidth           (value.getI) // 23
        case "Ascent"                   =>  FontProp.Ascent                   (value.getI) // 800
        case "Descent"                  =>  FontProp.Descent                  (value.getI) // 200
        case "InvalidEm"                =>  FontProp.InvalidEm                (value.getI) // 0
        case "sfntRevision"             =>  FontProp.sfntRevision             (value.getS) // 0x00000000
        case "LayerCount"               =>  FontProp.LayerCount               (value.getI) // 2
        case "Layer"                    =>  FontProp.Layer                    (value.getS) // 0 0 "Back" 1
        case "StyleMap"                 =>  FontProp.StyleMap                 (value.getS) // 0x0000
        case "FSType"                   =>  FontProp.FSType                   (value.getS) // 0
        case "OS2Version"               =>  FontProp.OS2Version               (value.getS) // 0
        case "OS2_WeightWidthSlopeOnly" =>  FontProp.OS2_WeightWidthSlopeOnly (value.getS) // 0
        case "OS2_UseTypoMetrics"       =>  FontProp.OS2_UseTypoMetrics       (value.getS) // 0
        case "CreationTime"             =>  FontProp.CreationTime             (value.getS) // 0
        case "ModificationTime"         =>  FontProp.ModificationTime         (value.getS) // 0
        case "OS2TypoAscent"            =>  FontProp.OS2TypoAscent            (value.getS) // 0
        case "OS2TypoAOffset"           =>  FontProp.OS2TypoAOffset           (value.getS) // 0
        case "OS2TypoDescent"           =>  FontProp.OS2TypoDescent           (value.getS) // 0
        case "OS2TypoDOffset"           =>  FontProp.OS2TypoDOffset           (value.getS) // 0
        case "OS2TypoLinegap"           =>  FontProp.OS2TypoLinegap           (value.getS) // 0
        case "OS2WinAscent"             =>  FontProp.OS2WinAscent             (value.getS) // 0
        case "OS2WinAOffset"            =>  FontProp.OS2WinAOffset            (value.getS) // 0
        case "OS2WinDescent"            =>  FontProp.OS2WinDescent            (value.getS) // 0
        case "OS2WinDOffset"            =>  FontProp.OS2WinDOffset            (value.getS) // 0
        case "HheadAscent"              =>  FontProp.HheadAscent              (value.getS) // 0
        case "HheadAOffset"             =>  FontProp.HheadAOffset             (value.getS) // 0
        case "HheadDescent"             =>  FontProp.HheadDescent             (value.getS) // 0
        case "HheadDOffset"             =>  FontProp.HheadDOffset             (value.getS) // 0
        case "DEI"                      =>  FontProp.DEI                      (value.getS) // 91125
        case "Encoding"                 =>  FontProp.Encoding                 (value.getS) // Original
        case "UnicodeInterp"            =>  FontProp.UnicodeInterp            (value.getS) // Original
        case "NameList"                 =>  FontProp.NameList                 (value.getS) // none
        case "DisplaySize"              =>  FontProp.DisplaySize              (value.getS) // AGL For New Fonts
        case "AntiAlias"                =>  FontProp.AntiAlias                (value.getS) // -48
        case "FitToEm"                  =>  FontProp.FitToEm                  (value.getS) // 1
        case "BeginPrivate"             =>  FontProp.BeginPrivate             (value.getS) // 0
        case "BlueValues"               =>  FontProp.BlueValues               (value.getS) // 6
        case "BlueScale"                =>  FontProp.BlueScale                (value.getS) // 23 [-9 -1 516 531 699 712]
        case "StdHW"                    =>  FontProp.StdHW                    (value.getS) // 7 0.36364
        case "StdVW"                    =>  FontProp.StdVW                    (value.getS) // 4 [39]
        case "StemSnapH"                =>  FontProp.StemSnapH                (value.getS) // 4 [88]
        case "StemSnapV"                =>  FontProp.StemSnapV                (value.getS) // 7 [39 74]
        case "EndPrivate"               =>  FontProp.EndPrivate
        case "EndSplineFont"            =>  FontProp.EndSplineFont
        case _                          =>  FontProp.ErrProp(key)

      }}

    props
  }


  def readSplines(strs: List[String]): (Option[GlyphProp.SplineSet], List[String]) = {
    val (pre, splineStart) =  strs.span(_ != "SplineSet")
    val (splineSet, post) = splineStart.span(_ != "EndSplineSet")

    if (splineSet.isEmpty) { (None, strs) } else {
      val splines = splineSet.drop(1).map(readSpline(_))
      (Option(GlyphProp.SplineSet(splines)), (pre ++ post.drop(1)))
    }
  }


  def readSpline(s: String): GlyphProp.Spline = {
    val toks = s.split(" ").map(_.trim).filterNot(_.isEmpty()).toList
    val (nums, rest) = toks.span(t => !"cml".contains(t.head))
    if (rest.length != 2) {
      sys.error(s"malformed spline line: ${s} => nums:${nums}, rest=${rest}")
    }
    GlyphProp.Spline(nums.map(_.toFloat), rest(0).head, rest(1))
  }

  def readKeyValPair(str: String): Option[GlyphProp] = {
    import GlyphProp._
    val kvs = str.split(":", 2)
    if (kvs.length==2) {
      val Array(key, value) = kvs
      val vtrim = value.trim
      key match {
        case "StartChar"   => Some(StartChar(vtrim))
        case "Encoding"    => Some(Encoding(vtrim)) // 13 169 0
        case "Width"       => Some(Width(vtrim)) // 1027
        case "Flags"       => Some(Flags(vtrim)) // MW
        case "HStem"       => Some(HStem(vtrim)) // -216 43<400 626.5 400 638> 673 43<400.5 627>
        case "VStem"       => Some(VStem(vtrim)) // 57 43<133.5 367.5 133.5 378.5> 927 43<132.5 366.5>
        case "LayerCount"  => Some(LayerCount(vtrim)) // 2
        case _ => None
      }
    } else None
  }

  def readKeyValPairs(strs: Seq[String]): Seq[GlyphProp] = {
    val glyphsWithIndex = strs
      .filter(str => str.matches("""^\w+\s*:.*$"""))
      // .map({s => println(s"  ${s} matches"); s})
      .map(str => readKeyValPair(str))

    glyphsWithIndex.flatten
  }


  def parser(glyphStr: String): Glyph = {
    val nonEmptyLines = nonemptyLines(glyphStr)

    val (splines, remaining) = readSplines(nonEmptyLines)

    val kvPairs = readKeyValPairs(nonEmptyLines)

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


//   val CharBlockSection: P[GlyphProp] = (
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
