package edu.umass.cs.iesl.watr.watrmarks


import java.io.StringReader
import org.scalatest._

class BIOParserSpec extends FlatSpec {

  behavior of "BIO parsers"


  it should "parse json-like syntax" in {
    val examples = List(
      ("""{ x: a.b.c }""",
        BioObj(Map("x" -> BioIdent(List("a", "b", "c"))))
      ),

      ("""{ x: y }""",
       BioObj(Map("x" -> BioIdent(List("y"))))),

      ("""{ns:pos, type: {verb: v, noun: n}, unit: word}""",
        BioObj(Map(
          "ns" -> BioIdent(List("pos")),
          "type" -> BioObj(Map(
            "verb" -> BioIdent(List("v")),
            "noun" -> BioIdent(List("n")))),
          "unit" -> BioIdent(List("word"))))
      )
    )
    examples foreach { case(instr, expected)  =>
      val actual = biomapParsers.parseBiomapBlock(instr)
      assert(Right(expected) === actual)
    }

  }

  it should "parse bio-row syntax" in {
    val examples = List(
      ("""| |a~$ b~$ X|{}""",
        (' ', "a~$ b~$ X", BioObj())
      ),
      ("""|q| | {}""",
        ('q', " ", BioObj())
      )
    )
    examples foreach { case(instr, expected)  =>
      val actual = bioParsers.toEither(bioParsers.parse(bioParsers.row, instr))
      assert(Right(expected) === actual)
    }
  }

  val bioLabels =
    """`| |V   N    V   | {ns:pos, type: {verb: v, noun: n}, unit: word}
       `| |w~$ w~~$ w~$P| {ns:tok, type: {word: w, punct: p}, unit: char}
       `| |          1  |
       `|z|~~$          |
       `| |0123456789012|
       `""".stripMargin('`')

  it should "parse bio-block syntax" in {
    val examples = List(
      ("""|| |V   N    V   | {ns:pos, type: {verb: v, noun: n}, unit: word}
          || |w~$ w~~$ w~$P| {ns:tok, type: {word: w, punct: p}, unit: char}
          ||z|~~$          | {}
          || |0123456789012|
          |""".stripMargin,
        ()
      )
    )
      // || |0123456789012|

    examples foreach { case(instr, expected)  =>
      val actual = bioParsers.toEither(bioParsers.parse(bioParsers.block, instr))

      // actual.right.foreach { bioline =>
      //   println(bioline.mkString("\n"))
      // }

      assert(actual.isRight)
    }

  }

  behavior of "BIO label serialization"


  // val runLolaLabelsx =
  //   """`| |V   N    V   | {ns:pos, type: {verb: v, noun: n}, unit: word}
  //      `| |w~$ w~~$ w~$P| {ns:tok, type: {word: w, punct: p}, unit: char}
  //      `| |            z|
  //      `|#|          1  |
  //      `|#|0123456789012|
  //      `  >Run Lola run.<
  //      `""".stripMargin('`')



  it should "serialize" in {
    // println(s"parsing $bioLabels")
    // val parsed = biolu.parseBioBlock(bioLabels)
    // parsed.foreach { case (m, l, c) =>
    //   println(s"""m: ${m}, l: $l, c: $c""")
    // }

    // bio="
    //   | |P           | {ns: waddlers, type: {penguin: p}, unit: token}
    //   | |f       $   | {ns: soarers, type: {falcon: f}, unit: token}
    //   | |q~~$q~~$q~~$| {ns: flappers, type: {token: q}, unit: char}
    //   | |          1 |
    //   | |012345678901|
    //    ">abcdefghijkl</tspan>
    // bio="
    //   | |P           P  | {type: {penguin: p}, unit: token}
    //   | |f       $      | {type: {falcon: f}, unit: token}
    //   | |q~~$q~~$q~~$q~~| {type: {token: q}, unit: char}
    //   | |1       2      |
    //   | |234567890123456|
    //    ">mnopqrstuvwxyz1</tspan>
    // bio="
    //   | |$q~~$Q| {type: {token: q}, unit: char}
    //   | |2  3  |
    //   | |789012|
    //    ">234567</tspan>
  }
}
