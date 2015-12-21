package edu.umass.cs.iesl.watr
package watrmarks

import java.io.StringReader
import org.scalatest._



class BioBrickSpec extends FlatSpec {
  // perhaps add namespace as a valid constraint?

  import StandardLabels._

  behavior of "labels"

  it should "allow construction of BIOLU pins from label definitions" in {
    // lazy val ld = LabelDictionary.create(word, verb)
    assert(Verb.B === BPin(Verb))
  }

  it should "ensure label names are unique within a namespace" in {


  }

  val runLolaHeader =
    """|| |V   N    V   | {ns:pos, type: {verb: v, noun: n}, unit: word}
       || |w~$ w~~$ w~$P| {ns:tok, type: {word: w, punct: p}, unit: char}
       ||1|          1  |
       ||0|0123456789012|
       |""".stripMargin

  val runLolaFull =
    """|| |V   N    V   | {ns:pos, type: {verb: v, noun: n}, unit: word}
       || |w~$ w~~$ w~$P| {ns:tok, type: {word: w, punct: p}, unit: char}
       |xx>Run Lola run.<jkl
       |""".stripMargin
      // ||1|          1  |
      // ||0|0123456789012|


  it should "allow labeled spans" in {

    val fontInfo: Option[FontInfo] = None
    val textBounds: Option[TextBounds] = None

    val expectedSpan = BrickColumns(List(
      BrickColumn(Set(Word.B, Verb.U ), 'R', fontInfo, textBounds),
      BrickColumn(Set(Word.I         ), 'u', fontInfo, textBounds),
      BrickColumn(Set(Word.L         ), 'n', fontInfo, textBounds),
      BrickColumn(Set(               ), ' ', fontInfo, textBounds),
      BrickColumn(Set(Word.B, Noun.U ), 'L', fontInfo, textBounds),
      BrickColumn(Set(Word.I         ), 'o', fontInfo, textBounds),
      BrickColumn(Set(Word.I         ), 'l', fontInfo, textBounds),
      BrickColumn(Set(Word.L         ), 'a', fontInfo, textBounds),
      BrickColumn(Set(               ), ' ', fontInfo, textBounds),
      BrickColumn(Set(Word.B, Verb.U ), 'r', fontInfo, textBounds),
      BrickColumn(Set(Word.I         ), 'u', fontInfo, textBounds),
      BrickColumn(Set(Word.L         ), 'n', fontInfo, textBounds),
      BrickColumn(Set(Punct.U        ), '.', fontInfo, textBounds)
    ))


    val lspan = biolu.parseBioBrick(runLolaFull, bioDict, None)

    assert(lspan === expectedSpan)

  }

  val runBrick =
    """|| |V   | {ns:pos, type: {verb: v}, unit: word}
       || |w~$P| {ns:tok, type: {word: w, punct: p}, unit: char}
       |  >Run.<
       |""".stripMargin

  it should "accept bounds and info" in {

    val bounds = Some(List.fill(4)(TextBounds(1, 2, 3, 4)))
    val fonts = Some(List.fill(4)(FontInfo("f1", "1px")))

    def b(i: Int) = Option(bounds.get.apply(i))
    def f(i: Int) = Option(fonts.get.apply(i))

    val lspan = biolu.parseBioBrick(runBrick, bioDict, None, bounds, fonts)
    val expectedSpan = BrickColumns(List(
      BrickColumn(Set(Word.B, Verb.U ), 'R', f(0), b(0)),
      BrickColumn(Set(Word.I         ), 'u', f(0), b(0)),
      BrickColumn(Set(Word.L         ), 'n', f(0), b(0)),
      BrickColumn(Set(Punct.U        ), '.', f(0), b(0))
    ))

  }

  behavior of "partial brick cursors over labels"

  val fullBrick =
    """|| |t~~~~~$ | {ns:pos, type: {token: t}, unit: char}
       |  >Running.<
       |""".stripMargin

  val partialBrickBegin =
    """|| |t~~~| {ns:pos, type: {token: t}, unit: char}
       |  >Runn<
       |""".stripMargin

  val partialBrickEnd =
    """||t|~~$ | {ns:pos, type: {token: t}, unit: char}
       |  >ing.<
       |""".stripMargin


  it should "parse half labels correctly" in {
    val full = biolu.parseBioBrick(fullBrick, bioDict, None, None, None)
    val begin = biolu.parseBioBrick(partialBrickBegin, bioDict, None, None, None)
    val end = biolu.parseBioBrick(partialBrickEnd, bioDict, None, None, None)


    assert(full.columns === begin.columns ++ end.columns)
    // println("full")
    // println(full)

    // println("begin")
    // println(begin)

    // println("end")
    // println(end) 
  }

}
