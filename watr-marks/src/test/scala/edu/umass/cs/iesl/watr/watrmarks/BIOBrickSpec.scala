package edu.umass.cs.iesl.watr.watrmarks

import java.io.StringReader
import org.scalatest._



class BIOBrickSpec extends FlatSpec {
  // perhaps add namespace as a valid constraint?

  import DefaultLabels._

  behavior of "labels"

  it should "allow construction of BIOLU pins from label definitions" in {
    // lazy val ld = LabelDictionary.create(word, verb)
    assert(verb.B === BPin(verb))
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

    val expectedSpan = LabeledSpan(List(
      LabeledColumn(Set(word.B, verb.U ), 'R', fontInfo, textBounds),
      LabeledColumn(Set(word.I         ), 'u', fontInfo, textBounds),
      LabeledColumn(Set(word.L         ), 'n', fontInfo, textBounds),
      LabeledColumn(Set(               ), ' ', fontInfo, textBounds),
      LabeledColumn(Set(word.B, noun.U ), 'L', fontInfo, textBounds),
      LabeledColumn(Set(word.I         ), 'o', fontInfo, textBounds),
      LabeledColumn(Set(word.I         ), 'l', fontInfo, textBounds),
      LabeledColumn(Set(word.L         ), 'a', fontInfo, textBounds),
      LabeledColumn(Set(               ), ' ', fontInfo, textBounds),
      LabeledColumn(Set(word.B, verb.U ), 'r', fontInfo, textBounds),
      LabeledColumn(Set(word.I         ), 'u', fontInfo, textBounds),
      LabeledColumn(Set(word.L         ), 'n', fontInfo, textBounds),
      LabeledColumn(Set(punct.U        ), '.', fontInfo, textBounds)
    ))


    val lspan = biolu.parseBioBlock(runLolaFull, bioDict, None)

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

    val lspan = biolu.parseBioBlock(runBrick, bioDict, None, bounds, fonts)
    val expectedSpan = LabeledSpan(List(
      LabeledColumn(Set(word.B, verb.U ), 'R', f(0), b(0)),
      LabeledColumn(Set(word.I         ), 'u', f(0), b(0)),
      LabeledColumn(Set(word.L         ), 'n', f(0), b(0)),
      LabeledColumn(Set(punct.U        ), '.', f(0), b(0))
    ))

  }

  it should "allow labeled locations, applying labels" in {
  }

}
