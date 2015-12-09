package edu.umass.cs.iesl.watr.watrmarks

import java.io.StringReader
import org.scalatest._


object standardLabels {
  val word = BioLabel("tok", "word")
  val punct = BioLabel("tok", "punct")

  val verb = BioLabel("pos", "verb", 'v', word)
  val noun = BioLabel("pos", "noun", 'n', word)

  val firstName = BioLabel("nameparts", "first", 'f', word)
  val lastName = BioLabel("nameparts", "last", 'l', word)

  val person = BioLabel("ner", "person", 'p', noun)
  val place = BioLabel("ner", "place", 'g', noun)


}

class BIOBrickSpec extends FlatSpec {
  // perhaps add namespace as a valid constraint?

  import standardLabels._

  behavior of "labels"

  it should "allow construction of BIOLU pins from label definitions" in {
    // lazy val ld = LabelDictionary.create(word, verb)

    assert(verb.B === BPin(verb))
  }

  it should "ensure label names are unique within a namespace" in {


  }

  it should "allow labeled spans" in {
    val bio="""
      | |V   N    V   | {ns:pos,  type: {verb: v, noun: n}, unit: word}
      | |w~$ w~~$ w~$P| {ns:tok, type: {word: w, punct: p}, unit: char}
      | |          1  |
      | |0123456789012|
         Run Lola run.
    """

    LabeledSpan(List(
      LabeledColumn(Set(word.B, verb.U)),
      LabeledColumn(Set(word.I)),
      LabeledColumn(Set(word.L)),
      LabeledColumn(),
      LabeledColumn(Set(word.B, noun.U)),
      LabeledColumn(Set(word.I)),
      LabeledColumn(Set(word.I)),
      LabeledColumn(Set(word.L)),
      LabeledColumn(),
      LabeledColumn(Set(word.B, verb.U)),
      LabeledColumn(Set(word.I)),
      LabeledColumn(Set(word.L)),
      LabeledColumn()
    ))

  }

  it should "allow labeled locations, applying labels" in {
    val lloc = LabeledLocation(
      current = List(),
      prevs = List(),
      nexts = List(
        LabeledColumn(Set(word.B, verb.U)),
        LabeledColumn(Set(word.I)),
        LabeledColumn(Set(word.L)),
        LabeledColumn(),
        LabeledColumn(Set(word.B, noun.U)),
        LabeledColumn(Set(word.I)),
        LabeledColumn(Set(word.I)),
        LabeledColumn(Set(word.L)),
        LabeledColumn(),
        LabeledColumn(Set(word.B, verb.U)),
        LabeledColumn(Set(word.I)),
        LabeledColumn(Set(word.L)),
        LabeledColumn()
      )
    )

    val firstNoun = lloc.next(noun)

    val nerPerson = firstNoun.addLabel(person)

    val nerLabeledSpan = nerPerson.toLabeledSpan


  }

}
