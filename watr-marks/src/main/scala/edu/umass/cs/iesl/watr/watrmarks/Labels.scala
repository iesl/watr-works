package edu.umass.cs.iesl.watr
package watrmarks



trait DefaultLabels {

  val word = BioLabel("tok", "word")
  val punct = BioLabel("tok", "punct")

  val verb = BioLabel("pos", "verb", 'v', word)
  val noun = BioLabel("pos", "noun", 'n', word)

  val firstName = BioLabel("nameparts", "first", 'f', word)
  val lastName = BioLabel("nameparts", "last", 'l', word)

  val person = BioLabel("ner", "person", 'p', noun)
  val place = BioLabel("ner", "place", 'g', noun)

  implicit val bioDict = BioDictionary(
    Map(
      "word" -> word,
      "verb" -> verb,
      "noun" -> noun,
      "punct" -> punct
    ),
    Map(
      'w' -> word,
      'v' -> verb,
      'n' -> noun,
      'p' -> punct
    )
  )

}

object DefaultLabels extends DefaultLabels
