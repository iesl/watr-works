package edu.umass.cs.iesl.watr
package watrmarks


trait TokenLabels {

  val Token = BioLabel("tok", "token")
  val Word = BioLabel("tok", "word")
  val Punct = BioLabel("tok", "punct")

  val allTokenLabels = List(
    Word,
    Punct,
    Token
  )
}


trait POSLabels extends TokenLabels {

  val Verb = BioLabel("pos", "verb", 'v', Word)
  val Noun = BioLabel("pos", "noun", 'n', Word)

  val allPOSLabels = List(
    Verb,
    Noun,
    Word,
    Punct
  )

}

trait NERLabels extends POSLabels {
  val Person = BioLabel("ner", "person", 'p', Noun)
  val Place = BioLabel("ner", "place", 'g', Noun)

  val allNERLabels = List(
    Person,
    Place
  )

}


trait PersonalNameLabels extends TokenLabels {

  val Name          = BioLabel("name", "name", 'n', Word)

  val FirstName     = BioLabel("name", "first", 'f', Name.Word)
  val MiddleInitial = BioLabel("name", "middle-i", 'i', Name.Word)
  val Middle        = BioLabel("name", "middle", 'm', Name.Word)
  val LastName      = BioLabel("name", "last", 'l', Name.Word)
  val Letters       = BioLabel("name", "letters", 't', Name.Word)

  val allPersonalNameLabels = List(
    FirstName,
    LastName
  )
}

// This label is handled specially, as characters are not explicitly labeled
object CharLabel extends BioLabel("char", "char", 'c', None)

// object PageLabel extends BioLabel("page", "page", 'p', None)

object StandardLabels
    extends POSLabels
    with NERLabels
    with PersonalNameLabels {



  val allStandardLabels = allPersonalNameLabels ++ allNERLabels ++ allPOSLabels ++ allTokenLabels

  implicit val bioDict = BioDictionary(
    allStandardLabels.map(l => (l.namespace+l.name -> l) ).toMap,
    allStandardLabels.map(l => (l.c -> l) ).toMap
  )

}
