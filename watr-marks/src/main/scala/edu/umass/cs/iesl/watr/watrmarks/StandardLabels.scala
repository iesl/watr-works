package edu.umass.cs.iesl.watr
package watrmarks


trait DocSegLabels {
  val Zone = Label("ds", "zone")
  val Image = Label("ds", "image")
  val Table = Label("ds", "table")
  val Line = Label("ds", "line")
  val Char = Label("ds", "char")
}

trait TokenLabels {

  val Token = Label("tok", "token")
  val Word = Label("tok", "word")
  val Punct = Label("tok", "punct")
  val Sup = Label("tok", "sup")
  val Sub = Label("tok", "sub")

  val allTokenLabels = List(
    Word,
    Punct,
    Token
  )
}


trait POSLabels extends TokenLabels {

  val Verb = Label("pos", "verb")
  val Noun = Label("pos", "noun")

  val allPOSLabels = List(
    Verb,
    Noun,
    Word,
    Punct
  )

}

trait NERLabels extends POSLabels {
  val Person = Label("ner", "person")
  val Place = Label("ner", "place")

  val allNERLabels = List(
    Person,
    Place
  )

}


trait PersonalNameLabels extends TokenLabels {

  val Name          = Label("name", "name")
  val FirstName     = Label("name", "first")
  val MiddleInitial = Label("name", "middle-i")
  val Middle        = Label("name", "middle")
  val LastName      = Label("name", "last")
  val Letters       = Label("name", "letters")

  val allPersonalNameLabels = List(
    FirstName,
    LastName
  )
}


object StandardLabels
    extends POSLabels
    with NERLabels
    with DocSegLabels
    with PersonalNameLabels {


  object CharLabel extends Label("", "char")
  object PageLabel extends Label("", "page")



  val allStandardLabels = allPersonalNameLabels ++ allNERLabels ++ allPOSLabels ++ allTokenLabels ++ Seq(
    CharLabel, PageLabel
  )

  implicit val bioDict = BioDictionary(
    allStandardLabels.map(l => (l.fqn -> l) ).toMap,
    allStandardLabels.map(l => (l.key(0) -> l) ).toMap
  )

}
