package edu.umass.cs.iesl.watr
package watrmarks


trait DocSegLabels {

  // val Pages = Label("ds", "pages")
  val Page = Label("ds", "page")
  val Column = Label("ds", "column")

  // val Section = Label("ds", "section")
  val SectionHeadingLine = Label("ds", "section-heading-line")
  val SectionNumber = Label("ds", "section-number")
  val SectionTitle = Label("ds", "section-title")


  val TextBlock = Label("ds", "text-block")
  val ParaBegin = Label("ds", "para-begin")
  val Para = Label("ds", "para")
  val Image = Label("ds", "image")
  val Table = Label("ds", "table")
  // val Line = Label("ds", "line")
  val VisualLine = Label("ds", "visual-line")
  val TextLine = Label("ds", "text-line")

  val TokenizedLine = Label("ds", "tline")
  val Token = Label("ds", "token")
  val Sup = Label("ds", "sup")
  val Sub = Label("ds", "sub")
}



// trait POSLabels extends DocSegLabels {

//   val Verb = Label("pos", "verb")
//   val Noun = Label("pos", "noun")

//   val allPOSLabels = List(
//     Verb,
//     Noun,
//     Word,
//     Punct
//   )

// }

// trait NERLabels extends POSLabels {
//   val Person = Label("ner", "person")
//   val Place = Label("ner", "place")

//   val allNERLabels = List(
//     Person,
//     Place
//   )

// }


// trait PersonalNameLabels extends TokenLabels {

//   val Name          = Label("name", "name")
//   val FirstName     = Label("name", "first")
//   val MiddleInitial = Label("name", "middle-i")
//   val Middle        = Label("name", "middle")
//   val LastName      = Label("name", "last")
//   val Letters       = Label("name", "letters")

//   val allPersonalNameLabels = List(
//     FirstName,
//     LastName
//   )
// }


// extends POSLabels
// with NERLabels
// with PersonalNameLabels {

object StandardLabels
    extends DocSegLabels {

  // object CharLabel extends Label("", "char")
  // object PageLabel extends Label("", "page")



  // val allStandardLabels = allPersonalNameLabels ++ allNERLabels ++ allPOSLabels ++ allTokenLabels ++ Seq(
  //   CharLabel, PageLabel
  // )

  // implicit val bioDict = BioDictionary(
  //   allStandardLabels.map(l => (l.fqn -> l) ).toMap,
  //   allStandardLabels.map(l => (l.key(0) -> l) ).toMap
  // )

  implicit val bioDict = BioDictionary(
    Map(), Map()
  )

}
