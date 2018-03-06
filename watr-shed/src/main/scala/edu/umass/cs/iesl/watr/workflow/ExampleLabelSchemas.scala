package edu.umass.cs.iesl.watr
package workflow

object ExampleLabelSchemas {
  import watrmarks._
  import TypeTags._


  val Title = Label.auto
  val Abstract = Label.auto

  val Authors = Label.auto
  val Author = Label.auto
  val FirstName = Label.auto
  val MiddleName = Label.auto
  val LastName = Label.auto
  val NobiliaryParticle = Label.auto
  val Degree = Label.auto
  val HereditySuffix = Label.auto

  val Affiliations = Label.auto
  val Affiliation = Label.auto

  val Institution = Label.auto
  val Address     = Label.auto
  val City        = Label.auto
  val Region      = Label.auto
  val Country     = Label.auto
  val Zip         = Label.auto
  val Email       = Label.auto
  val Other       = Label.auto

  val Keywords       = Label.auto
  val Keyword       = Label.auto

  val NoteMarker        = Label.auto

  val affilationSchema = {
    LabelSchema(Affiliations, None, None, List(
      LabelSchema(Affiliation)(List(
        LabelSchema(Institution , None, Some("An institution"), List()),
        LabelSchema(Address     , None, Some("Street address"), List()),
        LabelSchema(City        , None, Some("City in which a university or organization is located."), List()),
        LabelSchema(Region      , None, Some("Region in which a  city is located.  States in USA or Canada, or Counties/Regions in Europe etc."), List()),
        LabelSchema(Zip         , None, Some("Postal code"), List()),
        LabelSchema(Country     , None, None, List()),
        LabelSchema(Email),
        LabelSchema(NoteMarker),
        LabelSchema(Other       , None, Some("Anything not matching another category"), List())
      ))
    ))
  }


  val authorNamesSchema = {
    LabelSchema(Authors, Some(('a', 's')), None, List(
      LabelSchema(
        Author, None, Some("Full author name, including degree suffix, title"), List(
          LabelSchema(FirstName),
          LabelSchema(MiddleName),
          LabelSchema(LastName),
          LabelSchema(NobiliaryParticle),
          LabelSchema(Degree),
          LabelSchema(HereditySuffix),
          LabelSchema(NoteMarker)
        )
      )
    ))
  }

  val keywordSchema = {
    LabelSchema(Keywords, None, None, List(
      LabelSchema(Keyword)
    ))
  }


  val combinedSchemas = LabelSchemas(
    LabelSchemaName("PaperHeadersV1"),
    List(
      LabelSchema(Title),
      LabelSchema(Abstract),
      authorNamesSchema,
      affilationSchema,
      keywordSchema
    )
  )

  val headerLabelSchema = combinedSchemas

}

// object LabelSchemaPrinter extends App with utils.AppMainBasics {
//   /**
//     * Utility to quick-and-dirty print out the schema as json for upload to WatrColor server
//     **/
//   import watrmarks._
//   val jsonPrinter = circe.Printer(
//     preserveOrder = true,
//     dropNullValues = false,
//     indent = "    ",
//     lbraceRight = "\n",
//     rbraceLeft = "\n",
//     lbracketRight = "",
//     rbracketLeft = "\n",
//     lrbracketsEmpty = "",
//     arrayCommaRight = " ",
//     objectCommaRight = "\n",
//     colonLeft = " ",
//     colonRight = " "
//   )

//   val argMap = argsToMap(args)
//   val filename = argMap.get("file").flatMap(_.headOption)
//     .getOrElse(sys.error("please specify output file: --file zz.json"))
//   import ammonite.{ops => fs}, fs._
//   val output = headerLabelSchema.asJson.spaces4

//   val outputFile = pwd / filename

//   if (!fs.exists(outputFile)) {
//     fs.write(outputFile, output)
//   } else {
//     println(s"output file ${outputFile} exists. Won't overwrite...")
//   }
// }
