package edu.umass.cs.iesl.watr
package watrcolors
package models

import _root_.io.circe, circe.syntax._

// import circe.generic._
// import circe.generic.semiauto._


/**
  * Utility to quick-and-dirty print out the schema as json for upload to WatrColor server
  **/


object LabelSchemaPrinter extends App with utils.AppMainBasics {
  import watrmarks._
  val jsonPrinter = circe.Printer(
    preserveOrder = true,
    dropNullValues = false,
    indent = "    ",
    lbraceRight = "\n",
    rbraceLeft = "\n",
    lbracketRight = "",
    rbracketLeft = "\n",
    lrbracketsEmpty = "",
    arrayCommaRight = " ",
    objectCommaRight = "\n",
    colonLeft = " ",
    colonRight = " "
  )

  val headerLabelSchema = {

    val Title = Label.auto
    val Abstract = Label.auto

    val Authors = Label.auto
    val Author = Label.auto
    val FirstName = Label.auto
    val MiddleName = Label.auto
    val LastName = Label.auto
    val NobilityParticles = Label.auto
    val Degree = Label.auto
    val HereditySuffix = Label.auto

    val Affiliations = Label.auto
    val Affiliation = Label.auto

    val Faculty     = Label.auto
    val Hospital    = Label.auto
    val Zipcode     = Label.auto
    val Department  = Label.auto
    val Institution = Label.auto
    val University  = Label.auto
    val Company     = Label.auto
    val Address     = Label.auto
    val City        = Label.auto
    val Region      = Label.auto
    val Country     = Label.auto
    val Email       = Label.auto
    val Other       = Label.auto


    val affilationSchema = {
      LabelSchema(Affiliations, None, None, List(
        LabelSchema(Affiliation)(List(
          LabelSchema(Faculty     , None, Some("Name of a faculty member"), List()),
          LabelSchema(Hospital    , None, Some("Name of a hospital"), List()),
          LabelSchema(Zipcode     , None, Some("Postal code"), List()),
          LabelSchema(Department  , None, Some("A department, e.g., Department of Physics, Mathematics, etc."), List()),
          LabelSchema(Institution , None, Some("An institution"), List()),
          LabelSchema(University  , None, Some("A university, e.g., Univ. of Massachusetts."), List()),
          LabelSchema(Company     , None, Some("Company. Usually contains strings like “co.”, “pvt.”, “ltd.” etc."), List()),
          LabelSchema(Address     , None, Some("Street address"), List()),
          LabelSchema(City        , None, Some("City in which a university or organization is located."), List()),
          LabelSchema(Region      , None, Some("Region in which a  city is located.  States in USA or Canada, or Counties/Regions in Europe etc."), List()),
          LabelSchema(Country     , None, Some(""), List()),
          LabelSchema(Email       , None, Some(""), List()),
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
            LabelSchema(NobilityParticles),
            LabelSchema(Degree),
            LabelSchema(HereditySuffix)
          )
        )
      ))

    }


    LabelSchemas(
      List(
        LabelSchema(Title),
        LabelSchema(Abstract),
        authorNamesSchema,
        affilationSchema
      )
    )
  }


  val argMap = argsToMap(args)
  val filename = argMap.get("file").flatMap(_.headOption)
    .getOrElse(sys.error("please specify output file: --file zz.json"))
  import ammonite.{ops => fs}, fs._
  val output = headerLabelSchema.asJson.spaces4

  val outputFile = pwd / filename

  if (!fs.exists(outputFile)) {
    fs.write(outputFile, output)
  } else {
    println(s"output file ${outputFile} exists. Won't overwrite...")
  }
}
