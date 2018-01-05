package edu.umass.cs.iesl.watr
package watrmarks

import scala.scalajs.js.annotation._
import textboxing.{TextBoxing => TB}, TB._
import _root_.io.circe, circe._
// import circe.generic._
import circe.generic.semiauto._



@JSExportTopLevel("watr.watrmarks.LabelSchema")
case class LabelSchema(
  label: Label,
  abbrev: Option[(Char, Char)] = None,
  description: Option[String] = None,
  children: List[LabelSchema] = List()
) {
  def getAbbrev(): String = {
    abbrev
      .map{ case (c1, c2) => ""+c1+c2 }
      .getOrElse{
        val uppers = label.fqn.filter(_.isUpper).map(_.toLower)
        val lowers = label.fqn.filter(_.isLower)
          (uppers ++ lowers).take(2).mkString("")
      }
  }
  def abbrevFor(l: Label): Option[String] = {
    if (label == l) {
      Some(getAbbrev())
    } else children.flatMap(_.abbrevFor(l)).headOption
  }

  def allLabels(): List[Label] = label +: children.flatMap(_.allLabels())

  def childLabelsFor(l: Label): List[Label] = {
    if (label==l) {
      children.map(_.label)
    } else {
      children.flatMap(_.childLabelsFor(l))
    }
  }

  def apply(append: List[LabelSchema]): LabelSchema = {
    this.copy(
      children = children ++ append
    )
  }
}

object LabelSchema {
  def apply(label: Label): LabelSchema = {
    LabelSchema(label, None, None, List())
  }
  def apply(label: Label, desc: String): LabelSchema = {
    LabelSchema(label, None, Some(desc), List())
  }

  implicit val Encode_LabelSchema: Encoder[LabelSchema] = deriveEncoder
  implicit val Decode_LabelSchema: Decoder[LabelSchema] = deriveDecoder

}


@JSExportTopLevel("watr.watrmarks.LabelSchemas")
case class LabelSchemas(
  schemas: List[LabelSchema]
) {

  val allLabels: List[String] = {
    schemas.flatMap(_.allLabels()).map(_.fqn)
  }

  def topLabels(): List[String] = {
    schemas.map(_.label.fqn)
  }

  def abbrevFor(label: Label): String = {
    schemas.flatMap(_.abbrevFor(label)).headOption.getOrElse("")
  }

  def childLabelsFor(label: Label): List[String] = {
    schemas.flatMap(_.childLabelsFor(label))
      .map(_.fqn)
  }
}

@JSExportTopLevel("watr.watrmarks.LabelSchemasCompanion")
@JSExportAll
object LabelSchemas {
  def labelSchemaToBox(schema: LabelSchemas): TB.Box = {

    def renderSchema(s: LabelSchema): TB.Box = {
      val lbox = s.getAbbrev.box + ": " + s.label.fqn.box

      if (s.children.nonEmpty) {
        lbox atop indent(4,
          vcat(left, s.children.map(renderSchema(_)))
        )
      } else { lbox }
    }

    vjoin(left,
      "Label Schema", indent(4,
        vjoins(
          schema.schemas.map(renderSchema(_))
        ))
    )
  }

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

  val testLabelSchema = {

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
        LabelSchema(Affiliation, None, None, List(
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

  implicit val Encode_LabelSchemas: Encoder[LabelSchemas] = deriveEncoder
  implicit val Decode_LabelSchemas: Decoder[LabelSchemas] = deriveDecoder
}
