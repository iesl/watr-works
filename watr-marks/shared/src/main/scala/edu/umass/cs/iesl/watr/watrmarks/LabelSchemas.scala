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

  implicit val Encode_LabelSchemas: Encoder[LabelSchemas] = deriveEncoder
  implicit val Decode_LabelSchemas: Decoder[LabelSchemas] = deriveDecoder
}
