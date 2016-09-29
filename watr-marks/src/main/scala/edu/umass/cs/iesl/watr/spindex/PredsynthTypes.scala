package edu.umass.cs.iesl.watr
package spindex

trait PredsynthPaperAnnotationTypes extends TypeTagFormats {
  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  implicit def optionalFormat[T](implicit jsFmt: Format[T]): Format[Option[T]] =
    new Format[Option[T]] {
      override def reads(json: JsValue): JsResult[Option[T]] = json match {
        case JsNull => JsSuccess(None)
        case js     => jsFmt.reads(js).map(Some(_))
      }
      override def writes(o: Option[T]): JsValue = o match {
        case None    => JsNull
        case Some(t) => jsFmt.writes(t)
      }
    }

  implicit def formatModified: Format[Modified] =
    new Format[Modified] {
      override def reads(json: JsValue): JsResult[Modified] = json match {
        case _ => JsSuccess(Modified(0))
      }
      override def writes(o: Modified): JsValue = JsNull
    }

  implicit def Format_RawText          =  Json.format[RawText]
  implicit def Format_Apparatus        =  Json.format[Apparatus]
  implicit def Format_Condition        =  Json.format[Condition]
  implicit def Format_Amount           =  Json.format[Amount]
  implicit def Format_EntityDescriptor =  Json.format[EntityDescriptor]
  implicit def Format_Operation        =  Json.format[Operation]
  implicit def Format_Connection       =  Json.format[Connection]
  implicit def Format_Paragraph        =  Json.format[Paragraph]
  implicit def Format_Entity           =  Json.format[Entity]
  implicit def Format_Paper            =  Json.format[Paper]

}

case class Modified(n: Int)

case class Entity(
  _id : Option[String],
  is_target: Option[Boolean],
  raw_texts : Seq[Option[RawText]],
  amounts: Seq[Option[Amount]],
  entdescriptors: Seq[Option[EntityDescriptor]],
  x: Double, y: Double
)

case object Missing

case class Apparatus(raw_text : RawText)
case class Condition(raw_text : RawText)
case class Amount(raw_text : RawText)
case class EntityDescriptor(raw_text: RawText)

case class RawText(
  end_char_index : Option[Int],
  start_char_index : Option[Int],
  raw_text : String,
  paragraph_id : Option[String]
)

case class Operation(
  _id : String,
  raw_texts : Seq[RawText],
  apparatuses: Seq[Option[Apparatus]],
  conditions : Seq[Option[Condition]],
  x: Double, y: Double,
  order :Int
)

case class Paper(
  doi : String,
  title : String,
  plaintext : Option[String],
  `abstract` :  Option[String],
  annotator : Option[String],
  modified : Modified,
  pdf_loc: String,
  tags : Seq[String],
  connections : Seq[Option[Connection]],
  entities: Seq[Option[Entity]],
  paragraphs: Seq[Paragraph]
)

case class Paragraph(
  _id : String,
  text: String,
  is_recipe: Boolean
)

case class Connection(
  x1 : Double, x2 : Double,
  y1 : Double, y2 : Double,
  id1 : Option[String], id2 : Option[String],
  paper_dois : Option[Seq[String]]
)



import ammonite.{ops => fs}
import fs._
import java.nio.{file => nio}
import play.api.libs.json
import play.api.libs.json._
import play.api.data.validation.ValidationError

object PredsynthLoad extends PredsynthPaperAnnotationTypes {
  def main(args: Array[String]) = {

    val jsonPath = pwd / RelPath(args(0))
    val fis = nio.Files.newInputStream(jsonPath.toNIO)
    val papers = json.Json.parse(fis).validate[Seq[Paper]]

    papers.fold(
      (errors: Seq[(JsPath, Seq[ValidationError])]) => {
        println(s"errors: ${errors.length}")

        errors.take(10).foreach { case (errPath, errs) =>
          println(s"$errPath")
          errs.foreach { e =>
            println(s"> $e")
          }
        }

      }, (ps: Seq[Paper]) => {
        println("load went ok.")


        ps.foreach { paper =>

          val paras = paper.paragraphs.map({ para =>
            para._id -> para
          }).toMap

          println(s"\n\nPaper: ${paper.doi}, ${paper.title}")
          paper.entities.flatten.foreach{ entity =>
            println(s"Entity: ${entity._id}")
            val rawTexts = entity.raw_texts.flatten

            rawTexts.foreach { rawText =>
              val pId = rawText.paragraph_id.getOrElse { "" }
              val start = rawText.start_char_index.getOrElse { 0 }
              val end = rawText.end_char_index.getOrElse { 0 }
              val rtext = rawText.raw_text
              println(s"seeking raw_text: ${rtext}")

              paras.get(pId) match {
                case Some(para) =>
                  val substr = para.text.substring(start, end)
                  if (rtext == substr) {
                    println(s"id matched raw_text: ${substr}")
                  } else {
                    val matchingPara = paper.paragraphs.find(p =>
                      end < p.text.length() &&
                      p.text.substring(start, end) == rtext
                    )
                    matchingPara match {
                      case Some(p) =>
                        println(s"(1) scan matched raw_text: ${rtext}")
                      case None =>
                        println(s"(1) no matching para for ${rtext}")
                        var found = false
                        paper.paragraphs.foreach(p =>
                          if (p.text.indexOf(rtext) > -1) {
                            val i = p.text.indexOf(rtext)
                            val (imin, imax) = (math.max(0, i-15), math.min(i+rtext.length()+20, p.text.length()))
                            val context = p.text.substring(imin, imax)
                            println(s"(1) para:${p._id}, found substring near: ${context}")
                            found = true
                          }
                        )
                        if (!found) { println(s"xx-nope") }
                    }
                  }
                case None =>
                  val matchingPara = paper.paragraphs.find(p =>
                    end < p.text.length() &&
                    p.text.substring(start, end) == rtext
                  )
                  matchingPara match {
                    case Some(p) =>
                      println(s"(2) scan matched raw_text: ${rtext}")
                    case None =>
                      println(s"(2) no matching para ")
                      var found = false
                      paper.paragraphs.foreach(p =>
                        if (p.text.indexOf(rtext) > -1) {
                          val i = p.text.indexOf(rtext)
                          val (imin, imax) = (math.max(0, i-15), math.min(i+rtext.length()+20, p.text.length()))
                          val context = p.text.substring(imin, imax)
                          println(s"(2) para:${p._id}, found substring near: ${context}")
                          found = true
                        }
                      )
                      if (!found) { println(s"xx-nope") }
                  }

              }
            }

            val amounts = entity.amounts.flatten
            val entDescs = entity.entdescriptors.flatten
            // println(s"""  raw text> ${rawTexts.mkString(", ")}""")
          }



        }

      }
    )


  }
}
