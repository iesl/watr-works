package edu.umass.cs.iesl.watr
package predsynth

//import TypeTags._

import ammonite.{ops => fs}, fs._
import java.nio.{file => nio}
import play.api.libs.json, json._
import play.api.data.validation.ValidationError
import geometry._
import GeometricFigure.Point

trait DocsegJsonFormats extends PredsynthJsonFormats with ExplicitTypeTagFormats {
  private[this] val log = org.log4s.getLogger

  import play.api.libs.json._
  import Prop._
  import Docseg._
  import Json.format

  implicit def Format_RelationEntity: Format[Identities.Entity] = new Format[Identities.Entity] {
    override def reads(json: JsValue): JsResult[Identities.Entity] = json match {
      case JsString(str) =>
        Identities.read(str).fold(
          JsError(_),
          JsSuccess(_))

      case _ => ???
    }

    override def writes(o: Identities.Entity): JsValue = JsString(
      o.map(Identities.write).unify
    )
  }

  // implicit def Format_TextPosition   = format[TextPosition]
  // implicit def Format_Relation   = format[Relation.Record]
  implicit def Format_Relation: Format[Relation.Record] = new Format[Relation.Record] {
    override def reads(json: JsValue): JsResult[Relation.Record] = json match {
      case JsArray(Seq(
        ent1:JsString, rel: JsString, ent2: JsString
      )) =>
        (ent1.validate[Identities.Entity], ent2.validate[Identities.Entity]) match {
          case (JsSuccess(e1, p1), JsSuccess(e2, p2)) =>
            JsSuccess(Relation.Record(e1, rel.value, e2))
          case _ => JsError("")
        }

      case _ => JsError("")
    }

    override def writes(o: Relation.Record): JsValue = {
      JsArray(Seq())
    }
  }

  implicit def Format_PropValue       =  Json.format[Prop.Value]


  implicit def Format_PropKV  =   format[Prop.PropKV]


  implicit def Format_PropRec: Format[Prop.PropRec] = new Format[Prop.PropRec] {
    override def reads(json: JsValue): JsResult[Prop.PropRec] = json match {
      case JsArray(Seq(
        ident:JsString, key: JsString, value: JsValue
      )) =>
        ident.validate[Identities.Entity].map{id =>
          PropRec(id, PropKV(key.value, Prop.Value(value)))
        }

      case _ => ???
    }

    override def writes(o: Prop.PropRec): JsValue = {
      val e = o.propHolder.map(Identities.write).unify
      val k = o.prop.key

      JsArray(Seq(JsString(e), JsString(k), o.prop.value.jsval))
    }
  }



  implicit def Format_Mention: Format[Mention] = new Format[Mention] {
    override def reads(json: JsValue): JsResult[Mention] = json match {
      case JsArray(Seq(
        mentionId:JsNumber,
        clusterId:JsNumber,
        role: JsString,
        text: JsString,
        JsArray(textPositions))
      ) =>

        val tpos = textPositions
          .map(_.validate[TextPosition].get)
          .toList

        JsSuccess(
          Mention(
            MentionID(mentionId.value.toInt),
            ClusterID(clusterId.value.toInt),
            role.value,
            text.value,
            tpos
          ))
      case _ =>
        JsError(s"format error in id definition: ${json}")


    }
    override def writes(o: Mention): JsValue = JsNull
  }

  implicit def Format_Docseg         =  format[Docseg]

  // implicit def Format_ClusterID = FormatTaggedInt[ClusterID]
  // implicit def Format_PageID    = FormatTaggedInt[PageID]
  // implicit def Format_Offset    = FormatTaggedInt[Offset]
  // implicit def Format_LabelID   = FormatTaggedInt[LabelID]

  // implicit def Format_Point: Format[Point] = new Format[Point] {
  //   val pgen = Generic[Point]
  //   override def reads(json: JsValue)       = json.validate[pgen.Repr].map(pgen.from(_))
  //   override def writes(o: Point): JsValue  = ???
  // }

  // implicit def Format_Relation: Format[Relation] = new Format[Relation] {
  //   val pgen = Generic[Relation]
  //   override def reads(json: JsValue) = json.validate[pgen.Repr].map(pgen.from(_))
  //   override def writes(o: Relation): JsValue = JsNull
  // }


  // implicit def Format_Cluster        =  Json.format[Cluster]


  // // implicit  Format_PropRec0       =  derived.reads[PropRec]
  // // implicit def Format_PropRec1       =  derived.owrites[PropRec]

  // // implicit lazy val Format_PropRec0       =  derived.reads[PropRec]
  // implicit lazy val Format_PropRec       =  derived.oformat[PropRec]

  // // implicit def Format_Property: Format[PropRec] = new Format[PropRec] {
  // //   val pgen = Generic[PropRec]
  // //   override def reads(json: JsValue) = json.validate[pgen.Repr].map(pgen.from(_))
  // //   override def writes(o: PropRec): JsValue = JsNull
  // // }

  // // implicit def Format_PropRec: Format[PropRec] = new Format[PropRec] {
  // //   override def reads(json: JsValue): JsResult[PropRec] = json match {
  // //     case JsArray(Seq(
  // //       ident:JsString, key: JsString, value: JsValue
  // //     )) =>
  // //       ident.validate[Int@@Identity]
  // //       // value.validate[]

  // //       // JsSuccess(
  // //       //   PropRec(
  // //       //     indent.value
  // //       //   )
  // //       // )
  // //       ???
  // //     case _ =>
  // //       JsError(s"format error in id definition: ${json}")
  // //   }
  // //   override def writes(o: PropRec): JsValue = JsNull
  // // }


  // implicit def Format_LineDef        =  Json.format[LineDef]
  // implicit def Format_TargetPosition =  Json.format[TargetPosition]

  implicit def Format_Label: Format[Label] = new Format[Label] {
    override def reads(json: JsValue): JsResult[Label] = json match {
      case JsArray(Seq(
        role:JsString, lineNums: JsArray, labelId: JsNumber
      )) =>
        JsSuccess(
          Label(
            role.as[String],
            lineNums.as[List[Int]].map(i => Offset(i)),
            LabelID(labelId.as[Int])
          ))
      case _ =>
        JsError(s"format error in id definition: ${json}")
    }
    override def writes(o: Label): JsValue = JsNull
  }

  implicit def Format_IdDef: Format[IdDef] = new Format[IdDef] {
    override def reads(json: JsValue): JsResult[IdDef] = json match {
      case JsArray(Seq(
        charId:JsNumber, JsArray(Seq(
          pageNumber:JsNumber, JsArray(Seq(
            llX: JsNumber,
            llY:JsNumber
          )))))) =>

        JsSuccess(
          IdDef(
            charId.value.toIntExact,
            TargetPosition(
              PageID(pageNumber.value.toIntExact),
              Point(
                llX.value.toDouble,
                llY.value.toDouble
              ))))
      case _ => JsError(s"format error in id definition: ${json}")
    }
    override def writes(o: IdDef): JsValue = JsNull
  }

  implicit def Format_TextPosition: Format[TextPosition] = new Format[TextPosition] {
    override def reads(json: JsValue): JsResult[TextPosition] = json match {
      case JsArray(Seq(
        lineNum:JsNumber,
        charOffset:JsNumber,
        length:JsNumber
      )) =>
        JsSuccess(
          TextPosition(
            Offset(lineNum.value.toInt),
            Offset(charOffset.value.toInt),
            Length(length.value.toInt)
          ))

      case _ =>
        JsError(s"format error in id definition: ${json}")


    }
    override def writes(o: TextPosition): JsValue = JsNull
  }


}


object Docseg extends DocsegJsonFormats {
  private[this] val log = org.log4s.getLogger

  case class TextPosition(
    lineNumber: Int@@Offset,
    charBegin: Int@@Offset,
    length: Int@@Length
  )

  case class Mention(
    mentionID: Int@@MentionID,
    clusterID: Int@@ClusterID,
    role: String,
    text: String,
    positions: List[TextPosition]
  )

  case class Cluster(
    clusterID: Int@@ClusterID,
    role: String
  )

  case class Label(
    role: String,
    lines: List[Int@@Offset],
    labelId: Int@@LabelID
  )

  case class LineDef(
    line: List[Int]
  )

  case class TargetPosition(
    pageId: Int@@PageID,
    lowerLeft: Point
  )

  case class IdDef(
    id: Int,
    targetPosition: TargetPosition
  )

  case class Docseg(
    lines: List[String],
    mentions: List[Mention],
    relations: List[Relation.Record],
    properties: List[Prop.PropRec],
    labels: List[Label],
    lineDefs: List[List[Int]],
    ids: List[IdDef]
  )


  def read(path: Path): Option[Docseg] = {
    log.trace(s"reading Docseg ${path}")

    val fis = nio.Files.newInputStream(path.toNIO)
    val paper = json.Json.parse(fis).validate[Docseg]

    paper.fold(
      (errors: Seq[(JsPath, Seq[ValidationError])]) => {
        println(s"errors: ${errors.length}")

        errors.take(10).foreach { case (errPath, errs) =>
          println(s"$errPath")
          errs.foreach { e =>
            println(s"> $e")
          }
        }
        None

      }, (docseg: Docseg) => {
        Option(docseg)
      }
    )
  }


  def main(args: Array[String]): Unit = {
    val loadPath = pwd / RelPath(args(0))
    val docseg = read(loadPath)
    docseg.foreach { d =>
      println(d)
    }
  }

}
