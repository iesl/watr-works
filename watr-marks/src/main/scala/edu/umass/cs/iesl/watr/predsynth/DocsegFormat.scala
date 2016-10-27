package edu.umass.cs.iesl.watr
package predsynth

import TypeTags._
import edu.umass.cs.iesl.watr.spindex.GeometricFigure.Point
import scalaz.@@

trait DocsegJsonFormats extends PredsynthJsonFormats {
  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  import Docseg._

  implicit def Format_Point          =  Json.format[Point]
  implicit def Format_TextPosition   =  Json.format[TextPosition]
  implicit def Format_Mention        =  Json.format[Mention]
  implicit def Format_Cluster        =  Json.format[Cluster]
  implicit def Format_Relation       =  Json.format[Relation]
  implicit def Format_Property       =  Json.format[Property]
  implicit def Format_Label          =  Json.format[Label]
  implicit def Format_LineDef        =  Json.format[LineDef]
  implicit def Format_TargetPosition =  Json.format[TargetPosition]
  implicit def Format_IdDef          =  Json.format[IdDef]
  implicit def Format_Docseg         =  Json.format[Docseg]
}


object Docseg {

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

  case class Relation(
    lhs: Int@@ClusterID,
    relation: String,
    rhs: Int@@ClusterID
  )

  case class Property(
    owner: Int,
    key: String,
    value: String
  )

  case class Label(
    role: String,
    lines: List[Int@@Offset]
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
    relations: List[Relation],
    properties: List[Property],
    labels: List[Label],
    lineDefs: List[LineDef],
    ids: List[IdDef]
  )

}
