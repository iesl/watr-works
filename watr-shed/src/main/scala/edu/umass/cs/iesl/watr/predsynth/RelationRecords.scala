package edu.umass.cs.iesl.watr
package predsynth



import utils.{StringUtils => SU}
//import TypeTags._


trait Identities {
  import shapeless._

  type C = Int @@ ClusterID
  type M = Int @@ MentionID

  type Entity = C :+: M :+: CNil

  object write extends Poly1 {
    implicit val caseClusterID     = at[C](i => s"ClusterID:$i")
    implicit val caseMentionID     = at[M](i => s"MentionID:$i")
  }

  object idVal extends Poly1 {
    implicit def default[A, T]  = at[A @@ T](_.unwrap)
  }

  def read(str: String): Either[String,Entity] = {
    val Array(tagType, id) = str.split(":")
    tagType match {
      case "ClusterID" => Right(cluster(ClusterID(id.toInt)))
      case "MentionID" => Right(mention(MentionID(id.toInt)))
      case _ =>  Left(s"error unpacking tag type ${str}")
    }
  }

  def cluster(c: C): Entity = {
    Coproduct[Entity](c)
  }

  def mention(m: M): Entity = {
    Coproduct[Entity](m)
  }


  def idValue(e: Entity): Int = {
    e.map(idVal).unify
  }
}

object Identities extends Identities

object Prop {
  import play.api.libs.json._
  import Identities._

  case class Value(jsval: JsValue)

  case class PropKV(
    key: String,
    value: Value
  )

  def formatValue(prop: Value): String = prop.jsval match {
    case JsString(v)    => v.toString
    case JsBoolean(v)   => SU.dquote(v.toString)
    case JsNumber(v)    => v.toString
    case _              => sys.error("")
  }

  case class PropRec(
    propHolder: Entity,
    prop: PropKV
  )

  def formatPropRec(propRec: PropRec): String = {
    val e = propRec.propHolder.map(write).unify
    val k = propRec.prop.key
    val v = formatValue(propRec.prop.value)
    s"""["$e", "$k", $v]"""
  }

  def Str(s: String): Value = Value(JsString(s))

}


object Relation {
  import Identities._

  sealed trait RelationRec
  sealed trait RelationPartial extends RelationRec

  case class Record(
    lhs: Entity,
    relationship: String,
    rhs: Entity
  ) extends RelationRec

  case class LeftPartial(
    lhs: Entity,
    relationship: String
  ) extends RelationPartial

  case class RightPartial(
    relationship: String,
    rhs: Entity
  ) extends RelationPartial

}
