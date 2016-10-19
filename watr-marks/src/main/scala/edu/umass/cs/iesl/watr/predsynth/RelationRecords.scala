package edu.umass.cs.iesl.watr
package predsynth

import scalaz.@@


sealed trait Relation

object Relation {
  sealed trait RecNode
  sealed trait Elem extends RecNode
  sealed trait Prop extends RecNode

  object Elem {
    case class Mention(v: Int@@MentionID) extends Elem
    case class Cluster(v: Int@@ClusterID) extends Elem
    case class Relation(v: Int@@RelationID) extends Elem
  }

  object Prop {
    case class Bool(v: Boolean) extends Prop
    case class Str(v: String) extends Prop
    case class Num(v: Int) extends Prop
  }

  sealed trait RelationRec
  sealed trait RelationPartial extends RelationRec
  sealed trait Property extends RelationRec

  case class Record(
    id: Int@@RelationID,
    lhs: Elem,
    relationship: String,
    rhs: Elem
  ) extends RelationRec

  case class LeftPartial(
    lhs: Elem,
    relationship: String
  ) extends RelationPartial

  case class RightPartial(
    relationship: String,
    rhs: Elem
  ) extends RelationPartial

  case class PropKV(
    key: String,
    value: Prop
  )

  case class PropRec(
    elem: Elem,
    prop: PropKV
  )

  def formatElem(e: RecNode): String = e match {
    case Elem.Mention(v)  => s"""mention:$v"""
    case Elem.Cluster(v)  => s"""cluster:$v"""
    case Elem.Relation(v) => s"""relation:$v"""
    case Prop.Bool(v)     => s"""$v"""
    case Prop.Str(v)      => '"'+ v +'"'
    case Prop.Num(v)      => v.toString
  }

  def formatPropRec(propRec: PropRec): String = {
    val e = formatElem(propRec.elem)
    val k = propRec.prop.key
    val v = formatElem(propRec.prop.value)
    s"""[$e, "$k", $v]"""
  }


}
