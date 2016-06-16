package edu.umass.cs.iesl.watr
package watrmarks


import scala.collection.mutable
import textboxing.{TextBoxing => TB}
import scalaz.@@
import TypeTags._

import spindex._

sealed trait BioPin {
  def label: Label
  def pinChar: Char
  override def toString = s"<${pinChar}::${label}>"

  def showBox: TB.Box = {
    s"${label.key.take(3).mkString}.${pinChar}"
  }

  def isBegin: Boolean = false
  def isInside: Boolean = false
  def isOutSide: Boolean = false
  def isLast: Boolean = false
  def isUnit: Boolean = false

  def id: Int@@LabelID
}

case class BPin(
  label: Label,
  override val id: Int@@LabelID=LabelID(0)
) extends BioPin {
  override val pinChar:Char='B'
  override val isBegin:Boolean=true
}

case class IPin(
  label: Label,
  override val id: Int@@LabelID=LabelID(0)
) extends BioPin {
  override val isInside:Boolean=true
  override val pinChar:Char='I'
}

case class OPin(
  label: Label,
  override val id: Int@@LabelID=LabelID(0)
) extends BioPin {
  override val pinChar:Char='O'
  override val isOutSide:Boolean=true
}

case class LPin(
  label: Label,
  override val id: Int@@LabelID=LabelID(0)
) extends BioPin {
  override val isLast:Boolean=true
  override val pinChar:Char='L'
}

case class UPin(
  label: Label,
  override val id: Int@@LabelID=LabelID(0)
) extends BioPin {
  override val isUnit:Boolean=true
  override val pinChar:Char='U'
}


case class BioNode(
  component: Component,
  pins: mutable.Set[BioPin] =  mutable.Set()
)


case class Label(ns: String, key: String, value: Option[String]=None) {

  def B(id: Int@@LabelID=LabelID(0)): BioPin = BPin(this, id)
  def I(id: Int@@LabelID=LabelID(0)): BioPin = IPin(this, id)
  def O(id: Int@@LabelID=LabelID(0)): BioPin = OPin(this, id)
  def L(id: Int@@LabelID=LabelID(0)): BioPin = LPin(this, id)
  def U(id: Int@@LabelID=LabelID(0)): BioPin = UPin(this, id)

  def apply(value: String) = copy(value=Some(value))

  def fqn: String = {
    if (ns.length()>0) {
      s"""${ns}:${key}"""
    } else {
      key
    }
  }

  override def toString = s"${ns}:${key}"

  import TB._

  def showBox: Box = {
    s"${ns}:${key}".box
  }

  def matches(l: Label) =
    ns==l.ns && key==l.key
}


trait LabelDictionary {
  def apply(c: Char): Label
  def get(s: Char): Option[Label]

  def apply(s: String): Label
  def get(s: String): Option[Label]
}

case class BioDictionary(
  byName: Map[String, Label],
  byChar: Map[Char, Label]
) extends LabelDictionary {

  def apply(s: String) = byName(s)
  def get(s: String): Option[Label] = byName.get(s)

  def apply(s: Char) = byChar(s)
  def get(s: Char): Option[Label] = byChar.get(s)
}
