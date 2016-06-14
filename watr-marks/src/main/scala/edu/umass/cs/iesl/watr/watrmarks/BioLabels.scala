package edu.umass.cs.iesl.watr
package watrmarks

import textboxing.{TextBoxing => TB}

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
}

case class BPin(label: Label, override val pinChar:Char='B', override val isBegin:Boolean=true) extends BioPin
case class IPin(label: Label, override val pinChar:Char='I', override val isInside:Boolean=true) extends BioPin
case class OPin(label: Label, override val pinChar:Char='O', override val isOutSide:Boolean=true) extends BioPin
case class LPin(label: Label, override val pinChar:Char='L', override val isLast:Boolean=true) extends BioPin
case class UPin(label: Label, override val pinChar:Char='U', override val isUnit:Boolean=true) extends BioPin

case class Label(ns: String, key: String, value: Option[String]=None) {
  lazy val B = BPin(this)
  lazy val I = IPin(this)
  lazy val O = OPin(this)
  lazy val L = LPin(this)
  lazy val U = UPin(this)

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
