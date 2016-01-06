package edu.umass.cs.iesl.watr
package watrmarks

import textboxing.TextBoxing

sealed trait BioPin {
  def label: BioLabel
  def pinChar: Char
  override def toString = s"<${pinChar}::${label}>"

  def showBox: TB.Box = {
    s"${label.name.take(3).mkString}.${pinChar}"
  }
}

case class BPin(label: BioLabel, override val pinChar:Char='B') extends BioPin
case class IPin(label: BioLabel, override val pinChar:Char='I') extends BioPin
case class OPin(label: BioLabel, override val pinChar:Char='O') extends BioPin
case class LPin(label: BioLabel, override val pinChar:Char='L') extends BioPin
case class UPin(label: BioLabel, override val pinChar:Char='U') extends BioPin

case class FencePost(label: BioLabel, override val pinChar:Char='F') extends BioPin
// private val hash: java.util.UUID = java.util.UUID.randomUUID()

// import StandardLabels._
// case class Constraint()
// class BioLabel(val namespace: String, val name: String, val c: Char, val constraint: Constraint) {


import scala.language.dynamics

class BioLabel(val namespace: String, val name: String, val c: Char, val constraint: Option[BioLabel]) extends Dynamic {
  lazy val B = BPin(this)
  lazy val I = IPin(this)
  lazy val O = OPin(this)
  lazy val L = LPin(this)
  lazy val U = UPin(this)

  def fencePost = FencePost(this)


  def selectDynamic(s: String) = {
  // def selectDynamic(s: String)(implicit bioDict: BioLabelDictionary) = {
    // val sdf  = bioDict.get(s)
    this
  }
  override def toString = s"${namespace}:${name}"

  import TextBoxing._

  def showBox: TextBoxing.Box = {
    s"${namespace}:${name}".box
  }
}



trait BioLabelDictionary {
  def apply(c: Char): BioLabel
  def get(s: Char): Option[BioLabel]

  def apply(s: String): BioLabel
  def get(s: String): Option[BioLabel]
}

case class BioDictionary(
  byName: Map[String, BioLabel],
  byChar: Map[Char, BioLabel]
) extends BioLabelDictionary {

  def apply(s: String) = byName(s)
  def get(s: String): Option[BioLabel] = byName.get(s)

  def apply(s: Char) = byChar(s)
  def get(s: Char): Option[BioLabel] = byChar.get(s)
}


object BioLabel {

  def apply(ns: String, name: String, c: Char, constraint: BioLabel) =
    new BioLabel(ns, name, c, Some(constraint))

  def apply(ns: String, name: String) =
    new BioLabel(ns, name, name(0), Some(CharLabel))

  def apply(name: String) =
    new BioLabel(name, name, name(0), Some(CharLabel))

}
