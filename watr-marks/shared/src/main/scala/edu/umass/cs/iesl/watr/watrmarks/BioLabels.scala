package edu.umass.cs.iesl.watr
package watrmarks


//import TypeTags._

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


case class Label(
  ns: String, key: String,
  value: Option[String]=None,
  id: Int@@LabelID=LabelID(0)
) {

  def B: BioPin = BPin(this, id)
  def I: BioPin = IPin(this, id)
  def O: BioPin = OPin(this, id)
  def L: BioPin = LPin(this, id)
  def U: BioPin = UPin(this, id)

  def apply(value: String) = copy(value=Some(value))

  def fqn: String = {
    if (ns.length()>0) {
      s"""${ns}:${key}"""
    } else {
      key
    }
  }

  override def toString = {
    val v = value.map(x => s"=$x").getOrElse("")
    s"${ns}:${key}$v"
  }

  override def hashCode = (ns, key).##

  override def equals(o:Any) = o match {
    case Label(`ns`, `key`, _, _) => true
    case _ => false
  }

  def matches(l: Label) =
    ns==l.ns && key==l.key
}


import utils.IdGenerator
class Labeler() {
  val idGen = IdGenerator[LabelID]()

  def getLabel(l: Label): Label = {
    l.copy(id = idGen.nextId)
  }


}
