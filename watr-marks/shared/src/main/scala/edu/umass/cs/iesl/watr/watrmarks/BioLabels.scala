package edu.umass.cs.iesl.watr
package watrmarks

import TypeTags._

import textboxing.{TextBoxing => TB}, TB._



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

case class BPin(
  label: Label
) extends BioPin {
  override val pinChar:Char='B'
  override val isBegin:Boolean=true
}

case class IPin(
  label: Label
) extends BioPin {
  override val isInside:Boolean=true
  override val pinChar:Char='I'
}

case class OPin(
  label: Label
) extends BioPin {
  override val pinChar:Char='O'
  override val isOutSide:Boolean=true
}

case class LPin(
  label: Label
) extends BioPin {
  override val isLast:Boolean=true
  override val pinChar:Char='L'
}

case class UPin(
  label: Label
) extends BioPin {
  override val isUnit:Boolean=true
  override val pinChar:Char='U'
}

object Labels {
  def fromString(s: String): Label = {
    Label(s)
  }
}


object Label {
  import io.circe
  import circe._
  import scalaz.{@@ => _, Ordering => _, _} // , Scalaz._

  def auto(implicit name: sourcecode.Name): Label = {
    Label(name.value)
  }

  implicit val Enc_Label: Encoder[Label] = Encoder.encodeString.contramap(_.fqn)
  implicit val Dec_Label: Decoder[Label] = Decoder.decodeString.map(Label(_))

  implicit object LabelShow extends Show[Label] {
    override def shows(f: Label): String = f.fqn
  }
}


case class Label(
  key: String,
  id: Int@@LabelID=LabelID(0)
) {

  def B: BioPin = BPin(this)
  def I: BioPin = IPin(this)
  def O: BioPin = OPin(this)
  def L: BioPin = LPin(this)
  def U: BioPin = UPin(this)


  def fqn: String = {
    key
  }

  override def toString = {
    key
  }

  override def hashCode = (key).##

  override def equals(o:Any) = o match {
    case Label(`key`, _) => true
    case _ => false
  }

  def matches(l: Label) =
    key==l.key

  def qualifiedAs(t: String): Label = {
    Label(s"${fqn}::${t}")
  }

  def qualifiedAs(t: Label): Label = {
    Label(s"${fqn}::${t.fqn}")
  }

  def withNamespace(t: String): Label = {
    Label(s"${t}:${fqn}")
  }

  def /(l: Label): Label = {
    Label(s"${fqn}/${l.fqn}")
  }

}

