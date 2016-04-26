package edu.umass.cs.iesl.watr

import scalaz.Tag


package object watrmarks {

  import textboxing.TextBoxing
  val TB = TextBoxing


  def boxlf(b: TB.Box): TB.Box =
    TB.emptyBox(1)(0).atop(b)

  implicit class BoxOps(val value: TB.Box) extends AnyVal {
    def padTop1 = boxlf(value)
  }

  implicit class BoxSeqOps(val value: Seq[TB.Box]) extends AnyVal {
    def mkHBox(separator: TB.Box=TB.nullBox) =
      TB.hjoin(sep=separator)(value:_*)

    def mkVBox(separator: TB.Box=TB.nullBox) =
      TB.vjoin(sep=separator)(value:_*)
  }

  val ZoneID = Tag.of[ZoneID]
  val LabelID = Tag.of[LabelID]
  val PageID = Tag.of[PageID]

}
