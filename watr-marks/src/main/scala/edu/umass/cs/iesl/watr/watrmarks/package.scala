package edu.umass.cs.iesl.watr

import scalaz.Tag


package object watrmarks {

  import textboxing.TextBoxing
  val TB = TextBoxing



  val ZoneID = Tag.of[ZoneID]
  val RegionID = Tag.of[RegionID]
  val TokenID = Tag.of[TokenID]
  val PageID = Tag.of[PageID]
  val CharID = Tag.of[CharID]
  val LabelID = Tag.of[LabelID]


}
