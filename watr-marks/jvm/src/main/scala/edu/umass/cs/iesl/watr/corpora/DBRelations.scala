package edu.umass.cs.iesl.watr
package corpora

import edu.umass.cs.iesl.watr.{geometry => G}

object RelationModel {

  case class Document(
    prKey   : Int@@DocumentID,
    stableId: String@@DocumentID
  )

  case class Page(
    prKey      : Int@@PageID,
    document   : Int@@DocumentID,
    pagenum    : Int@@PageNum,
    imageclip  : Option[Int@@ImageID],
    bounds     : G.LTBounds
  )

  case class TargetRegion(
    prKey      : Int@@RegionID,
    page       : Int@@PageID,
    rank       : Int,
    imageclip  : Option[Int@@ImageID],
    bounds     : G.LTBounds
  )

  case class ImageClip(
    prKey    : Int@@ImageID,
    image    : Array[Byte]
  )

  case class Zone(
    prKey    : Int@@ZoneID,
    document : Int@@DocumentID,
    rank     : Int
  )

  case class ZoneTree(
    prKey    : Int@@ZoneID,
    document : Int@@DocumentID,
    parent   : Option[Int@@ZoneID],
    label    : Option[Int@@LabelID],
    region   : Option[Int@@RegionID],
    rank     : Int
  )

  case class TextReflow(
    prKey     : Int@@TextReflowID,
    reflow    : String,
    astext    : String,
    zone      : Int@@ZoneID
  )

  case class Label(
    prKey : Int@@LabelID,
    key   : String
  )

  case class LabelingWidget(
    prKey   : Int@@LabelerID,
    widget  : String
  )

  case class LabelingTask(
    prKey     : Int@@LabelingTaskID,
    taskName  : String,
    taskState : String,
    assignee  : String
  )

}
