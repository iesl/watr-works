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

  // TODO rank ordering should be wrt (document, role)
  case class Zone(
    prKey    : Int@@ZoneID,
    document : Int@@DocumentID,
    label    : Int@@LabelID,
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


  case class Person(
    prKey   : Int@@UserID,
    email   : String@@EmailAddr
  )

  case class WorkflowDef(
    workflow     : String@@WorkflowID,
    description  : String
  )

  case class LockGroup(
    id: Int@@LockGroupID,
    user: Int@@UserID
  )

  case class ZoneLock(
    id         : Int@@ZoneLockID,
    group      : Option[Int@@LockGroupID],
    zone       : Int@@ZoneID,
    status     : String@@StatusCode
  )


}
