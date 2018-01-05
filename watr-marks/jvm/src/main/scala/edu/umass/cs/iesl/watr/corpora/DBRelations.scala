package edu.umass.cs.iesl.watr
package corpora

import edu.umass.cs.iesl.watr.{geometry => G}
import watrmarks._

object RelationModel {

  case class Document(
    prKey   : Int@@DocumentID,
    stableId: String@@DocumentID
  )

  case class Page(
    prKey      : Int@@PageID,
    document   : Int@@DocumentID,
    pagenum    : Int@@PageNum,
    bounds     : G.LTBounds
  )

  case class TargetRegion(
    prKey      : Int@@RegionID,
    page       : Int@@PageID,
    rank       : Int,
    bounds     : G.LTBounds
  )

  // TODO rank ordering should be wrt (document, role)
  case class Zone(
    prKey        : Int@@ZoneID,
    document     : Int@@DocumentID,
    label        : Int@@LabelID,
    rank         : Int,
    glyphs       : Option[String]
  )

  case class Label(
    prKey : Int@@LabelID,
    key   : String
  )


  case class Person(
    prKey     : Int@@UserID,
    email     : String@@EmailAddr
  )

  case class WorkflowDef(
    workflow      : String@@WorkflowID,
    description   : String,
    targetLabel   : Label,
    labelSchemas  : LabelSchemas
  )

  case class ZoneLock(
    id         : Int@@ZoneLockID,
    assignee   : Option[Int@@UserID],
    workflow   : String@@WorkflowID,
    zone       : Int@@ZoneID,
    status     : String@@StatusCode
  )


}
