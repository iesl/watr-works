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

  case class Person(
    prKey     : Int@@UserID,
    email     : String@@EmailAddr
  )

  case class WorkflowDef(
    workflow      : String@@WorkflowID,
    labelSchemas  : LabelSchemas,
    targetPath    : String@@CorpusPath,
  )

  case class CorpusLock(
    id         : Int@@LockID,
    holder     : Option[Int@@UserID],
    document   : Int@@DocumentID,
    lockPath   : String@@CorpusPath,
    status     : String@@StatusCode
  )

  case class AnnotationRec(
    id         : Int@@AnnotationID,
    document   : Int@@DocumentID,
    owner      : Option[Int@@UserID],
    annotPath  : Option[String@@CorpusPath],
    created    : java.time.Instant,
    body       : Option[String]
  )


}
