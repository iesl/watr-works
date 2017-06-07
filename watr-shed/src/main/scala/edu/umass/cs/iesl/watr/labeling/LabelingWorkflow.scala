package edu.umass.cs.iesl.watr
package labeling

import watrmarks.Label
import scalaz.Tag
// import corpora._
import labeling.data._

// This is a replacement for document labeler identifier
case class LabelWidgetConfig(
  workflow            : String@@WorkflowID
  // zone                : Int@@ZoneID,
  // regions             : Seq[Int@@RegionID],
  // paginationStart     : Int,
  // paginationIncrement : Int
)



trait LabelerBuilder {
  def createLabeler(): (LabelWidget, LabelerIdentifier)
}
