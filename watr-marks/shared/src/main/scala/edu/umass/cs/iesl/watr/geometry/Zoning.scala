package edu.umass.cs.iesl.watr
package geometry

import watrmarks._

case class Zone(
  id: Int@@ZoneID,
  regions: Seq[TargetRegion],
  labels: Seq[Label]
)
