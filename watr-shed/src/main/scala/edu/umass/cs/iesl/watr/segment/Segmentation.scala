package edu.umass.cs.iesl.watr
package segment //;import acyclic.file

import spindex._
import extract.images._

case class DocumentSegmentation(
  mpageIndex: MultiPageIndex,
  pageImages: PageImages
)

case class MultiDocSegmentation(
  segs: Map[String@@DocumentID, DocumentSegmentation]
)
