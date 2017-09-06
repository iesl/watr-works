package edu.umass.cs.iesl.watr
package segment

import scalaz.{@@ => _, _}
import Scalaz._
import Free.Trampoline

import edu.umass.cs.iesl.watr.corpora.DocumentZoningApi
import edu.umass.cs.iesl.watr.tracing.VisualTracer
import spindex._
// import ammonite.{ops => fs}, fs._

case class SegReadOnly(
)

// case class SegWriteOnly()

case class SegState(
  mpageIndex: MultiPageIndex,
  tracer: VisualTracer,
  docStats: DocumentLayoutStats,
  pageSegState: Option[PageSegState]
)

case class PageSegState(
  pageId: Int@@PageID,
  pageNum: Int@@PageNum
)

trait SegmenterCommon {

  def docStore: DocumentZoningApi
  def stableId: String@@DocumentID
  def docId: Int@@DocumentID

}

trait SegmentationSystem extends SegmenterCommon {

  type SegFunc[A] = ReaderWriterStateT[Trampoline, SegReadOnly, Unit, SegState, A]

  // type PageSegFunc[A] = ReaderWriterStateT[Trampoline, SegReadOnly, Unit, PageSegState, A]

  def rwstMonad[W : Monoid] = ReaderWriterStateT.rwstMonad[Trampoline, SegReadOnly, Unit, SegState]

  protected val rle = rwstMonad[Unit]


}
