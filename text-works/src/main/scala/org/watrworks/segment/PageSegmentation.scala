package org.watrworks
package segment

import rtrees._
import textgrid._

trait PageLevelFunctions extends ColumnFinding
    with TextReconstruction
    with TextBlockGrouping
    with ShapeFunctions
    with ReferenceBlockConverter
    with MarginalMatterDetectionPageScope

object PageSegmenter {
  import SegmentationSystem._

  def apply(
    pageId0: Int@@PageID,
    pageNum0: Int@@PageNum,
    documentSegmenter0: DocumentScopeSegmenter
  ): PageSegmenter = new PageSegmenter {

    override val docScope: DocumentScopeSegmenter = documentSegmenter0

    override val pageId: Int@@PageID = pageId0
    override val pageNum: Int@@PageNum = pageNum0
    override val pageStats: PageLayoutStats = new PageLayoutStats()

  }
}

trait PageSegmenter extends PageLevelFunctions
