package org.watrworks
package segment

trait PageLevelFunctions extends ColumnFinding
    with TextReconstruction
    with GlyphRuns
    with TextBlockGrouping
    with ShapeFunctions
    with ReferenceBlockConverter

object PageSegmenter {

  def apply(
    pageNum0: Int@@PageNum,
    documentSegmenter0: DocumentScopeSegmenter
  ): PageSegmenter = new PageSegmenter {

    override val docScope: DocumentScopeSegmenter = documentSegmenter0

    override val pageNum: Int@@PageNum = pageNum0
    override val pageStats: PageLayoutStats = new PageLayoutStats()

  }
}

trait PageSegmenter extends PageLevelFunctions
