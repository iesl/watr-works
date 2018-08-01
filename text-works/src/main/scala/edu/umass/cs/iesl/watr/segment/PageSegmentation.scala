package edu.umass.cs.iesl.watr
package segment

import rtrees._
import textgrid._

trait PageLevelFunctions extends ColumnFinding
    with TextReconstruction
    with TextBlockGrouping
    with ShapeFunctions
    with ReferenceBlockConverter

object PageSegmenter {
  import SegmentationSystem._

  def getVisualLinesInExtractionOrder(shapeIndex: ShapeIndex): Seq[LineShape] = {
    // shapeIndex.getOrdering(LB.ExtractedLineStarts)
    ???
  }

  // def getVisualLinesInReadingOrder(shapeIndex: LabeledShapeIndex): Seq[(Component, Seq[Component])] = {
  def getVisualLinesInReadingOrder(shapeIndex: ShapeIndex): Seq[LineShape] = {
    // val linesPerBlock0 = for {
    //   block <- shapeIndex.components.getOrdering(LB.ReadingBlocks)
    // } yield {
    //   for {
    //     visualLineRootCCs <- shapeIndex.components.getRelation(block, LB.HasVisualLines).toList
    //     // visualLineRootCC <- visualLineRootCCs
    //     lineMembers   <- shapeIndex.getClusterMembers(LB.ReadingBlockLines, visualLineRootCC)
    //   } yield {
    //     (block, lineMembers)
    //   }
    // }

    // val linesPerBlock = linesPerBlock0.flatten

    // linesPerBlock
    ???
  }

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

trait PageSegmenter extends PageLevelFunctions {

  def runLineClassification(): Unit = {
    shapeFunctions.classifyLines()
  }

}
