package edu.umass.cs.iesl.watr
package segment

import spindex._
import segment.{SegmentationLabels => LB}
import textgrid._


trait PageLevelFunctions extends ColumnFinding
    with TextReconstruction
    with TextBlockGrouping
    with ShapeFunctions

object PageSegmenter {

  def getVisualLinesInExtractionOrder(pageIndex: PageIndex): Seq[LineShape] = {
    // pageIndex.shapes.getOrdering(LB.ExtractedLineStarts)
    ???
  }

  // def getVisualLinesInReadingOrder(pageIndex: PageIndex): Seq[(Component, Seq[Component])] = {
  def getVisualLinesInReadingOrder(pageIndex: PageIndex): Seq[LineShape] = {
    // val linesPerBlock0 = for {
    //   block <- pageIndex.components.getOrdering(LB.ReadingBlocks)
    // } yield {
    //   for {
    //     visualLineRootCCs <- pageIndex.components.getRelation(block, LB.HasVisualLines).toList
    //     // visualLineRootCC <- visualLineRootCCs
    //     lineMembers   <- pageIndex.getClusterMembers(LB.ReadingBlockLines, visualLineRootCC)
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

  def getPageTextGrid(): TextGrid = {
    textReconstruction.getTextGrid()
  }

  def runPageSegmentationPass1(): Unit =  {
    markNatLangText()

    // findContiguousBlocks(LB.CharRunFontBaseline)

    // createColumnClusters()

  }

  def runPageSegmentationPass2(): Unit =  {

    // lineFinding.runLineSegmentation()
    // shapeFunctions.buildLinePairTrapezoids()

  }

  def runLineClassification(): Unit = {
    shapeFunctions.classifyLines()
  }

}
