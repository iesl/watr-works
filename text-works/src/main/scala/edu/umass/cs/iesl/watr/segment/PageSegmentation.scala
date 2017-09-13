package edu.umass.cs.iesl.watr
package segment

import spindex._
import watrmarks.{StandardLabels => LB}
import textgrid._


trait PageLevelFunctions extends ColumnFinding
    with LineFinding
    with ShapeFunctions


object PageSegmenter {

  def getVisualLinesInReadingOrder(pageIndex: PageIndex): Seq[(Component, Seq[Component])] = {
    val linesPerBlock0 = for {
      block <- pageIndex.getOrdering(LB.ReadingBlocks)
    } yield {
      for {
        visualLineRootCCs <- pageIndex.getRelations(block, LB.HasVisualLines).toList
        visualLineRootCC <- visualLineRootCCs
        lineMembers   <- pageIndex.getClusterMembers(LB.ReadingBlockLines, visualLineRootCC)
      } yield {
        (block, lineMembers)
      }
    }

    val linesPerBlock = linesPerBlock0.flatten

    linesPerBlock
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


  def runPageSegmentation(): Unit = {
    tracer.enter()
    if (traceLog.tracingEnabled()) {
      tracing.VisualTracer.newPage()
    }

    labelImageRegions()

    lineFinding.runLineSegmentation()

    shapeFunctions.buildLinePairTrapezoids()

    tracer.exit()
  }

  def runLineClassification(): Unit = {
    shapeFunctions.classifyLines()

    setPageText()
  }

  private def labelImageRegions(): Unit = {
    mpageIndex.getImageAtoms(pageNum).foreach { imgCC =>
      mpageIndex.labelRegion(Seq(imgCC), LB.Image)
    }
  }

  def setPageText(): Unit = {
    for {
      pageNum      <- mpageIndex.getPages
      pageIndex    <- List(mpageIndex.getPageIndex(pageNum))
    }  {
      val textLines = for {
        (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex)
        (line, n)    <- lineCCs.zipWithIndex
        textRow      <- pageIndex.getComponentText(line, LB.VisualLine).toList
      } yield textRow

      val pageTextGrid = TextGrid.fromRows(textLines)

      docStore.setPageText(pageId, pageTextGrid)
    }
  }
















}
