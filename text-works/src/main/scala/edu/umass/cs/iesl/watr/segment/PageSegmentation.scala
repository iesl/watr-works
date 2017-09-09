package edu.umass.cs.iesl.watr
package segment

import spindex._
import watrmarks.{StandardLabels => LB}
import textgrid._


trait PageLevelFunctions extends ColumnFinding
    with LineFinding
    with TrapezoidFinding
    with LineGroupClassification


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

}


class PageSegmenter(
  override val pageId: Int@@PageID,
  override val pageNum: Int@@PageNum,
  documentSegmenter: DocumentScopeSegmenter
) extends SegmentationCommons with PageLevelFunctions {

  val mpageIndex = documentSegmenter.mpageIndex
  val docStats = documentSegmenter.docStats

  val docStore = mpageIndex.docStore
  val stableId = mpageIndex.getStableId
  val docId = docStore.getDocument(stableId)
    .getOrElse(sys.error(s"DocumentSegmenter trying to access non-existent document ${stableId}"))
  val pageStats = docStats.addPage(pageNum)

  val pageIndex = mpageIndex.getPageIndex(pageNum)


  def runLineSegmentation(): Unit = {
    tracer.enter()

    labelImages()

    runLineDeterminationOnPage()

    buildLinePairTrapezoids()

    tracer.exit()
  }

  def runLineClassification(): Unit = {
    // val lineClassifier = new LineGroupClassifier(mpageIndex, pageId, pageNum, tracer)
    lineClassifier.classifyLines()

    setPageText()
  }

  private def labelImages(): Unit = {
    mpageIndex.getImageAtoms(pageNum).foreach { imgCC =>
      mpageIndex.labelRegion(Seq(imgCC), LB.Image)
    }
  }

  private def runLineDeterminationOnPage(): Unit = {
    tracer.enter()

    lineFinding.determineLines()

    tracer.exit()
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
