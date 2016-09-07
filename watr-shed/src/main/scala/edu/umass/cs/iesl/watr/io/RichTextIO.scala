package edu.umass.cs.iesl.watr
package format


import spindex._
import ComponentOperations._
import EnrichGeometricFigures._
import watrmarks.{StandardLabels => LB}


object RichTextIO {

  import utils.SlicingAndDicing._
  import ComponentRendering.VisualLine
  import textboxing.{TextBoxing => TB}
  import TB._


  def serializeDocumentAsText(zoneIndexer: ZoneIndexer, artifactPath: Option[String]): String = {

    val allDocumentLines = for {
      pageId <- zoneIndexer.getPages
      pageInfo = zoneIndexer.getPageInfo(pageId)
      page <- pageInfo.getComponentsWithLabel(LB.Page)
      line <-  page.getChildren(LB.VisualLine)
      lineText <- VisualLine.render(line)
    } yield {
      val region = line.targetRegion.toString()
      (lineText, region.box)
    }

    vjoin()(
      "===================",
      artifactPath.map(_.box).getOrElse("corpus:unknown artifact".box) ,
      hjoin()(
        vjoins(a=left)(allDocumentLines.map(_._1)),
        "     ",
        vjoins(a=left)(allDocumentLines.map(_._2))
      ),
      "==================="
    ).toString()


  }
}
