package edu.umass.cs.iesl.watr
package format


import spindex._
import ComponentOperations._


object RichTextIO {

  import utils.SlicingAndDicing._
  def serializeDocumentAsText(zoneIndexer: ZoneIndexer, artifactPath: Option[String]): String = {
    val lineSpine = zoneIndexer.bioSpine("TextBlockSpine")


    val pages = lineSpine.splitOnPairs({ (l1, l2) =>
      zoneIndexer.getPageForComponent(l1.component) != zoneIndexer.getPageForComponent(l2.component)
    })

    val fmtPages = for {
      page <- pages
    } yield {
      val pageHeader = page.headOption.map({c0 =>
        val comp = c0.component
        val pageId = zoneIndexer.getPageForComponent(comp)
        // val pageGeom = zoneIndexer.getPageGeometry(pageId)
        s"Page ${pageId}"
      }).getOrElse("Page (empty)")


      val pageText = pageHeader +: (for {
        lineBio <- page
      } yield {
        val lineComponent = lineBio.component

        lineComponent.tokenizeLine()
        lineComponent.toText
      })

      pageText.mkString("", "\n", "")
    }

    fmtPages.mkString("\n\n")

  }
}
