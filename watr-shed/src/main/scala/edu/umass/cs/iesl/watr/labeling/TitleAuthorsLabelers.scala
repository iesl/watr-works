package edu.umass.cs.iesl.watr
package labeling


import bioarxiv._
import BioArxiv._
import textboxing.{TextBoxing => TB}, TB._
import data._
import corpora._
import geometry._
import utils.Color
import utils.Colors
import watrmarks.{StandardLabels => LB, Label}
import TypeTags._


object TitleAuthorsLabelers {

  def labelerColors(): Seq[(Label, Color)] = {
    List(
      (LB.Title, Colors.Firebrick3),
      (LB.Authors, Colors.Orange),
      (LB.Abstract, Colors.MediumTurquoise),
      (LB.Affiliations, Colors.OliveDrab),
      (LB.References, Colors.Peru)
    )
  }

  def bioArxivLabeler(
    labelerIdentifier: LabelerIdentifier,
    paperRec: PaperRec,
    docStore: DocumentZoningApi
  ): (LabelWidget, LabelerIdentifier) = {
    val DocumentLabelerIdentifier(stableId, labelerType, pagination) = labelerIdentifier

    val startingPageWindow: Int = pagination.currentPage.unwrap

    val paperRecWidget = LW.textbox(
      TB.vjoin()(
        paperRec.title,
        indent(4)(
          TB.vcat(
            paperRec.authors.map(_.box)
          )
        )
      )
    )

    val docId = docStore.getDocument(stableId)
      .getOrElse(sys.error(s"Trying to access non-existent document ${stableId}"))

    val allPagesWindows = docStore.getPages(docId).sliding(4, 2).toList
    val numWindows = allPagesWindows.length
    val displayWindow = math.min(math.max(0, startingPageWindow), numWindows-1)

    val pageWidgets = if (displayWindow < 0) {
      List[LabelWidget]()
    } else {
      for {
        pageId <- allPagesWindows(displayWindow)
      } yield {

        val pageGeometry = docStore.getPageGeometry(pageId)

        val pageTargetRegion = docStore.getTargetRegion(
          docStore.addTargetRegion(pageId, pageGeometry)
        )

        LW.pad(
          LW.targetOverlay(pageTargetRegion, overlays=List()),
          Padding.Ints(2),
          Colors.DarkSlateBlue
        )
      }
    }

    val placeholders = Stream.continually(LW.textbox("<empty page>"))

    val widgets = (pageWidgets.toStream ++ placeholders).take(4)

    val rows = widgets.grouped(2).toList.map{ws =>
      LW.row(ws:_*)
    }

    val grid = LW.col(rows:_*)

    val body = LW.row(
      grid,
      LW.pad(
        paperRecWidget,
        Padding.Ints(20, 10, 1, 1)
      )
    )

    val updatedIdentifier =  DocumentLabelerIdentifier(
      stableId, labelerType,
      Pagination(numWindows, PageNum(displayWindow), None)
    )

    (body, updatedIdentifier)

  }
}
