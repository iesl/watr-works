package edu.umass.cs.iesl.watr
package labeling


import bioarxiv._
import BioArxiv._
import data._
import textboxing.{TextBoxing => TB}, TB._
import data._
import corpora._
import geometry._
import utils.Colors
import watrmarks.{StandardLabels => LB}
import TypeTags._


object TitleAuthorsLabelers extends LabelWidgetUtils {

  def bioArxivLabeler(
    labelerIdentifier: LabelerIdentifier,
    paperRec: PaperRec,
    docStore: DocumentCorpus
  ): (LabelWidget, LabelerIdentifier) = {
    val DocumentLabelerIdentifier(stableId, labelerType, pagination, labelColors) = labelerIdentifier

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
          Padding(2),
          Colors.DarkSlateBlue
        )
      }
    }

    val placeholders = Stream.continually(LW.textbox("<empty page>"))

    val widgets = (pageWidgets.toStream ++ placeholders).take(4)

    val body = LW.row(
      LW.col(
        LW.row(
          widgets(0),
          widgets(1)
        ),
        LW.row(
          widgets(2),
          widgets(3)
        )
      ),
      LW.pad(
        paperRecWidget,
        Padding(10, 10, 0, 0),
        Colors.Linen
      )
    )

    val updatedIdentifier =  DocumentLabelerIdentifier(
      stableId, labelerType,
      Pagination(numWindows, PageNum(displayWindow), None),
      Map(
        (LB.Title, Colors.Wheat),
        (LB.Authors, Colors.Orange),
        (LB.Abstract, Colors.MediumTurquoise),
        (LB.Affiliation, Colors.OliveDrab),
        (LB.References, Colors.Peru)
      )
    )

    (body, updatedIdentifier)

  }

}

// val allPageLines = for {
//   (zone, linenum) <- docStore.getPageVisualLines(stableId, page0).zipWithIndex
//   lineReflow      <- docStore.getTextReflowForZone(zone.id)
// } yield {
//   // FIXME: kludge:
//   val lt = LW.labeledTarget(pageTargetRegion, None, None)
//   (linenum, (0d, lt))
// }

// val scores: Seq[AlignmentScores] = AlignBioArxiv.alignPaperWithDB(docStore, paperRec, stableId)

// // linenum -> best-score, label-widget
// val allLineScores = mutable.HashMap[Int, (Double, LabelWidget)]()
// allLineScores ++= allPageLines

// val overlays = scores.map({alignScores =>
//   val label = alignScores.alignmentLabel

//   val scoreList = alignScores.lineScores.toList
//   val maxScore = scoreList.map(_._2).max
//   val lineScores = scoreList.filter(_._2 > maxScore/2)

//   lineScores.foreach({case (linenum, score) =>
//     val lineReflow = alignScores.lineReflows(linenum)
//     val lineBounds = lineReflow.targetRegion()
//     val normalScore = score/maxScore

//     val lt = LW.labeledTarget(pageTargetRegion, Some(label), Some(normalScore))

//     if (allLineScores.contains(linenum)) {
//       val Some((currScore, currWidget)) = allLineScores.get(linenum)

//       if (normalScore > currScore) {
//         allLineScores.put(linenum, (normalScore, lt))
//       }

//     } else {
//       allLineScores.put(linenum, (normalScore, lt))
//     }

//   })
// })

// val lwidgets = allLineScores.toList
//   .sortBy(_._1)
//   .map({case (linenum, (score, lwidget)) =>
//     lwidget
//   })
