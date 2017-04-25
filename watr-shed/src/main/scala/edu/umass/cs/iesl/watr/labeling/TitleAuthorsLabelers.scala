package edu.umass.cs.iesl.watr
package labeling


import bioarxiv._
import BioArxiv._
// import AlignBioArxiv._
import data._
// import textreflow.data._
import watrmarks.{StandardLabels => LB}
import textboxing.{TextBoxing => TB}, TB._
import TypeTags._
import data._
import corpora._
// import scala.collection.mutable
import geometry._
import utils.Colors


object TitleAuthorsLabelers extends LabelWidgetUtils {

  def bioArxivLabeler(stableId: String@@DocumentID, paperRec: PaperRec, theDocumentCorpus: DocumentCorpus): LabelingPanel = {

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

    val docId = theDocumentCorpus.getDocument(stableId)
      .getOrElse(sys.error(s"Trying to access non-existent document ${stableId}"))



    val pageWidgets = for {
      pageId <- theDocumentCorpus.getPages(docId).take(4)
    } yield {

      val pageGeometry = theDocumentCorpus.getPageGeometry(pageId)

      val pageTargetRegionId = theDocumentCorpus.addTargetRegion(pageId, pageGeometry)

      val pageTargetRegion = theDocumentCorpus.getTargetRegion(pageTargetRegionId)

      LW.pad(
        LW.targetOverlay(pageTargetRegion, overlays=List()),
        Padding(4),
        Colors.DarkSlateBlue
      )
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

    LabelingPanel(
      body,
      LabelOptions(Map(
        (LB.Title, Colors.DarkSlateBlue),
        (LB.Authors, Colors.Orange),
        (LB.Abstract, Colors.MediumTurquoise),
        (LB.Affiliation, Colors.OliveDrab),
        (LB.References, Colors.Peru)
      ))
    )

  }

}

      // val allPageLines = for {
      //   (zone, linenum) <- theDocumentCorpus.getPageVisualLines(stableId, page0).zipWithIndex
      //   lineReflow      <- theDocumentCorpus.getTextReflowForZone(zone.id)
      // } yield {
      //   // FIXME: kludge:
      //   val lt = LW.labeledTarget(pageTargetRegion, None, None)
      //   (linenum, (0d, lt))
      // }

      // val scores: Seq[AlignmentScores] = AlignBioArxiv.alignPaperWithDB(theDocumentCorpus, paperRec, stableId)

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
