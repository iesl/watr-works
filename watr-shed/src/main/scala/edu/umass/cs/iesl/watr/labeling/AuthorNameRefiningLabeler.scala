package edu.umass.cs.iesl.watr
package labeling

import bioarxiv._
import BioArxiv._
import AlignBioArxiv._
// import data._
import textreflow.data._
import watrmarks.{StandardLabels => LB}
import TypeTags._
import corpora._
import geometry._
import utils._
import watrmarks._
import labeling.data._
import textboxing.{TextBoxing => TB}, TB._


class AuthorNameRefiningLabeler(
  override val corpusAccessApi: CorpusAccessApi,
  workflowId: String@@WorkflowID
) extends ServerLabelerBuilder {
  private[this] val log = org.log4s.getLogger

  def queryLabel = LB.Authors
  def batchSize = 5

  def targetLabels(): Seq[(Label, Color)] = List(
    (LB.Author, Colors.Orange),
    (LB.NoteMarkers, Colors.OliveDrab)
  )

  def createLabeler(zones: Seq[Zone]): LabelWidgetConfig = {
    // If we have access to metadata (with author names), load it in and try to do a text alignment

    ???
  }

  def findAndAlignAuthorsOnPage0(paper: PaperRec, stableId: String@@DocumentID): AlignmentScores = {
    log.info(s"aligning authors to bioarxiv paper ${paper.title}")

    val authorBoosts = new AlignmentScores(LB.Authors)

    val lineReflows = for {
      (vlineZone, linenum) <- docStore.getPageVisualLines(stableId, PageNum(0)).zipWithIndex
    } yield {

      println(s"alignAuthors: ${vlineZone}, ${linenum}")
      val vlineReflow = docStore.getTextReflowForZone(vlineZone.id)
      val reflow = vlineReflow.getOrElse { sys.error(s"no text reflow found for line ${linenum}") }
      (linenum, reflow, reflow.toText)
    }


    val lineTrisAndText = for {
      (linenum, vlineReflow, lineText) <- lineReflows
      lineInfo = ReflowSliceInfo(linenum, vlineReflow, vlineReflow.toText())
    } yield for {
      i <- 0 until vlineReflow.length
      (slice, sliceIndex)       <- vlineReflow.slice(i, i+3).zipWithIndex
    } yield {
      val triInfo = ReflowSliceInfo(sliceIndex, slice, slice.toText())
      (lineInfo, triInfo)
    }

    val page0Trigrams = lineTrisAndText.flatten.toList


    paper.authors.map(author =>
      authorBoosts.alignStringToPage(author, page0Trigrams)
    )

    authorBoosts
  }

  def nameLabeler(docs: Seq[(String@@DocumentID, PaperRec)]): LabelWidget = {
    val highestScoringLines =
      docs.map({case (docId, paperRec) =>
        val scores = findAndAlignAuthorsOnPage0(paperRec, docId)
        val bestLine = scores.lineScores
          .toList.sortBy(_._2).reverse
          .headOption.map(_._1)
          .map({linenum =>
            scores.lineReflows(linenum)
          })

        val authorStrings = indent(2)(
          TB.vcat(List(
            "BibTex Authors".box,
            indent(2)(
              TB.vcat(
                paperRec.authors.map(_.box)
              )))))

        bestLine.map(l => (l, authorStrings))
      })

    val nameLines = highestScoringLines.flatten

    val allNames = nameLines.map({ case (namesReflow, authorNames) =>
      val namesText = namesReflow.toText()
      val target = namesReflow.targetRegion()
      val namesTarget = LW.targetOverlay(ensureTargetRegion(target), List())

      // val bbox = namesReflow.bounds()
      // val pageIds = namesReflow.charAtoms.map(_.pageId).toSet
      // val pageId = pageIds.head
      // val namesTarget = LW.targetOverlay(bbox, pageId, List())

      // Try super/subscript split:
      val splitSuperscripts = labelSplit(namesReflow, LB.Sup)

      // println("namesReflow: ")
      // println(prettyPrintTree(namesReflow))

      splitSuperscripts
        .foreach({case (name0, maybeLabel) =>
          println(s"name: ${maybeLabel} ")
          println(prettyPrintTree(name0))
        })

      val foundSupscripts = splitSuperscripts
        .filter(_._2.isDefined)
        .length > 1

      val nameReflows = if (foundSupscripts) {

        val labelTargetRegions = splitSuperscripts
          .filter(_._2.isDefined)
          .map(_._1)


        val candidates = labelTargetRegions
          .foldLeft((namesReflow, List.empty[TextReflow]))({case ((accReflow, splits), elemReflow) =>
            // val splitTr = accReflow.bounds.splitHorizontal(elemReflow.bounds)
            val elemTargetRegion = elemReflow.targetRegion()
            val splitTr: Seq[LTBounds] = ??? //  accReflow.targetRegion().splitHorizontal(elemTargetRegion)

            val local =  (splitTr.map({tr =>
              accReflow.clipToBoundingRegion(tr).map(_._1).toList
            })).flatten

            (local.last, splits ++ local)
          })

        candidates._2.sortBy(_.bounds.left)

      } else {
        // split names on comma/semicolon
        val regex = if (namesText.contains(';')) { ";".r }
                    else if (namesText.contains(',')) { ",".r }
                    else { "<do nothing>".r }

        val miter = regex.findAllIn(namesText)

        val sliced = miter.matchData.toList
          .foldLeft((0, List[TextReflow]()))({case ((lastIndex, reflows), m) =>
            val slice = namesReflow.slice(lastIndex, m.start)

            (m.end, slice.get :: reflows)
          })

        val (lastSliceEnd, slices) = sliced

        namesReflow
          .slice(lastSliceEnd, namesReflow.length)
          .map(_ :: slices)
          .getOrElse(slices)
          .reverse

      }


      val nameWidgets = nameReflows
        .map({n =>
          LW.pad(
            LW.targetOverlay(ensureTargetRegion(n.targetRegion), List()),
            Padding.Ints(left=0, top=0, right=0, bottom=2)
          )
        })

      val nameCol = LW.pad(
        LW.col(nameWidgets:_*),
        Padding.Ints(left=4, top=2, right=0, bottom=3)
      )

      val authors = LW.textbox(authorNames)

      val extraction = TB.indent(2)(TB.vcat(List(
        "Extracted Text",
        TB.indent(2)(namesText)
      )))

        LW.pad(
          LW.row(
            LW.col(
              namesTarget,
              nameCol
            ),
            LW.col(
              LW.textbox(extraction),
              authors
            )
          ),
          Padding.Ints(left=2, top=0, right=0, bottom=5)
        )


    })

    LW.col(allNames:_*)

  }


}
