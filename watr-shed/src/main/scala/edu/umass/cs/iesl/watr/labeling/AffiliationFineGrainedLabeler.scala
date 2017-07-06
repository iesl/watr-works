package edu.umass.cs.iesl.watr
package labeling

import watrmarks.{StandardLabels => LB}
import textreflow.data._
import corpora._
import scala.collection.mutable
import labeling.data._
import geometry._
import utils._
import watrmarks._


class AffiliationsFineGrainedLabeler(
  override val corpusAccessApi: CorpusAccessApi,
  workflowId: String@@WorkflowID
) extends ServerLabelerBuilder {

  import heuristics.GenericHeuristics._
  import heuristics.AffiliationsHeuristics._
  import heuristics.Utils._

  override def queryLabel: Label = LB.Affiliations
  override def batchSize: Int = 5

  override def createLabeler(zones: Seq[Zone]): LabelWidgetConfig = {

    val separatedComponentsWithClasses: mutable.ListBuffer[(String, mutable.ListBuffer[String])] = mutable.ListBuffer()

    val zoneWidgets = for {
      zone <- zones
    } yield {

      // val zoneRegions = affilationZone.regions
      for {
        targetRegion   <- zone.regions
        targetRegionTextReflow <- docStore.getTextReflowForTargetRegion(targetRegion.id)
      } yield {


        print("Text Reflow: \t\t\t\t" + targetRegionTextReflow.toText() + "\n")
        val tokenizedReflow = tokenizeTextReflow(targetRegionTextReflow)
        if (tokenizedReflow.nonEmpty) {
          print("Tokenized: \t\t\t\t\t" + tokenizedReflow.mkString("||") + "\n")
          val affiliationsSeparatedByText: mutable.ListBuffer[String] =
            getSeparateAffiliationComponentsByText(tokenizedReflow)

          if (affiliationsSeparatedByText.nonEmpty) {
            print("Text Separated: \t\t\t" + affiliationsSeparatedByText.mkString("||") + "\n")

            val affiliationsSeparatedByGeometry =
              getSeparateComponentsByGeometry(affiliationsSeparatedByText, targetRegionTextReflow)
                .map { cleanSeparatedComponent(_) }

            if (affiliationsSeparatedByGeometry.nonEmpty) {

              print("Geometrically Separated: \t" + affiliationsSeparatedByGeometry.mkString("||") + "\n")
              println("Keywords matched for affiliations: ")

              val affilationCategories = getCategoryForSeparateAffiliationComponents(affiliationsSeparatedByGeometry)
              separatedComponentsWithClasses ++= affilationCategories
              println(affilationCategories)
            }
          }
        }

      }
      // val authorBlock = authorRegions.map{ region =>
      //   LW.pad(
      //     LW.targetOverlay(region, overlays=List()),
      //     Padding.Ints(left=0, top=0, right=2, bottom=2),
      //     Color.Transparent
      //   )
      // }
      // LW.row(authorBlock:_*)
      LW.row()
    }


    val allBlocks = zoneWidgets.map{ block =>
      LW.pad(
        LW.pad(
          block,
          Padding.Ints(left=0, top=5, right=5, bottom=12),
          Color.Transparent
        ),
        Padding.Ints(left=0, top=1, right=0, bottom=0),
        Colors.Blue
      )
    }

    val widget = LW.col(allBlocks:_*)

    LabelWidgetConfig(
      workflowId,
      widget
    )
  }

  def targetLabels(): Seq[(Label, Color)] = List(
    (LB.Author, Colors.Orange),
    (LB.NoteMarkers, Colors.OliveDrab)
  )
}
