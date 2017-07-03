package edu.umass.cs.iesl.watr
package watrcolors
package annotate


import scala.collection.mutable

import watrmarks.{StandardLabels => LB}
import watrmarks._
import utils.Color
import textboxing.{TextBoxing => TB}, TB._

import geometry._
import corpora._
import labeling._
import labeling.data._
import utils.Colors
import textreflow.data._
import scala.collection.mutable



class AffiliationsFineGrainedLabeler(
  override val corpusAccessApi: CorpusAccessApi,
  workflowId: String@@WorkflowID
) extends ServerLabelerBuilder {

  // import heuristics._
  import heuristics.GenericHeuristics._
  // import heuristics.AuthorNameHeuristics._
  import heuristics.AffiliationsHeuristics._
  import heuristics.Utils._


  val BATCH_SIZE = 5

  def createLabeler(userId: Int@@UserID): LabelWidgetConfig = {
    println(s"createLabeler for ${userId}")

    workflowApi.getUserLockGroup(userId).foreach { lockGroupId =>
      println(s"createLabeler: releasing old lock for ${lockGroupId}")
      workflowApi.releaseZoneLocks(lockGroupId)
    }

    val lockGroupId = workflowApi.makeLockGroup(userId)
    println(s"createLabeler: makeLockGroup ${lockGroupId}")

    println(s"createLabeler: acquiring zone locks ")
    val zoneLocks = workflowApi.aquireZoneLocks(
      lockGroupId,
      docStore.ensureLabel(LB.Affiliations),
      BATCH_SIZE
    )
    println(s"createLabeler: acquired zone locks ${zoneLocks} ")

    val separatedComponentsWithClasses: mutable.ListBuffer[(String, mutable.ListBuffer[String])] = mutable.ListBuffer()

    val zoneRegions = for {
      zoneLockId <- zoneLocks.toSeq
      zoneLock <- workflowApi.getZoneLock(zoneLockId).toSeq
    } yield {
      val affilationZone = docStore.getZone(zoneLock.zone)
      // val zoneRegions = affilationZone.regions
      for {
        targetRegion   <- affilationZone.regions
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

    val allBlocks = zoneRegions.map{ block =>
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
