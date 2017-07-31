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
// import corpora.{RelationModel => Rel}
import TypeTags._
// import workflow._
// import textreflow.data._

object WorkflowServers {
  val servers = mutable.Map[String@@WorkflowID, (CorpusAccessApi, String@@WorkflowID) => ServerLabelerBuilder]()

  servers.put(WorkflowID("1-page"), (c, wid) => new PageOneLabeler(c, wid))
  servers.put(WorkflowID("au-names1"), (c, wid) => new AuthorNamesLabeler1(c, wid))
}


class AuthorNamesLabeler1(
  override val corpusAccessApi: CorpusAccessApi,
  workflowId: String@@WorkflowID
) extends ServerLabelerBuilder {

  override def queryLabel: Label = LB.Authors
  override def batchSize: Int = 20

  def createLabeler(zones: Seq[Zone]): LabelWidgetConfig = {
    val zoneRegions = for {
      zone <- zones
    } yield {

      val authorBlock = zone.regions.map{ region =>
        LW.pad(
          LW.targetOverlay(region, overlays=List()),
          Padding.Ints(left=0, top=0, right=2, bottom=2),
          Color.Transparent
        )
      }
      LW.row(authorBlock:_*)

    }

    val allAuthorBlocks = zoneRegions.map{ block =>
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

    val widget = LW.col(allAuthorBlocks:_*)

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


class PageOneLabeler(
  override val corpusAccessApi: CorpusAccessApi,
  workflowId: String@@WorkflowID
) extends ServerLabelerBuilder {

  override def queryLabel: Label = LB.DocumentPages
  override def batchSize: Int = 8

  override def createLabeler(zones: Seq[Zone]): LabelWidgetConfig = {

    val pageOnes = for {
      zone <- zones
      region <- zone.regions.headOption
    } yield region

    singlePageLabeler(pageOnes)

  }

  def singlePageLabeler(targetRegions: Seq[TargetRegion]): LabelWidgetConfig = {

    val pageOnes = targetRegions.map{ pageTargetRegion =>
      LW.pad(
        LW.targetOverlay(pageTargetRegion, overlays=List()),
        Padding.Ints(2),
        Colors.DarkSlateBlue
      )
    }

    val placeholders = Stream.continually(LW.textbox("<empty page>"))
    val widgets = (pageOnes.toStream ++ placeholders).take(batchSize)

    val rows = widgets.grouped(3).toList.map{ws =>
      LW.row(ws:_*)
    }

    val widget = LW.col(rows:_*)

    LabelWidgetConfig(
      workflowId,
      widget
    )
  }


  def targetLabels(): Seq[(Label, Color)] = List(
    (LB.Title, Colors.Firebrick3),
    (LB.Authors, Colors.Orange),
    (LB.Abstract, Colors.MediumTurquoise),
    (LB.Affiliations, Colors.OliveDrab)
  )
}
