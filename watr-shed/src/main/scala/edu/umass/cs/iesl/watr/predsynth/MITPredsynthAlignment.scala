package edu.umass.cs.iesl.watr
package predsynth //;import acyclic.file

import spindex._
import ComponentOperations._

import scala.collection.mutable

import utils.SlicingAndDicing._

import geometry._
import GeometryImplicits._


object MITAlignPredsynth {
  private[this] val log = org.log4s.getLogger

  import utils.IdGenerator
  import textreflow._

  def alignPredSynthPaper(mpageIndex: MultiPageIndex, paper: Paper): Seq[AlignedGroup] = {
    log.debug("aligning predsynth paper ")

    val paperTextReflows = mpageIndex.getTextReflows(LB.PageTextBlocks, LB.TextBlock)

    log.debug("creating one line from entire paper")
    val oneLineReflow = paperTextReflows.reduce { joinTextLines(_, _)(utils.EnglishDictionary.global) }

    val oneLineText = oneLineReflow.toText

    log.debug("Aligning contexts")

    val alignedGroups: Seq[AlignedGroup] = PredsynthLoad.alignContexts(paper, oneLineText)

    val mongoIdToClusterId = mutable.HashMap[String, Int@@ClusterID]()
    val rawTextMentionsById = mutable.HashMap[Int@@MentionID, RawTextContext]()
    val relations = mutable.ArrayBuffer[Relation.Record]()
    val props = mutable.ArrayBuffer[Prop.PropRec]()

    val relationIds = IdGenerator[RelationID]()
    // val clusterIds = IdGenerator[ClusterID]()


    alignedGroups.foreach({ alignedGroup: AlignedGroup =>
      val groupNumber = alignedGroup.textMentionGroup.groupNumber
      val id = alignedGroup.textMentionGroup.id
      val groupClusterID = ClusterID(groupNumber) // clusterIds.nextId
      log.debug(s"aligning group w/ClusterID:${groupClusterID}")

      id.foreach { mongoId =>
        mongoIdToClusterId.put(mongoId, groupClusterID)
      }

      props += Prop.PropRec(
        Identities.cluster(groupClusterID),
        Prop.PropKV(
          "role",
          Prop.Str("recipe/"+alignedGroup.groupType)
        )
      )

      props += Prop.PropRec(
        Identities.cluster(groupClusterID),
        Prop.PropKV(
          "mongoId",
          Prop.Str(id.getOrElse("null"))
        )
      )

      props ++= alignedGroup.textMentionGroup.props.map(
        Prop.PropRec(
          Identities.cluster(groupClusterID), _))


      alignedGroup.alignedContexts.foreach {
        case AlignSuccess(rtc, (begin, end)) =>

          val len = oneLineText.length
          val reflowSliceOpt = oneLineReflow.slice(begin, end)

          reflowSliceOpt match {
            case Some(reflowSlice) =>
              val reflowSliceText = reflowSlice.toText
              log.debug(s"found mention: ${reflowSliceText}")

              // TODO The following should be captured by something like textReflow.intersectPages(LB.VisualLine, ..) function
              val targetRegions = reflowSlice.targetRegions

              val intersectedVisualLines  = targetRegions.map{ targetRegion =>
                val pageIndex = mpageIndex.getPageIndex(targetRegion.pageId)

                pageIndex.componentIndex
                  .queryForIntersects(targetRegion.bbox)
                  .filter(_.hasLabel(LB.VisualLine))
                  .headOption
                  .getOrElse { sys.error(s"no visual line found intersecting ${targetRegion}") }

              }

              val uniqVisualLines = intersectedVisualLines
                .groupByPairs ({ case (c1, c2) => c1.id == c2.id })
                .map(_.head)

              val ann = LB.Annotation(rtc.textType)

              // Compute the intersection of a TextReflow w/ RegionComponent
              val annotationRegions = uniqVisualLines.map{visualLine =>
                val pageForLine = visualLine.pageId
                val pageRegions = targetRegions.filter(_.pageId == visualLine.pageId)
                // Select the span for each line that corresponds to labeled region
                val intersectingLineAtoms = visualLine.queryAtoms()
                  .trimLeftRightBy({lineAtom: AtomicComponent =>
                    val intersects = pageRegions.exists(_.bbox.intersects(lineAtom.bounds));
                    !intersects
                  })

                mpageIndex.labelRegion(intersectingLineAtoms, ann)
              }

              val annRegions = annotationRegions.flatten.map{_.targetRegion}
              val newZone = Zone(ZoneID(0), annRegions, List(ann))

              val zAdded = mpageIndex.addZone(newZone)

              // HACK: make zoneId==mentionId TODO document why
              val mentionId = MentionID(zAdded.id.unwrap)

              rawTextMentionsById.put(mentionId, rtc)

              relations += Relation.Record(
                Identities.cluster(groupClusterID),
                "hasMember",
                Identities.mention(mentionId)
              )

              val textSlice = oneLineText.slice(begin, end)
              if (rtc.rawText.raw_text == textSlice) {
                log.debug(s"exact str match  > g:${groupNumber} ${id} >> ${rtc.toString()}")
              } else {
                log.debug(s"*inexact match> g:${groupNumber} ${id} >> ${rtc.toString()}  ===>  ${textSlice}")
              }

            case None =>
              log.debug("Error: couldn't slice textreflow")
          }


        case AlignFailure(rtc, message) =>
          log.debug(s"!failed to align> ${rtc} ${message}")
      }
    })

    paper.connections
      .flatten.foreach({ connection =>
        (connection.id1, connection.id2) match {
          case (Some(id1), Some(id2)) =>
            val group1 = mongoIdToClusterId(id1)
            val group2 = mongoIdToClusterId(id2)

            relations += Relation.Record(
              Identities.cluster(group1),
              "connectsTo",
              Identities.cluster(group2)
            )

          case (None, Some(id2)) =>
            val group2 = mongoIdToClusterId(id2)
            log.debug(
              s"""errata: (?? `connectsTo` group:${group2})""")
          case (Some(id1), None) =>
            val group1 = mongoIdToClusterId(id1)
            log.debug(
              s"""(group:${group1} `connectsTo` ??)""")
          case _ =>
        }
      })

    mpageIndex.addRelations(relations)
    mpageIndex.addProps(props)

    alignedGroups

  }

}
