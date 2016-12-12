package edu.umass.cs.iesl.watr
package segment

import spindex._

import scalaz.@@
import TypeTags._

import scala.collection.mutable
import watrmarks.{StandardLabels => LB}
// import textreflow.TextReflowRendering._
// import TextReflowConversion._

import predsynth._
import spindex.ComponentOperations._
import spindex.EnrichGeometricFigures._
import utils.SlicingAndDicing._

object MITAlignPredsynth {

  import utils.IdGenerator
  import textreflow._

  def alignPredSynthPaper(zoneIndexer: ZoneIndexer, paper: Paper): Unit = {
    println("aligning predsynth paper ")

    val paperTextReflows = for {
      pageId <- zoneIndexer.getPages
      pageTextBlocks <- zoneIndexer.getPageIndex(pageId).getComponentsWithLabel(LB.PageTextBlocks)
      textBlockCC <- pageTextBlocks.getChildren(LB.TextBlock)
      blockTextReflow <- zoneIndexer.getTextReflow(textBlockCC.id)
    } yield {
      blockTextReflow
    }



    println("creating one line from entire paper")
    val oneLineReflow = paperTextReflows.reduce { joinTextLines(_, _)(utils.EnglishDictionary.global) }

    val oneLineText = oneLineReflow.toText


    println("aligning contexts")

    // val contexts: Seq[Either[(RawTextContext, String), (RawTextContext, (Int, Int))]]
    val contexts: Seq[AlignedGroup]
      = PredsynthLoad.alignContexts(paper, oneLineText)

    val mongoIdToClusterId = mutable.HashMap[String, Int@@ClusterID]()
    val rawTextMentionsById = mutable.HashMap[Int@@MentionID, RawTextContext]()

    val relations = mutable.ArrayBuffer[Relation.Record]()
    val props = mutable.ArrayBuffer[Prop.PropRec]()


    val relationIds = IdGenerator[RelationID]()
    // val mentionIds = IdGenerator[MentionID]()
    val clusterIds = IdGenerator[ClusterID]()

    contexts.foreach({ alignedGroup: AlignedGroup =>
      val groupNumber = alignedGroup.textMentionGroup.groupNumber
      val id = alignedGroup.textMentionGroup.id
      val groupClusterID = clusterIds.nextId
      println(s"aligning group ${groupClusterID}")

      id.foreach { mongoId =>
        mongoIdToClusterId.put(mongoId, groupClusterID)
      }


      // val groupCluster = Relation.Elem.Cluster(groupClusterID)

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

          val textSlice = oneLineText.slice(begin, end)

          println(s"(string) text.slice(): ${textSlice}")

          val reflowSliceOpt = oneLineReflow.slice(begin, end)

          reflowSliceOpt match {
            case Some(reflowSlice) =>
              val reflowSliceText = reflowSlice.toText
              println(s"reflow.slice(): ${reflowSliceText}")

              // TODO The following should be captured by something like textReflow.intersectPages(LB.VisualLine, ..) function
              val targetRegions = reflowSlice.targetRegions

              val intersectedVisualLines  = targetRegions.map{ targetRegion =>
                val pageIndex = zoneIndexer.getPageIndex(targetRegion.target)

                println("TODO: don't filter by LB.VisualLine")
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
                val pageRegions = targetRegions.filter(_.target == visualLine.pageId)
                // Select the span for each line that corresponds to labeled region
                val intersectingLineAtoms = visualLine.queryAtoms()
                  .trimLeftRightBy({lineAtom: AtomicComponent =>
                    val intersects = pageRegions.exists(_.bbox.intersects(lineAtom.bounds));
                    !intersects
                  })

                zoneIndexer.labelRegion(intersectingLineAtoms, ann)
              }

              val annRegions = annotationRegions.flatten.map{_.targetRegion}
              val newZone = Zone(ZoneID(0), annRegions,ann)
              val zAdded = zoneIndexer.addZone(newZone)
              // HACK: make zoneId==mentionId TODO document why
              val mentionId = MentionID(zAdded.id.unwrap)

              rawTextMentionsById.put(mentionId, rtc)

              relations += Relation.Record(
                Identities.cluster(groupClusterID),
                "hasMember",
                Identities.mention(mentionId)
              )

              if (rtc.rawText.raw_text == textSlice) {
                println(s"   > g:${groupNumber} ${id} >> ${rtc.toString()}")
              } else {
                println(s"***> g:${groupNumber} ${id} >> ${rtc.toString()}  ===>  ${textSlice}")
              }

            case None =>
              println("Error: couldn't slice textreflow")
          }


        case AlignFailure(rawTextContext, message) =>
          println(s"!!!> ${rawTextContext.toString()} ${message}")

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
            println(
              s"""errata: (?? `connectsTo` group:${group2})""")
          case (Some(id1), None) =>
            val group1 = mongoIdToClusterId(id1)
            println(
              s"""(group:${group1} `connectsTo` ??)""")
          case _ =>
        }
      })

    zoneIndexer.addRelations(relations)
    zoneIndexer.addProps(props)

  }

}
