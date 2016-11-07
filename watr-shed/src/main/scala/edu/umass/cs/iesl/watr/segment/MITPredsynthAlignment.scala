package edu.umass.cs.iesl.watr
package segment

import spindex._

import scalaz.@@
import TypeTags._
import ComponentRendering._
import ComponentOperations._
import EnrichGeometricFigures._
import utils.SlicingAndDicing._

import scala.collection.mutable
import watrmarks.{StandardLabels => LB}

import predsynth._

object MITAlignPredsynth {

  import textflow.TextReflow._

  def alignPredSynthPaper(zoneIndexer: ZoneIndexer, paper: Paper): Unit = {
    println("aligning predsynth paper ")

    val lineBioLabels = zoneIndexer.bioLabeling("LineBioLabels")

    val lineTextReflows = for {
      linec <- lineBioLabels
      line   <- VisualLine.toTextReflow(linec.component).toSeq
    } yield { line }

    // Join the TextReflow into a single line:
    // TODO: join using de-hyphenation
    val oneLineReflow = joins(" ")(lineTextReflows)
    val oneLineText = oneLineReflow.toText
    // val lineUnits = lineTextAndUnits.flatMap(_._2)


    // val contexts: Seq[Either[(RawTextContext, String), (RawTextContext, (Int, Int))]]
    val contexts: Seq[AlignedGroup]
      = PredsynthLoad.alignContexts(paper, oneLineText)

    val mongoIdToClusterId = mutable.HashMap[String, Int@@ClusterID]()
    val rawTextMentionsById = mutable.HashMap[Int@@MentionID, RawTextContext]()

    val relations = mutable.ArrayBuffer[Relation.Record]()
    val props = mutable.ArrayBuffer[Prop.PropRec]()

    import utils.IdGenerator

    val relationIds = IdGenerator[RelationID]()
    // val mentionIds = IdGenerator[MentionID]()
    val clusterIds = IdGenerator[ClusterID]()



    // idGen.nextId
    contexts.foreach({ alignedGroup: AlignedGroup =>
      val groupNumber = alignedGroup.textMentionGroup.groupNumber
      val id = alignedGroup.textMentionGroup.id
      val groupClusterID = clusterIds.nextId
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

          val slice = oneLineReflow.slice(begin, end)
          val foundText = slice.toText

          // val slice = lineUnits.slice(begin, end)
          // val foundText = slice.map({ funit =>
          //   TextFlow.toText(funit)
          // }).mkString


          val targetRegions = slice.targetRegions
          // val targetRegions = slice.collect({
          //   case u: FlowUnit.Atom =>
          //     val cc = u.atomicComponent
          //     cc.pageAtom.region

          //   case u: FlowUnit.Rewrite =>
          //     val cc = u.atom.atomicComponent
          //     cc.pageAtom.region
          // })


          val intersectedVisualLines  = targetRegions.map{ targetRegion =>
            val pageIndex = zoneIndexer.getPageIndex(targetRegion.target)

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
              .dropWhile({ lineAtom: AtomicComponent =>
                val haveIntersection = pageRegions.exists { _.bbox.intersects(lineAtom.bounds) }
                  !haveIntersection
              }).reverse
              .dropWhile({ lineAtom: AtomicComponent =>
                val haveIntersection = pageRegions.exists { _.bbox.intersects(lineAtom.bounds) }
                  !haveIntersection
              }).reverse


            zoneIndexer.labelRegion(intersectingLineAtoms, ann)
          }

          val annRegions = annotationRegions.flatten.map{_.targetRegion}
          val newZone = Zone(ZoneID(0), annRegions,ann)
          val zAdded = zoneIndexer.addZone(newZone)
          val mentionId = MentionID(zAdded.id.unwrap)

          rawTextMentionsById.put(mentionId, rtc)

          relations += Relation.Record(
            Identities.cluster(groupClusterID),
            "hasMember",
            Identities.mention(mentionId)
          )

          if (rtc.rawText.raw_text == foundText) {
            println(s"   > g:${groupNumber} ${id} >> ${rtc.toString()}")
          } else {
            println(s"***> g:${groupNumber} ${id} >> ${rtc.toString()}  ===>  ${foundText}")
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
