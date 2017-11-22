package edu.umass.cs.iesl.watr
package formats

import edu.umass.cs.iesl.watr.watrmarks.WeightedLabeling
import spindex._
import textboxing.{TextBoxing => TB}, TB._
// import watrmarks.{StandardLabels => LB}
import segment.{SegmentationLabels => LB}
import segment.PageSegmenter
import _root_.io.circe, circe.syntax._


object DocumentIO  {

  import textgrid._

  def documentToPlaintext(mpageIndex: MultiPageIndex): String = {
    // // mpageIndex.docStore.getPageText(pageId)

    // val serProps = new TextGrid.SerializationProps
    // for {
    //   pageNum      <- mpageIndex.getPages
    //   pageIndex    <- List(mpageIndex.getPageIndex(pageNum))
    // }  {
    //   for {
    //     (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex)
    //     (line, n)    <- lineCCs.zipWithIndex
    //     textRow      <- pageIndex.components.getComponentText(line, LB.VisualLine).toList
    //   } {
    //     textRow.serialize(serProps)
    //   }

    // }
    // val lineNums = serProps.lineMap.keys.toList.sorted

    // val textLines = lineNums.map { lineNum =>
    //   val t = serProps.lineMap(lineNum)._2
    //   t.box
    // }

    // textLines.mkString("\n")
    "TODO"
  }

  def documentToStructuredPlaintext(mpageIndex: MultiPageIndex): String = {
    val allText = for {
      pageNum      <- mpageIndex.getPages
      pageIndex    <- List(mpageIndex.getPageIndex(pageNum))
    } yield {
      val textCol = for {
        (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex).toList
        lineCC <- lineCCs
      } yield {

        val lineText = pageIndex.components.getComponentText(lineCC, LB.VisualLine).map(_.toText().take(40).mkString)
        lineText.getOrElse("<no text>").box
      }

      val pinCol = for {
        (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex).toList
        lineCC <- lineCCs
      } yield {
        val lineWeights = pageIndex.components.getAttribute[WeightedLabeling](lineCC.id, watrmarks.Label("LineGrouping")).get
        val linePins = lineWeights.countedPins()
        if (linePins.nonEmpty) {
          val maxPin = linePins.maxBy(_._2)._1

          val pinrep = {
            maxPin.isBegin.option[String]("^")
              .orElse { maxPin.isInside.option[String]("|") }
              .orElse { maxPin.isUnit.option[String]("#") }
              .orElse { maxPin.isLast.option[String]("$") }
              .getOrElse { "?" }
          }

          val pincol = maxPin.label match {
            case LB.Para =>
              val pincls = {
                maxPin.isBegin.option[String]("¶")
                  .getOrElse { " " }
              }
              // '¶' ; '⁋' '§'
              s"${pincls}${pinrep}"

            case _ =>
              s"${pinrep} "
          }

          pincol.box

        } else {
          "~".box
        }
      }
      val groupings = hjoin(left)(
        vjoins(TB.right)(pinCol),
        "  ",
        vjoins(TB.left)(textCol)
      )

      groupings
    }

    vjoins(left)(
      allText
    ).toString()
  }

  def documentToJson(mpageIndex: MultiPageIndex): String = {

    val serProps = new TextGrid.SerializationProps
    for {
      pageNum      <- mpageIndex.getPages
      pageIndex    <- List(mpageIndex.getPageIndex(pageNum))
    }  {
      for {
        (blockCC, lineCCs) <- PageSegmenter.getVisualLinesInReadingOrder(pageIndex)
        (line, n)    <- lineCCs.zipWithIndex
        textRow      <- pageIndex.components.getComponentText(line, LB.VisualLine).toList
      } {
        textRow.serialize(serProps)
      }

    }
    val lineNums = serProps.lineMap.keys.toList.sorted

    val textLines = lineNums.map { lineNum =>
      val t = serProps.lineMap(lineNum)._2
      t.asJson.noSpaces.box
    }

    val textLinesBlock = indent(4)(vjoinTrailSep(left, ",")(textLines:_*))

    val lineDefs = lineNums.map { lineNum =>
      serProps.lineMap(lineNum)._1.noSpaces.box
    }

    val lineDefsBlock = indent(4)(vjoinTrailSep(left, ",")(lineDefs:_*))

    val pageIdDefs = serProps.pageIdMap.toList
      .map{ case (pageId, (stableId, pageNum)) =>
        s"""[${pageId}, "${stableId}", ${pageNum}]""".box
      }


    val pageIdBlock = indent(4)(vjoinTrailSep(left, ",")(pageIdDefs:_*))

    val finalDocument = (
      s"""|{ "lines": [
          |${textLinesBlock}
          |  ],
          |  "zones": [
          |{serializedZones}
          |  ],
          |  "relations": [
          |{relationBlock}
          |  ],
          |  "errors": [
          |  ],
          |  "labels": [
          |  ],
          |  "pageDefs": [
          |${pageIdBlock}
          |  ],
          |  "lineDefs": [
          |${lineDefsBlock}
          |  ]
          |}
          |""".stripMargin)


    finalDocument.split("\n")
      .map(_.reverse.dropWhile(_==' ').reverse)
      .mkString("\n")

    textLines.mkString("\n")
  }

  // def richTextSerializeDocument(mpageIndex: MultiPageIndex): String = {
  //   val docStore = mpageIndex.docStore
  //   val stableId = mpageIndex.getStableId()
  //   val docId = docStore.getDocument(stableId).get

  //   // mpageIndex.getPages
  //   for {
  //     pageNum <- mpageIndex.getPages
  //   } yield {
  //     val pageIndex = mpageIndex.getPageIndex(pageNum)
  //     pageIndex.components.getClusterRoots(LB.VisualLine).map { root =>
  //       val maybeText = pageIndex.components.getComponentText(root, LB.VisualLine)
  //       maybeText.foreach { text =>
  //         // println(text.toText())
  //       }
  //     }
  //   }

  //   val escapedTextReflows = for {
  //     lineZone <- docStore.getDocumentZones(docId, LB.VisualLine)
  //     reflow <- docStore.getTextReflowForZone(lineZone.id)
  //   } yield {
  //     reflow.applyLineFormatting()
  //   }

  //   val textAndJsons = escapedTextReflows
  //     .map({ blockTextReflow =>
  //       val formattedText = blockTextReflow.toText()
  //       val json = docStore.textReflowToJson(blockTextReflow)
  //       (formattedText, json)
  //     })


  //   val textLines = textAndJsons.map(_._1)
  //   val textLinesBlock = indent(4)(vjoinTrailSep(left, ",")( textLines.map(t => Json.stringify(JsString(t)).box):_*))
  //   // val jsonLines =      indent(4)(vjoinTrailSep(left, ",")(textAndJsons.map(pair => Json.stringify(pair._2).box):_* ))

  //   // val serializedZones = indent(4)(serializeZones(mpageIndex, escapedTextReflows, textLines))

  //   // val relations = serializeRelation(mpageIndex)

  //   // val relationBlock = indent(4)(vjoinTrailSep(left, ",")(relations:_*))

  //   // val propertyBlock = indent(4)(vjoinTrailSep(left, ",")(
  //   //   mpageIndex.props.map({ prop =>
  //   //     Json.stringify(Json.toJson(prop)).box
  //   //   }):_*
  //   // ))

  //   // val lineLabelBlock = serializeLineLabels(mpageIndex)


  //   // Lines: ordered list ("visual order") of text lines
  //   // Zones: region-based labelings {"authors", [[10, 12, 14, 180], ... ]}

  //   val finalDocument = (
  //     s"""|{ "lines": [
  //         |${textLinesBlock}
  //         |  ],
  //         |  "zones": [
  //         |{serializedZones}
  //         |  ],
  //         |  "relations": [
  //         |{relationBlock}
  //         |  ],
  //         |  "errors": [
  //         |  ],
  //         |  "properties": [
  //         |{indent(4)(propertyBlock)}
  //         |  ],
  //         |  "labels": [
  //         |  ],
  //         |  "reflows": [
  //         |{jsonLines}
  //         |  ]
  //         |}
  //         |""".stripMargin)


  //   finalDocument.split("\n")
  //     .map(_.reverse.dropWhile(_==' ').reverse)
  //     .mkString("\n")
  // }



  // def serializeZones(mpageIndex: MultiPageIndex, textBlockReflows: Seq[TextReflow], textLines: Seq[String]): Box = {

  //   val docStore = mpageIndex.docStore
  //   val docId = mpageIndex.docId

  //   val zones = for {
  //     labelId <- docStore.getZoneLabelsForDocument(docId)
  //     zoneId <- docStore.getZonesForDocument(docId, labelId)
  //   } yield {
  //     val zone = docStore.getZone(zoneId)

  //     // for each target region in each zone, find the (begin, end) bounds of the TextReflow in that zone
  //     val targetRegions = zone.regions.map{ targetRegion =>
  //       s"[${targetRegion.bbox}]"
  //     }.mkString(", ")


  //     val zoneLabel = zone.label.toString

  //     val mentionStr = s"""[${zoneId}, ["$zoneLabel"], [${targetRegions}]]""".box
  //     mentionStr
  //   }

  //   vjoinTrailSep(left, ",")(zones:_*)
  // }

  // def serializeLineLabels(mpageIndex: MultiPageIndex): Box = {

  //   // val serComponents = List(
  //   //   LB.SectionHeadingLine,
  //   //   LB.ParaBegin,
  //   //   LB.TextBlock,
  //   //   LB.Abstract
  //   // ).map(serializeLabeling(_, lineBioLabels))

  //   // vjoinTrailSep(left, ",")(serComponents.flatten:_*)
  //   ???
  // }


  // def serializeLabeling(label: Label, bioLabeling: Seq[BioNode]): Seq[Box] = {
  //   // val labeledSpans = selectBioLabelings(label, bioLabeling)

  //   // val spanBoxes = for {
  //   //   span <- labeledSpans
  //   // } yield {

  //   //   val bioSpan = span
  //   //     .map(p => selectPinForLabel(label, p))

  //   //   val spanId = bioSpan.head.id
  //   //   val compIds = span.map(_.component.id)

  //   //   val cids = compIds.mkString(",")

  //   //   s"""["${label}", [${cids}], ${spanId}]""".box
  //   // }

  //   // spanBoxes
  //   ???
  // }




}

  // def serializeZones2(mpageIndex: MultiPageIndex, textBlockReflows: Seq[TextReflow], textLines: Seq[String]): Box = {

  //   val docStore = mpageIndex.docStore
  //   val docId = mpageIndex.docId

  //   val zoneTuple = for {
  //     labelId <- docStore.getZoneLabelsForDocument(docId)
  //     zoneId <- docStore.getZonesForDocument(docId, labelId)
  //   } yield {
  //     val zone = docStore.getZone(zoneId)

  //     // for each target region in each zone, find the (begin, end) bounds of the TextReflow in that zone
  //     val zoneLocationsAndReflows = zone.regions.map({ targetRegion =>

  //       val zoneTargetRegion = docStore.getTargetRegion(targetRegion.id)
  //       for {
  //         (textFlow, lineNum) <- textBlockReflows.zipWithIndex
  //         (clipped, range) <- textFlow.clipToBoundingRegion(zoneTargetRegion.bbox)
  //       } yield {
  //         ((lineNum, range), clipped)
  //       }
  //     }).flatten

  //     val zoneLineAndRange = zoneLocationsAndReflows.map(_._1)
  //     val zoneReflows = zoneLocationsAndReflows.map(_._2)
  //     // TODO This is a future error! it will only work b/c I happen to know in this case that
  //     //    labeled zones won't cross TextReflow instances for the MIT labeling
  //     val joinedZoneReflow = joins("")(zoneReflows)
  //     val zoneText = joinedZoneReflow.toText()

  //     val zoneTargets = zoneLineAndRange
  //       .map({case (l, r) =>
  //         val slice = textLines(l).slice(r.min, r.max)
  //         if (!zoneText.contains(slice)) {
  //           // Assert that the text for the mention we're serializing matches the line text
  //           println(s"mismatch between mention/text line: mention=${zoneText}, lineText=${slice}")
  //         }

  //         s"""[${l}, ${r.min}, ${r.len}]"""
  //       }).mkString(",")

  //     val zoneLabel = zone.label.toString
  //     val mentionId = zone.id.unwrap

  //     val clustId = mpageIndex.relations.collect({
  //       case Relation.Record(clusterId, "hasMember", e2) if Identities.idValue(e2) == mentionId =>
  //         Identities.idValue(clusterId)
  //     }).headOption.getOrElse(0)


  //     val pad3 = " "*(3 - mentionId.toString.length)
  //     val pad4 = " "*(3 - clustId.toString.length)

  //     val mentionStr = s"""[${mentionId},$pad3 ${clustId},$pad4 ["$zoneLabel"], "${zoneText}", [${zoneTargets}]]""".box

  //     (mentionId, clustId, mentionStr)
  //   }

  //   val zones = zoneTuple
  //     .sortBy({x => (x._1, x._2) })

  //   vjoinTrailSep(left, ",")(zones.map(_._3):_*)
  // }

// def serializeRelation(mpageIndex: MultiPageIndex): Seq[Box] = {
//   mpageIndex.relations
//     .filter({
//       case Relation.Record(_, rel, _)
//           if rel=="hasType" || rel== "hasMember" =>  false
//       case _ => true
//     })
//     .map({relation =>
//       val lhs = relation.lhs.map(Identities.write).unify
//       val rhs = relation.rhs.map(Identities.write).unify

//       val rel = relation.relationship
//       val pad1 = " "*(15-lhs.length())
//       val pad2 = " "*(12-rel.length())
//       s"""["$lhs", $pad1 "$rel", $pad2 "$rhs"]""".box
//     })
// }


// def selectPinForLabel(lb: Label, n: BioNode): BioPin = {
//   n.pins
//     .filter(p => p.label==lb)
//     .head
// }
