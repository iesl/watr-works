package edu.umass.cs.iesl.watr
package formats


import TypeTags._

import extract.PdfTextExtractor
import extract.fonts._
import spindex._
import textboxing.{TextBoxing => TB}, TB._
import utils.IdGenerator
import predsynth._

import ammonite.{ops => fs}, fs._

import watrmarks._
import textreflow._
import utils.EnrichNumerics._

import geometry._
import TextReflowJsonFormats._

object DocumentIO extends DocsegJsonFormats {

  def richTextSerializeDocument(zoneIndexer: ZoneIndexer, alignedGroups: Seq[AlignedGroup]): String = {
    import play.api.libs.json, json._

    val escapedTextReflows = zoneIndexer.getTextReflows()
      .map({ blockTextReflow =>
        blockTextReflow.applyLineFormatting()
      })

    val textAndJsons = escapedTextReflows
      .map({ blockTextReflow =>
        val formattedText = blockTextReflow.toText()
        if (formattedText.contains("analytical")) {
          // textreflow.doDebugPrinting = true
          // val _ = blockTextReflow.annotateCharRanges()
          // textreflow.doDebugPrinting = false
        }
        val json = blockTextReflow.toJson()
        (formattedText, json)
      })

    // Record any misalignments as errors
    val misalignments = for {
      group <- alignedGroups
      if group.alignedContexts.exists(_.isInstanceOf[AlignFailure])
    } yield {
      val clusterID = group.textMentionGroup.groupNumber
      group.alignedContexts.collect({
        case AlignFailure(rawTextContext, msg) =>
          s"ClusterID:${clusterID}: ${rawTextContext}"
      })
    }

    val alignmentErrors = indent(4)(vjoinTrailSep(left, ",")(
      misalignments.flatten.map({msg =>
        Json.stringify(JsString(msg)).box
      }):_*
    ))

    val textLines = textAndJsons.map(_._1)
    val textLinesBlock = indent(4)(vjoinTrailSep(left, ",")(   textLines.map(t => Json.stringify(JsString(t)).box):_*))
    val jsonLines =      indent(4)(vjoinTrailSep(left, ",")(textAndJsons.map(pair => Json.stringify(pair._2).box):_* ))

    val serializedZones = indent(4)(serializeZones(zoneIndexer, escapedTextReflows, textLines))

    val relations = serializeRelation(zoneIndexer)

    val relationBlock = indent(4)(vjoinTrailSep(left, ",")(relations:_*))

    val propertyBlock = indent(4)(vjoinTrailSep(left, ",")(
      zoneIndexer.props.map({ prop =>
        Json.stringify(Json.toJson(prop)).box
      }):_*
    ))

    // val lineLabelBlock = serializeLineLabels(zoneIndexer)

    val finalDocument = (
      s"""|{ "lines": [
          |${textLinesBlock}
          |  ],
          |  "mentions": [
          |${serializedZones}
          |  ],
          |  "relations": [
          |${relationBlock}
          |  ],
          |  "errors": [
          |${alignmentErrors}
          |  ],
          |  "properties": [
          |${indent(4)(propertyBlock)}
          |  ],
          |  "labels": [
          |  ],
          |  "lineDefs": [
          |${jsonLines}
          |  ]
          |}
          |""".stripMargin)

    finalDocument.split("\n")
      .map(_.reverse.dropWhile(_==' ').reverse)
      .mkString("\n")



  }

  def serializeRelation(zoneIndexer: ZoneIndexer): Seq[TB.Box] = {
    zoneIndexer.relations
      .filter({
        case Relation.Record(_, rel, _)
            if rel=="hasType" || rel== "hasMember" =>  false
        case _ => true
      })
      .map({relation =>
        val lhs = relation.lhs.map(Identities.write).unify
        val rhs = relation.rhs.map(Identities.write).unify

        val rel = relation.relationship
        val pad1 = " "*(15-lhs.length())
        val pad2 = " "*(12-rel.length())
        s"""["$lhs", $pad1 "$rel", $pad2 "$rhs"]""".box
      })
  }


  def selectPinForLabel(lb: Label, n: BioNode): BioPin = {
    n.pins
      .filter(p => p.label==lb)
      .head
  }


  def serializeZones(zoneIndexer: ZoneIndexer, textBlockReflows: Seq[TextReflow], textLines: Seq[String]): TB.Box = {

    // Serialize all zones (mentions)
    val zones = zoneIndexer.getZones
      .map({ zone =>

        // for each target region in each zone, find the (begin, end) bounds of the TextReflow in that zone
        val zoneLocationsAndReflows = zone.regions.map({zoneTargetRegion =>
          for {
            (textFlow, lineNum) <- textBlockReflows.zipWithIndex
            (clipped, range) <- textFlow.clipToTargetRegion(zoneTargetRegion)
          } yield {
            ((lineNum, range), clipped)
          }
        }).flatten

        val zoneLineAndRange = zoneLocationsAndReflows.map(_._1)
        val zoneReflows = zoneLocationsAndReflows.map(_._2)
        // TODO This is a future error! it will only work b/c I happen to know in this case that
        //    labeled zones won't cross TextReflow instances for the MIT labeling
        val joinedZoneReflow = joins("")(zoneReflows)
        val zoneText = joinedZoneReflow.toText()

        val zoneTargets = zoneLineAndRange
          .map({case (l, r) =>
            val slice = textLines(l).slice(r.min, r.max)
            if (!zoneText.contains(slice)) {
              // Assert that the text for the mention we're serializing matches the line text
              println(s"mismatch between mention/text line: mention=${zoneText}, lineText=${slice}")
            }

            s"""[${l}, ${r.min}, ${r.len}]"""
          }).mkString(",")

        val zoneLabel = zone.label.value.get
        val pad1 = " "*(20-zoneLabel.length)
        val mentionId = zone.id.unwrap

        val clustId = zoneIndexer.relations.collect({
          case Relation.Record(clusterId, "hasMember", e2) if Identities.idValue(e2) == mentionId =>
            Identities.idValue(clusterId)
        }).headOption.getOrElse(0)


        val pad3 = " "*(3 - mentionId.toString.length)
        val pad4 = " "*(3 - clustId.toString.length)

        val mentionStr = s"""[${mentionId},$pad3 ${clustId},$pad4 "${zoneLabel}",${pad1} "${zoneText}", [${zoneTargets}]]""".box

        (mentionId, clustId, mentionStr)
      })
      .toSeq
      .sortBy({x => (x._1, x._2) })

    vjoinTrailSep(left, ",")(zones.map(_._3):_*)
  }

  def serializeLineLabels(zoneIndexer: ZoneIndexer): TB.Box = {

    // val serComponents = List(
    //   LB.SectionHeadingLine,
    //   LB.ParaBegin,
    //   LB.TextBlock,
    //   LB.Abstract
    // ).map(serializeLabeling(_, lineBioLabels))

    // vjoinTrailSep(left, ",")(serComponents.flatten:_*)
    ???
  }


  def serializeLabeling(label: Label, bioLabeling: Seq[BioNode]): Seq[Box] = {
    // val labeledSpans = selectBioLabelings(label, bioLabeling)

    // val spanBoxes = for {
    //   span <- labeledSpans
    // } yield {

    //   val bioSpan = span
    //     .map(p => selectPinForLabel(label, p))

    //   val spanId = bioSpan.head.id
    //   val compIds = span.map(_.component.id)

    //   val cids = compIds.mkString(",")

    //   s"""["${label}", [${cids}], ${spanId}]""".box
    // }

    // spanBoxes
    ???
  }


  def extractChars(
    pdfPath: Path,
    charsToDebug: Set[Int] = Set(),
    glyphDefs: Seq[SplineFont.Dir] = Seq()
  ): Seq[(Seq[PageAtom], PageGeometry)] = {

    val charExtractor = new PdfTextExtractor(
      charsToDebug,
      IdGenerator[RegionID](), //, IdGenerator[PageID]
      glyphDefs
    )

    charExtractor.extractCharacters(pdfPath)
  }



}
