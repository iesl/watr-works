package edu.umass.cs.iesl.watr
package formats


import TypeTags._

import extract.PdfTextExtractor
import extract.fonts._
import spindex._
// import EnrichGeometricFigures._
import textboxing.{TextBoxing => TB}, TB._
import utils.IdGenerator
import predsynth._

import ammonite.{ops => fs}, fs._
import scala.util.{Try}

import watrmarks._
import watrmarks.{StandardLabels => LB}
import textreflow._
import utils.EnrichNumerics._

object DocumentIO extends DocsegJsonFormats {

  def richTextSerializeDocument(zoneIndexer: ZoneIndexer): String = {
    import play.api.libs.json, json._

    val textAndJsons = for {
      pageId <- zoneIndexer.getPages
      pageTextBlocks <- zoneIndexer.getPageIndex(pageId).getComponentsWithLabel(LB.PageTextBlocks)
      _ = println(s"Page $pageId")
      textBlockCC <- pageTextBlocks.getChildren(LB.TextBlock)
      blockTextReflow <- zoneIndexer.getTextReflow(textBlockCC.id)
    } yield {

      val formattedText = blockTextReflow.toFormattedText()
      val json = blockTextReflow.toJson()
      // println()
      // println(formattedText)
      // println(idList)

      // assert(formattedText.length()==idList.length)
      (formattedText, json)
    }

    val textLines = indent(4)(vjoins()(textAndJsons.map(_._1.box)))
    val jsonLines = indent(4)(vjoins()(textAndJsons.map(pair => Json.stringify(pair._2).box)))

    val serializedZones = serializeZones(zoneIndexer)

    val finalDocument = (
      s"""|{ "lines": [
          |${textLines}
          |  ],
          |  "mentions": [
          |  "relations": [
          |  "properties": [
          |  "labels": [
          |  "lineDefs": [
          |${jsonLines}
          |  ],
          |  "ids": [
          |""".stripMargin)

    finalDocument.split("\n")
      .map(_.reverse.dropWhile(_==' ').reverse)
      .mkString("\n")



    // val lineLabelBlock = serializeLineLabels(zoneIndexer)


    // // val tokenBlock = vjoinTrailSep(left, ",")(tokenDict:_*)

    // val pageAtomBlock = serializePageAtoms(zoneIndexer)

    // // output all relationships:
    // // rawTextMentionsById
    // val relations = zoneIndexer.relations
    //   .filter({
    //     case Relation.Record(_, rel, _)
    //         if rel=="hasType" || rel== "hasMember" =>  false
    //     case _ => true
    //   })
    //   .map({relation =>
    //     val lhs = relation.lhs.map(Identities.write).unify
    //     val rhs = relation.rhs.map(Identities.write).unify

    //     val rel = relation.relationship
    //     val pad1 = " "*(15-lhs.length())
    //     val pad2 = " "*(12-rel.length())
    //     s"""["$lhs", $pad1 "$rel", $pad2 "$rhs"]""".box
    //   })

    // val relationBlock = vjoinTrailSep(left, ",")(relations:_*)

    // val propertyBlock = vjoinTrailSep(left, ",")(
    //   zoneIndexer.props.map({ prop =>
    //     Json.stringify(Json.toJson(prop)).box
    //   }):_*
    // )



    // (s"""|{ "lines": [
    //      |${indent(4)(lineTextBlock)}
    //      |  ],
    //      |  "mentions": [
    //      |${indent(4)(mentionBlock)}
    //      |  ],
    //      |  "relations": [
    //      |${indent(4)(relationBlock)}
    //      |  ],
    //      |  "properties": [
    //      |${indent(4)(propertyBlock)}
    //      |  ],
    //      |  "labels": [
    //      |${indent(4)(lineLabelBlock)}
    //      |  ],
    //      |  "lineDefs": [
    //      |${indent(4)(lineDefBlock)}
    //      |  ],
    //      |  "ids": [
    //      |${indent(4)(pageAtomBlock)}
    //      |  ] }
    //      |""".stripMargin)

    // finalDocument
  }

  def selectPinForLabel(lb: Label, n: BioNode): BioPin = {
    n.pins
      .filter(p => p.label==lb)
      .head
  }


  def serializeZones(zoneIndexer: ZoneIndexer): TB.Box = {
    val textBlockReflows = for {
      pageId <- zoneIndexer.getPages
      pageTextBlocks <- zoneIndexer.getPageIndex(pageId).getComponentsWithLabel(LB.PageTextBlocks)
      textBlockCC <- pageTextBlocks.getChildren(LB.TextBlock)
      blockTextReflow <- zoneIndexer.getTextReflow(textBlockCC.id)
    } yield {
      blockTextReflow
    }

    // Serialize all zones (mentions)
    val zones = zoneIndexer.getZones
      .map({ zone =>

        // for each target region in each zone, find the (begin, end) bounds of the TextReflow in that zone
        val linePerZTR = zone.regions.map({zoneTargetRegion =>
          textBlockReflows.zipWithIndex
            .map({case (textFlow, lineNum) =>
              textFlow.clipToTargetRegion(zoneTargetRegion)
                .map(t => (t, lineNum))
            })
            .flatten.headOption
            .map({ case ((textFlow, intRange), lineNum)  =>
              val begin = intRange.min
              val len = intRange.len
              (s"""[${lineNum}, ${begin}, ${len}]""", textFlow)
            })

        }).flatten

        val trs = linePerZTR.map(_._1).mkString(",")
        val ts = linePerZTR.map(_._2)
        // TODO This is an error! it will only work b/c I happen to know in this case that
        //    labeled zones won't cross TextReflow instances for the MIT labeling
        val jtextFlow = joins(" ")(ts)
        val labelStr = zone.label.value.get
        val pad1 = " "*(20-labelStr.length())
        val pad2 = " "*(20 - jtextFlow.toText.length())

        val mentionId = zone.id.unwrap

        val clustId = zoneIndexer.relations.collect({
          case Relation.Record(clusterId, "hasMember", e2) if Identities.idValue(e2) == mentionId =>
            Identities.idValue(clusterId)
        }).headOption.getOrElse(0)


        val pad3 = " "*(3 - mentionId.toString.length)
        val pad4 = " "*(3 - clustId.toString.length)

        val mentionStr = s"""[${mentionId},$pad3 ${clustId},$pad4 "${labelStr}",${pad1} "${jtextFlow.toText()}",${pad2} [${trs}]]""".box

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
  ): Try[Seq[(PageAtoms, PageGeometry)]] = {

    val charExtractor = new PdfTextExtractor(
      charsToDebug,
      IdGenerator[RegionID](), //, IdGenerator[PageID]
      glyphDefs
    )

    charExtractor.extractCharacters(pdfPath)
  }



}

// def serializeMentions(zoneIndexer: ZoneIndexer): TB.Box = {
//   val textBlockReflows = for {
//     pageId <- zoneIndexer.getPages
  //     pageTextBlocks <- zoneIndexer.getPageIndex(pageId).getComponentsWithLabel(LB.PageTextBlocks)
  //     textBlockCC <- pageTextBlocks.getChildren(LB.TextBlock)
  //     blockTextReflow <- zoneIndexer.getTextReflow(textBlockCC.id)
  //   } yield {
  //     blockTextReflow
  //   }

  //   // val lines = for {
  //   //   linec <- lineBioLabels
  //   //   line = linec.component
  //   //   textFlow <- line.getTextReflow
  //   // } yield (textFlow, line.id, line.targetRegion)

  //   // val zones = zoneIndexer
  //   //   .getZones.map({ zone =>

  //   //     val linePerZTR = zone.regions.map({zoneTargetRegion =>
  //   //       lines.map(_._1).zipWithIndex
  //   //         .map({case (textFlow, lineNum) =>
  //   //           textFlow.clipToTargetRegion(zoneTargetRegion)
  //   //             .map(t => (t, lineNum))
  //   //         })
  //   //         .flatten.headOption
  //   //         .map({ case ((textFlow, begin, len), lineNum)  =>
  //   //           (s"""[${lineNum}, ${begin}, ${len}]""", textFlow)
  //   //         })

  //   //     }).flatten

  //   //     val trs = linePerZTR.map(_._1).mkString(",")
  //   //     val ts = linePerZTR.map(_._2)
  //   //     val jtextFlow = TextReflow.joins(" ")(ts)
  //   //     val labelStr = zone.label.value.get
  //   //     val pad1 = " "*(20-labelStr.length())
  //   //     val pad2 = " "*(20 - jtextFlow.toText.length())

  //   //     val mentionId = zone.id.unwrap


  //   //     val clustId = zoneIndexer.relations.collect({
  //   //       case Relation.Record(clusterId, "hasMember", e2) if Identities.idValue(e2) == mentionId =>
  //   //         Identities.idValue(clusterId)
  //   //     }).headOption.getOrElse(0)


  //   //     val pad3 = " "*(3 - mentionId.toString.length)
  //   //     val pad4 = " "*(3 - clustId.toString.length)

  //   //     val mentionStr = s"""[${mentionId},$pad3 ${clustId},$pad4 "${labelStr}",${pad1} "${jtextFlow.toText()}",${pad2} [${trs}]]""".box

  //   //     (0, 0, mentionStr)
  //   //     (mentionId, clustId, mentionStr)
  //   //   })
  //   //   .toSeq
  //   //   .sortBy({x => (x._1, x._2) })

  //   // vjoinTrailSep(left, ",")(zones.map(_._3):_*)
  //   ???
  // }
