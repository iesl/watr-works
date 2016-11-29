package edu.umass.cs.iesl.watr
package formats


import TypeTags._


import extract.PdfTextExtractor
import extract.fonts._

import spindex._
import EnrichGeometricFigures._

import textboxing.{TextBoxing => TB}, TB._

import utils.IdGenerator

import watrmarks._

import predsynth._

import ammonite.{ops => fs}, fs._
import scala.util.{Try}


import watrmarks.{StandardLabels => LB}
object DocumentIO extends DocsegJsonFormats {
  // import play.api.libs.json._
  // import textreflow.TextReflow
  import textreflow.TextReflowRendering._
  // import TextReflow._
  // import play.api.libs.functional.syntax._

  def serializeTargetRegion(tr: TargetRegion): TB.Box = {
    s"[${tr.target}, ${tr.bbox.compactPrint}]]"
  }

  def serializeZone(zone: Zone): TB.Box = {
    val trs = zone.regions.map(serializeTargetRegion(_)).mkString(",")
    s"[${zone.id}, ${zone.label}, [${trs}]]"
  }


  def richTextSerializeDocument(zoneIndexer: ZoneIndexer): String = {
    for {
      pageId <- zoneIndexer.getPages
      pageTextBlocks <- zoneIndexer.getPageIndex(pageId).getComponentsWithLabel(LB.PageTextBlocks)
      _ = println(s"Page $pageId")
      textBlock <- pageTextBlocks.getChildren(LB.TextBlock)
    }  {
      val blockText = zoneIndexer.getTextReflow(textBlock.id)

      blockText.foreach{bt =>
        println("Block Text")
        println(bt.toFormattedText())
      }

    }

    // val mentionBlock = serializeMentions(zoneIndexer)

    // val lineLabelBlock = serializeLineLabels(zoneIndexer)

    // val (lineTextBlock, lineDefBlock) = serializeTextLines(zoneIndexer)

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

    ???
  }

  def selectPinForLabel(lb: Label, n: BioNode): BioPin = {
    n.pins
      .filter(p => p.label==lb)
      .head
  }

  def serializeMentions(zoneIndexer: ZoneIndexer): TB.Box = {

    // val lines = for {
    //   linec <- lineBioLabels
    //   line = linec.component
    //   textFlow <- line.getTextReflow
    // } yield (textFlow, line.id, line.targetRegion)

    // val zones = zoneIndexer
    //   .getZones.map({ zone =>

    //     val linePerZTR = zone.regions.map({zoneTargetRegion =>
    //       lines.map(_._1).zipWithIndex
    //         .map({case (textFlow, lineNum) =>
    //           textFlow.clipToTargetRegion(zoneTargetRegion)
    //             .map(t => (t, lineNum))
    //         })
    //         .flatten.headOption
    //         .map({ case ((textFlow, begin, len), lineNum)  =>
    //           (s"""[${lineNum}, ${begin}, ${len}]""", textFlow)
    //         })

    //     }).flatten

    //     val trs = linePerZTR.map(_._1).mkString(",")
    //     val ts = linePerZTR.map(_._2)
    //     val jtextFlow = TextReflow.joins(" ")(ts)
    //     val labelStr = zone.label.value.get
    //     val pad1 = " "*(20-labelStr.length())
    //     val pad2 = " "*(20 - jtextFlow.toText.length())

    //     val mentionId = zone.id.unwrap


    //     val clustId = zoneIndexer.relations.collect({
    //       case Relation.Record(clusterId, "hasMember", e2) if Identities.idValue(e2) == mentionId =>
    //         Identities.idValue(clusterId)
    //     }).headOption.getOrElse(0)


    //     val pad3 = " "*(3 - mentionId.toString.length)
    //     val pad4 = " "*(3 - clustId.toString.length)

    //     val mentionStr = s"""[${mentionId},$pad3 ${clustId},$pad4 "${labelStr}",${pad1} "${jtextFlow.toText()}",${pad2} [${trs}]]""".box

    //     (0, 0, mentionStr)
    //     (mentionId, clustId, mentionStr)
    //   })
    //   .toSeq
    //   .sortBy({x => (x._1, x._2) })

    // vjoinTrailSep(left, ",")(zones.map(_._3):_*)
    ???
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


  def serializeTextLines(zoneIndexer: ZoneIndexer): (TB.Box, TB.Box) = {

    // val lines = for {
    //   linec <- lineBioLabels
    //   line = linec.component
    //   textFlow <- VisualLine.toTextReflow(line)
    // } yield (textFlow, line.id, line.targetRegion)
    // val lineTextAndIds = for {
    //   (lineText, lineId, _) <- lines
    // } yield {
    //   val charIds = (for {
    //     funit0 <- lineText.flow
    //     funit <- (0 until funit0.length).map(_ => funit0)
    //   } yield funit match {
    //     case u: FlowUnit.Atom => u.atomicComponent.id.unwrap
    //     case u: FlowUnit.Rewrite => u.atom.atomicComponent.id.unwrap
    //     case u: FlowUnit.Insert => 0
    //   }).mkString("[", ",", "]")

    //   val text = BX.bracket('"', '"', lineText.text.box)
    //   (text, charIds.box)
    // }

    // val joinedLineText =  vjoinTrailSep(left, ",")(lineTextAndIds.map(_._1):_*)
    // val joinedLineCharIds =  vjoinTrailSep(left, ",")(lineTextAndIds.map(_._2):_*)
    // (joinedLineText, joinedLineCharIds)
    ???
  }

  def serializePageAtoms(zoneIndexer: ZoneIndexer): TB.Box = {
    // val allCharIds = for {
    //   (lineText, lineId, _) <- lines
    //   cc <- lineText.flow.collect({
    //     case u: FlowUnit.Atom     => u.atomicComponent
    //     case u: FlowUnit.Rewrite  => u.atom.atomicComponent
    //   })
    // } yield {
    //   val tr = serializeTargetRegion(cc.targetRegion)
    //   s"[${cc.id}, ${tr}]".box
    // }


    val idBlock = for {
      pageId <-zoneIndexer.getPages
    } yield {
      val pageInfo = zoneIndexer.getPageIndex(pageId)

      pageInfo.getPageAtoms.toSeq
        .sortBy(_.id.unwrap)
        .map({case pageAtom =>
          val id = pageAtom.id
          val pageId = pageAtom.pageId
          val bbox = pageAtom.bounds
          s"[${id},[${pageId}, ${bbox.lowLeftCornerPrint}]]".box
        })
    }

    val tokenDict = idBlock.flatten
      .grouped(10)
      .map(group => hjoin(sep=",")(group:_*))
      .toList

    vjoinTrailSep(left, ",")(tokenDict:_*)
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
