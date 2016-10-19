package edu.umass.cs.iesl.watr
package formats

import java.io.InputStream

import TypeTags._

import extract.PdfTextExtractor
import extract.fonts._

import spindex._
import spindex.ComponentOperations._
import spindex.ComponentRendering._
import spindex.EnrichGeometricFigures._

import textboxing.{TextBoxing => TB}, TB._

import textflow.{TextFlow, FlowUnit}
import textflow.TextFlowRendering.BX

import utils.EnrichNumerics._
import utils.IdGenerator

import watrmarks.BioLabeling._
import watrmarks.{StandardLabels => LB, _}

import predsynth._


object DocumentIO {

  def serializeTargetRegion(tr: TargetRegion): TB.Box = {
    s"[${tr.target}, ${tr.bbox.compactPrint}]]"
  }

  // def serializeTargetRegionLowLeftCorner(tr: TargetRegion): TB.Box = {
  //   s"[${tr.target}, ${tr.bbox.lowLeftCornerPrint}]]"
  // }

  def serializeZone(zone: Zone): TB.Box = {
    val trs = zone.regions.map(serializeTargetRegion(_)).mkString(",")
    /// Zone = [line#, charBegin, #chars]
    s"[${zone.id}, ${zone.label}, [${trs}]]"
  }

  def richTextSerializeDocument(zoneIndexer: ZoneIndexer): String = {

    val lineBioLabels = zoneIndexer.bioLabeling("LineBioLabels")

    val serComponents = List(
      LB.SectionHeadingLine,
      LB.ParaBegin,
      LB.TextBlock,
      LB.Abstract
    ).map(serializeLabeling(_, lineBioLabels))

    val lines = for {
      linec <- lineBioLabels
      line = linec.component
      textFlow <- VisualLine.render(line)
    } yield (textFlow, line.id, line.targetRegion)

    val joinedLabels =  vjoinTrailSep(left, ",")(serComponents.flatten:_*)


    // serialize Zones
    // map zone to line#
    val zones = zoneIndexer
      .getZones.map({ zone =>

        val linePerZTR = zone.regions.map({zoneTargetRegion =>
          lines.map(_._1).zipWithIndex
            .map({case (textFlow, lineNum) =>
              TextFlow.clipToTargetRegion(textFlow, zoneTargetRegion)
                .map(t => (t, lineNum))
            })
            .flatten.headOption
            .map({ case ((textFlow, begin, len), lineNum)  =>
              (s"""[${lineNum}, ${begin}, ${len}]""", textFlow)
            })

        }).flatten

        val trs = linePerZTR.map(_._1).mkString(",")
        val ts = linePerZTR.map(_._2)
        val jtextFlow = TextFlow.joins(" ")(ts)
        val labelStr = zone.label.value.get
        val pad1 = " "*(20-labelStr.length())
        val pad2 = " "*(20 - jtextFlow.text.length())

        val mentionId = MentionID(zone.id.unwrap)

        val clustId = zoneIndexer.relations.collect({
          case Relation.Record(id,
            Relation.Elem.Cluster(clusterId),
            "hasMember", Relation.Elem.Mention(`mentionId`)) =>
            clusterId
        }).headOption.getOrElse(ClusterID(0))


        val pad3 = " "*(3 - mentionId.toString.length)
        val pad4 = " "*(3 - clustId.toString.length)

        val mentionStr = s"""[${mentionId},$pad3 ${clustId},$pad4 "${labelStr}",${pad1} "${jtextFlow.text}",${pad2} [${trs}]]""".box

        (0, 0, mentionStr)
        (mentionId.unwrap, clustId.unwrap, mentionStr)
      })
      .toSeq
      .sortBy({x => (x._1, x._2) })


    val zoneBlock =  vjoinTrailSep(left, ",")(zones.map(_._3):_*)

    val allCharIds = for {
      (lineText, lineId, _) <- lines
      cc <- lineText.flow.collect({
        case u: FlowUnit.Atom     => u.atomicComponent
        case u: FlowUnit.Rewrite  => u.atom.atomicComponent
      })
    } yield {
      val tr = serializeTargetRegion(cc.targetRegion)
      s"[${cc.id}, ${tr}]".box
    }

    val lineTextAndIds = for {
      (lineText, lineId, _) <- lines
    } yield {
      val charIds = (for {
        funit0 <- lineText.flow
        funit <- (0 until funit0.length).map(_ => funit0)
      } yield funit match {
        case u: FlowUnit.Atom => u.atomicComponent.id.unwrap
        case u: FlowUnit.Rewrite => u.atom.atomicComponent.id.unwrap
        case u: FlowUnit.Insert => 0
      }).mkString("[", ",", "]")

      val text = BX.bracket('"', '"', lineText.text.box)
      (text, charIds.box)
    }

    val joinedLineText =  vjoinTrailSep(left, ",")(lineTextAndIds.map(_._1):_*)
    val joinedLineCharIds =  vjoinTrailSep(left, ",")(lineTextAndIds.map(_._2):_*)


    val tokenDict = allCharIds
      .grouped(10)
      .map(group => hjoin(sep=",")(group:_*))
      .toList

    val tokenBlock = vjoinTrailSep(left, ",")(tokenDict:_*)

    // output all relationships:
    // rawTextMentionsById
    val relations = zoneIndexer.relations
      .filter({
        case Relation.Record(_, _, rel, _)
            if rel=="hasType" || rel== "hasMember" =>  false
        case _ => true
      })
      .map({relation =>
        val id = relation.id
        val lhs = Relation.formatElem(relation.lhs)
        val rhs = Relation.formatElem(relation.rhs)
        val rel = relation.relationship
        val pad1 = " "*(15-lhs.length())
        val pad2 = " "*(12-rel.length())
        s"""[$lhs, $pad1 "$rel", $pad2 $rhs]""".box
      })

    val relationBlock = vjoinTrailSep(left, ",")(relations:_*)


    val props =
      vjoinTrailSep(left, ",")(
        zoneIndexer.props.map({ prop =>
          Relation.formatPropRec(prop).box
        }):_*
      )

    (s"""|{ "lines": [
         |${indent(4)(joinedLineText)}
         |  ],
         |  "__comments": [
         |    "mention entry format:",
         |      "[mentionId, clusterId, mentionType, mentionText, textPosition]",
         |    "textPostion format:",
         |      "[lineNumber, mentionStart, mentionLength]",
         |      "where start is relative to lineNumber, and mentionLength can span consecutive lines"
         |    ],
         |  "mentions": [
         |${indent(4)(zoneBlock)}
         |  ],
         |  "relations": [
         |${indent(4)(relationBlock)}
         |  ],
         |  "properties": [
         |${indent(4)(props)}
         |  ],
         |  "labels": [
         |${indent(4)(joinedLabels)}
         |  ],
         |  "lineDefs": [
         |${indent(4)(joinedLineCharIds)}
         |  ],
         |  "ids": [
         |${indent(4)(tokenBlock)}
         |  ] }
         |""".stripMargin)

  }

  def selectPinForLabel(lb: Label, n: BioNode): BioPin = {
    n.pins
      .filter(p => p.label==lb)
      .head
  }

  def serializeLabeling(label: Label, bioLabeling: Seq[BioNode]): Seq[Box] = {
    val labeledSpans = selectBioLabelings(label, bioLabeling)

    val spanBoxes = for {
      span <- labeledSpans
    } yield {

      val bioSpan = span
        .map(p => selectPinForLabel(label, p))

      val spanId = bioSpan.head.id
      val compIds = span.map(_.component.id)

      val cids = compIds.mkString(",")

      s"""["${label}", [${cids}], ${spanId}]""".box
    }

    spanBoxes
  }


  def serializeDocument(zoneIndexer: ZoneIndexer): String = {

    val lineBioLabels = zoneIndexer.bioLabeling("LineBioLabels")

    val serComponents = List(
      LB.SectionHeadingLine,
      LB.ParaBegin,
      LB.TextBlock,
      LB.Abstract
    ).map(l =>
      serializeLabeling(l, lineBioLabels)
    )

    val lines = for {
      linec <- lineBioLabels
      line = linec.component
    } yield {
      VisualLine.renderWithIDs(line)
    }

    val joinedLines =  vjoinTrailSep(left, ",")(lines:_*)
    val joinedLabels =  vjoinTrailSep(left, ",")(serComponents.flatten:_*)

    val idBlock = for {
      pageId <-zoneIndexer.getPages
    } yield {
      val pageInfo = zoneIndexer.getPageIndex(pageId)

      pageInfo.componentToLabels.toSeq
        .filter({case (k, v) =>
          v.exists({ l =>
            l==LB.VisualLine || l==LB.Token
          })
        })
        .sortBy({case (k, v) => k.unwrap})
        .map({case (cid, _) =>
          val comp = zoneIndexer.getComponent(cid, pageId)
          s"[${cid},[${pageId}, ${comp.bounds.compactPrint}]]".box
        })
    }

    val tokenDict = idBlock.flatten
      .grouped(10)
      .map(group => hjoin(sep=",")(group:_*))
      .toList

    val tokenBlock = indent()(vjoinTrailSep(left, ",")(tokenDict:_*))

    (s"""|{ "labels": [
         |${indent(4)(joinedLabels)}
         |  ],
         |  "lines": [
         |${indent(4)(joinedLines)}
         |  ],
         |  "ids": [
         |${indent()(tokenBlock)}
         |  ]}
         |""".stripMargin)

  }


  def charInfosBox(cbs: Seq[CharAtom]): Seq[TB.Box] = {

    cbs.zip(spaceWidths(cbs))
      .map{ case (c, dist) =>
        (tbox(c.char.toString) +| "->" +| (dist.pp)) %
          c.region.bbox.top.pp %
          (c.region.bbox.left.pp +| c.region.bbox.right.pp) %
          (c.region.bbox.bottom.pp +| "(w:" + c.region.bbox.width.pp + ")")
    }
  }


  def extractChars(
    pdfis: InputStream,
    charsToDebug: Set[Int] = Set(),
    glyphDefs: Seq[SplineFont.Dir] = Seq()
  ): Seq[(PageAtoms, PageGeometry)] = {


    val charExtractor = new PdfTextExtractor(
      charsToDebug,
      IdGenerator[RegionID](), //, IdGenerator[PageID]
      glyphDefs
    )
    val _ = charExtractor.extractCharacters(pdfis)

    val pageInfos = charExtractor.pagesInfo

    pageInfos.map({ case (pchars, pgeom) =>
      (pchars, pgeom)
    })

  }



  def modifyZoneLabelName(name: String): Label = {
    val Array(pre, post0) = name.toLowerCase.split("_", 2)
    val post = post0.replace("_", "-")

    Label("bx", s"${pre}:${post}")
  }


}
