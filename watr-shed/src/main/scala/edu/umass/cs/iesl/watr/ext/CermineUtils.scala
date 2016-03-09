package edu.umass.cs.iesl.watr
package ext

import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.ExtractionUtils
// import pl.edu.icm.cermine.content.transformers.BxContentStructToDocContentStructConverter
// import pl.edu.icm.cermine.exception.TransformationException
// import pl.edu.icm.cermine.content.model._
// import pl.edu.icm.cermine.structure.model._
// import pl.edu.icm.cermine.structure.tools.BxBoundsBuilder
// import scala.collection.JavaConversions._

import scala.collection.mutable
// import javax.xml.namespace._
import javax.xml.stream.events._

import watrmarks._
// import watrmarks.dom._


object CermineBoundingBoxes {
  import JavaxXmlUtils._
  import java.io.Reader

  def loadSpatialIndices(reader: Reader): Seq[PageSpatialInfo] = {
    import javax.xml.stream.XMLInputFactory
    // import JavaxXmlUtils._
    // import scala.collection.JavaConversions._
    // val spi = SpatialPageInfo()


    val factory = XMLInputFactory.newInstance();
    val events = factory.createXMLEventReader(reader);

    var currentTargetID = ""

    val infos = mutable.HashMap[String, PageSpatialInfo]()


    while (events.hasNext()) {
      val event = events.nextEvent();

      event match {
        case elem: StartElement =>
          val classAttr = elem.getClassAttr

          elem.getName.getLocalPart.toLowerCase match {
            case "g" if classAttr == "annotation" =>

            case "g" if classAttr == "annotation-set" =>
              currentTargetID = elem.getTargetAttr

            case "rect" if classAttr == "bounding-box" =>
              val id = elem.getIdAttr
              // map file:sha/page# -> spatialInfo
              // infos.getOrElseUpdate("", PageSpatialInfo())

            case _ =>
          }

        case elem: EndElement =>
          //
      }
    }
    ???
  }

}

  // structuredDoc.asChunks().toList.foreach{ chunk =>
  //   debugReport(
  //     chunk.getFontNames,
  //     chunk.getX,
  //     chunk.getY,
  //     chunk.getArea,
  //     chunk.getWidth,
  //     chunk.getHeight,
  //     chunk.getBounds
  //   // chunk.getText


    // debugReport(
    //   // "chunk",
    //   // chunk.toText
    //   // chunk.childrenCount,
    //   // chunk.getFontName,
    //   // chunk.getFontNames,
    //   // chunk.getMostPopularFontName,
    //   // chunk.getArea,
    //   // chunk.getBounds,
    //   // chunk.getFirstChild,
    //   // chunk.getHeight,
    //   // chunk.getId,
    //   // chunk.getNext,
    //   // chunk.getNextId,
    //   // chunk.getParent,
    //   // chunk.getPrev,
    //   // chunk.getWidth,
    //   // chunk.getX,
    //   // chunk.getY,
    //   // chunk.hasChildren,
    //   // chunk.hasNext,
    //   // chunk.hasPrev
    // )


    // debugReport(
    //   // "word"
    //   // word.toText,
    //   // word.childrenCount
    //   // word.getArea,
    //   // word.getBounds,
    //   // word.getFirstChild,
    //   // word.getHeight,
    //   // word.getId,
    //   // word.getNext,
    //   // word.getNextId,
    //   // word.getParent,
    //   // word.getPrev,
    //   // word.getWidth,
    //   // word.getX,
    //   // word.getY,
    //   // word.hasChildren,
    //   // word.hasNext,
    //   // word.hasPrev
    // )

      // line.toText()
      // line.childrenCount,
      // line.getFontNames
      // line.getMostPopularFontName

      // s"Zone id ${zone.getId}",
      // zone.toText,
      // zone.getChunks,
      // zone.getFontNames,
      // printLabel(zone.getLabel)
      // zone.iterator,
      // zone.getArea,
      // zone.getBounds,
      // zone.getHeight,
      // zone.getNext,
      // zone.getNextId,
      // zone.getParent,
      // zone.getPrev,
      // zone.getWidth,
      // zone.getX,
      // zone.getY,
      // zone.hasChildren,
      // zone.hasNext,
      // zone.hasPrev


// 36 	GEN_METADATA        (BxZoneLabelCategory.CAT_GENERAL),
// 39 	GEN_BODY            (BxZoneLabelCategory.CAT_GENERAL),
// 42 	GEN_REFERENCES      (BxZoneLabelCategory.CAT_GENERAL),
// 45 	GEN_OTHER           (BxZoneLabelCategory.CAT_GENERAL),
// 51     MET_ABSTRACT        (BxZoneLabelCategory.CAT_METADATA),
// 54     MET_AFFILIATION     (BxZoneLabelCategory.CAT_METADATA),
// 57     MET_ACCESS_DATA     (BxZoneLabelCategory.CAT_METADATA),
// 60     MET_BIOGRAPHY       (BxZoneLabelCategory.CAT_METADATA),
// 63     MET_AUTHOR          (BxZoneLabelCategory.CAT_METADATA),
// 66     MET_BIB_INFO        (BxZoneLabelCategory.CAT_METADATA),
// 69     MET_CORRESPONDENCE  (BxZoneLabelCategory.CAT_METADATA),
// 72     MET_DATES           (BxZoneLabelCategory.CAT_METADATA),
// 75     MET_EDITOR          (BxZoneLabelCategory.CAT_METADATA),
// 78     MET_KEYWORDS        (BxZoneLabelCategory.CAT_METADATA),
// 81     MET_TITLE           (BxZoneLabelCategory.CAT_METADATA),
// 84     MET_TYPE            (BxZoneLabelCategory.CAT_METADATA),
// 87     MET_COPYRIGHT       (BxZoneLabelCategory.CAT_METADATA),
// 93     BODY_CONTENT        (BxZoneLabelCategory.CAT_BODY),
// 96     BODY_GLOSSARY       (BxZoneLabelCategory.CAT_BODY),
// 99     BODY_EQUATION       (BxZoneLabelCategory.CAT_BODY),
// 102     BODY_EQUATION_LABEL (BxZoneLabelCategory.CAT_BODY),
// 105     BODY_FIGURE         (BxZoneLabelCategory.CAT_BODY),
// 108     BODY_FIGURE_CAPTION (BxZoneLabelCategory.CAT_BODY),
// 111     BODY_HEADING        (BxZoneLabelCategory.CAT_BODY),
// 114     BODY_JUNK           (BxZoneLabelCategory.CAT_BODY),
// 117     BODY_TABLE          (BxZoneLabelCategory.CAT_BODY),
// 120     BODY_TABLE_CAPTION  (BxZoneLabelCategory.CAT_BODY),
// 123     BODY_ACKNOWLEDGMENT (BxZoneLabelCategory.CAT_BODY),
// 126     BODY_CONTRIBUTION   (BxZoneLabelCategory.CAT_BODY),
// 129     BODY_CONFLICT_STMT  (BxZoneLabelCategory.CAT_BODY),
// 132     BODY_ATTACHMENT 	(BxZoneLabelCategory.CAT_BODY),
// 138     OTH_PAGE_NUMBER     (BxZoneLabelCategory.CAT_OTHER),
// 141     OTH_UNKNOWN         (BxZoneLabelCategory.CAT_OTHER),
// 147     REFERENCES          (BxZoneLabelCategory.CAT_REFERENCES),
// 150     MET_TITLE_AUTHOR    (BxZoneLabelCategory.CAT_METADATA),
// 153     MET_CATEGORY        (BxZoneLabelCategory.CAT_METADATA),
// 156     MET_TERMS           (BxZoneLabelCategory.CAT_METADATA);


