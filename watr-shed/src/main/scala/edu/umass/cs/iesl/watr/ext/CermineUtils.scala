package edu.umass.cs.iesl.watr
package ext

// import scala.collection.mutable
// import net.sf.jsi.Rectangle

import watrmarks._
import play.api.libs.json

object CermineUtils extends SpatialJsonFormat {
  import json._
  // import play.api.libs.functional.syntax._



  def loadSpatialIndices(jsvalue: JsValue): Seq[ZoneIndexer] = {

    val id = (jsvalue \ "id").validate[String]
    val target = (jsvalue \ "target").validate[String]
    val pageGeometry = (jsvalue \ "page-geometry" \ "bounds").validate[Seq[LBBounds]]
    val zones = (jsvalue \ "zones").validate[Seq[Zone]]

    ???
  }

  // def loadSpatialIndices(reader: Reader): Seq[ZoneIndexer] = {
  //   import javax.xml.stream.XMLInputFactory

  //   val factory = XMLInputFactory.newInstance();
  //   val events = factory.createXMLEventReader(reader);

  //   val pageBoundsById = mutable.ArrayBuffer[(String, Rectangle)]()
  //   val annotationsByTarget = mutable.HashMap[String, mutable.Seq[watrmarks.Annotation]]().withDefaultValue(mutable.Seq())



  //   while (events.hasNext()) {
  //     val event = events.nextEvent();

  //     event match {
  //       case elem: StartElement =>
  //         val classAttr = elem.getClassAttr

  //         elem.getName.getLocalPart.toLowerCase match {
  //           case "g" if classAttr == "annotation" =>
  //             val id = elem.getIdAttr
  //             val target = elem.getTargetAttr

  //             annotationsByTarget(target) :+ Annotation(id, target, Seq())


  //           case "g" if classAttr == "annotation-set" =>

  //           case "rect" if classAttr == "bounding-box" =>
  //             // val id = elem.getIdAttr

  //             pageBoundsById :+ new Rectangle(
  //               // id,
  //               // elem.getTargetAttr,
  //               elem.getAttr("x").toFloat,
  //               elem.getAttr("y").toFloat,
  //               elem.getAttr("width").toFloat,
  //               elem.getAttr("height").toFloat
  //             )

  //           case _ =>
  //         }

  //       case elem: EndElement =>
  //         //
  //     }
  //   }


  //   pageBoundsById.map { case (idstr, bb) =>
  //     // allocate a new spatial index
  //     // spatialindex.regions.bbox(
  //     val spInfo = ZoneIndexer(bb)
  //     // spInfos :+ (idstr, spInfo)
  //     val annotations = annotationsByTarget(idstr)

  //     annotations.foreach { a =>
  //       spInfo.addAnnotation(a)
  //     }
  //     spInfo
  //   }

  // }

}
