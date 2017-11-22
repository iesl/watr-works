package edu.umass.cs.iesl.watr
package table

import corpora._
import geometry._

import _root_.io.circe, circe._, circe.syntax._
import com.sksamuel.scrimage
import scrimage._
import scrimage.{canvas => SC}
import images.ImageManipulation

// import TypeTags._
import utils.ExactFloats._

trait LabeledPageImageWriter extends ImageManipulation {

  def exportDocuments()(implicit docStore: DocumentZoningApi): Unit = {
    import ammonite.{ops => fs}, fs._
    val exportedJs = docStore.getDocuments().flatMap(exportDocument(_))
    val allExports = exportedJs

    val jsOut = allExports.asJson.noSpaces

    fs.write(pwd / RelPath("zone-document-export"), jsOut)
  }


  // export one json record per zone
  def exportDocument(stableId: String@@DocumentID)(implicit docStore: DocumentZoningApi): Option[Json]= {
    val documentZones = for {
      docId <- docStore.getDocument(stableId).headOption
    } yield {
      // docStore.getPageZones(pageId: Int <refinement> PageID, label: Label)

      val pageDefs = for {
        (pageId, pageNum) <- docStore.getPages(docId).zipWithIndex
      } yield {
        val pageGeometry = docStore.getPageGeometry(pageId)
        val LTBounds.Ints(left, top, width, height) = pageGeometry

        Json.obj(
          ("page" := pageNum),
          ("geometry" := Json.obj(("x" := left), ("y" := top), ("width" := width), ("height" := height)))
        )
      }

      val zonesJs = for {
        labelId <- docStore.getZoneLabelsForDocument(docId)
        zoneId <- docStore.getZonesForDocument(docId, labelId) if labelId.unwrap > 2
      } yield {
        val zone = docStore.getZone(zoneId)

        val regions = zone.regions.map{region =>
          val LTBounds.Ints(left, top, width, height) = region.bbox
          Json.obj(
            ("page" := region.page.pageNum.unwrap),
            ("x" := left),     ("y" := top),
            ("width" := width), ("height" := height)
          )
        }
        Json.obj(
          ("label" := zone.label.toString()),
          ("regions" := regions)
        )
      }

      if (zonesJs.nonEmpty) Some(
        Json.obj(
          ("document" := stableId.unwrap),
          ("pages" := pageDefs),
          ("zones" := zonesJs)
        )
      ) else None
    }

    val zonedPages = documentZones.flatten

    zonedPages
  }

  def writeLabeledPageImages()(implicit docStore: DocumentZoningApi): Unit = {
    import ammonite.{ops => fs}, fs._
    val rootDir = pwd / RelPath("labeled-image-output")

    for {
      stableId <- docStore.getDocuments().take(40)
      (pageNum, pageImage) <- createLabeledPageImages(stableId)
    } {
      val imageBytes = pageImage.bytes

      val docDir = rootDir / RelPath(stableId.unwrap)
      val pageImagePath = docDir / RelPath(s"page-${pageNum}.png")
      if (! fs.exists(docDir)) {
        mkdir(docDir)
      }

      fs.write(pageImagePath, imageBytes)
    }

  }


  def createLabeledPageImages(stableId: String@@DocumentID)(implicit docStore: DocumentZoningApi): Seq[(Int@@PageNum, Image)] = {
    val pageImages = for {
      docId <- docStore.getDocument(stableId).toList
      documentLabels = docStore.getZoneLabelsForDocument(docId).filter(_.unwrap > 2)
      pageId <- docStore.getPages(docId) if documentLabels.nonEmpty
    } yield {
      val pageDef = docStore.getPageDef(pageId).get
      val pageGeometry = docStore.getPageGeometry(pageId)
      val LTBounds.Ints(left, top, width, height) = pageGeometry

      println(s"createLabeledPageImages: ${stableId}: page ${pageDef.pagenum}")

      val imageGeometry = LTBounds.Ints(left, top, width+40, height+40)
      val labelsAndDrawables = for {
        labelId <- documentLabels
        label = docStore.getLabel(labelId)
        zone <- docStore.getPageZones(pageId, label)
      } yield {
        val embossings = for {
          region <- zone.regions
        } yield {
          val r = (labelId.unwrap * 10) % 255
          val g = (labelId.unwrap * 20) % 255
          val b = (labelId.unwrap * 30) % 255
          ltBoundsToDrawablesFilled(region.bbox, pageGeometry, imageGeometry, Color.apply(red=r , green=g, blue=b))
        }

        (zone.label, embossings.flatten)
      }

      if (labelsAndDrawables.nonEmpty) {
        println(s"   writing page ${pageDef.pagenum}: ")
        // Init page image:
        var labelOverlay = new SC.Canvas(Image.filled(imageGeometry.width.asInt(), imageGeometry.height.asInt(), Color.Transparent))
        labelsAndDrawables.foreach {case (label, drawables) =>
          println(s"      l: ${label.toString()}: ")
          labelOverlay = labelOverlay.draw(drawables)
        }
        Some(
          (pageDef.pagenum, labelOverlay.image)
        )
      } else None
    }

    pageImages.flatten
  }


}
