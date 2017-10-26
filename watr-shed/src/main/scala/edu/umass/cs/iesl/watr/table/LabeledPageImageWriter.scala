package edu.umass.cs.iesl.watr
package table

import corpora._
import geometry._

import play.api.libs.json, json._
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
        val allExports = Json.arr(exportedJs)

        val jsOut = Json.prettyPrint(allExports)
        fs.write(pwd / RelPath("zone-document-export"), jsOut)
    }


    // export one json record per zone
    def exportDocument(stableId: String @@ DocumentID)(implicit docStore: DocumentZoningApi): Option[JsObject] = {
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
                    ("page" -> pageNum),
                    ("geometry" -> Json.obj(("x" -> left), ("y" -> top), ("width" -> width), ("height" -> height)))
                )
            }

            val zonesJs = for {
                labelId <- docStore.getZoneLabelsForDocument(docId)
                zoneId <- docStore.getZonesForDocument(docId, labelId) if labelId.unwrap > 2
            } yield {
                val zone = docStore.getZone(zoneId)

                val regions = zone.regions.map { region =>
                    val LTBounds.Ints(left, top, width, height) = region.bbox
                    Json.obj(
                        ("page" -> region.page.pageNum.unwrap),
                        ("x" -> left), ("y" -> top),
                        ("width" -> width), ("height" -> height)
                    )
                }
                Json.obj(
                    ("label" -> zone.label.toString()),
                    ("regions", regions)
                )
            }

            if (zonesJs.nonEmpty) Some(
                Json.obj(
                    ("document", stableId.unwrap),
                    ("pages", pageDefs),
                    ("zones", zonesJs)
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
            stableId <- docStore.getDocuments().drop(97).take(10000)
            (pageNum, pageImage) <- createLabeledPageImages(stableId)
        } {
            val imageBytes = pageImage.bytes

            val docDir = rootDir / RelPath(stableId.unwrap)
            val pageImagePath = docDir / RelPath(s"page-${pageNum}.png")
            if (!fs.exists(docDir)) {
                mkdir(docDir)
            }

            fs.write(pageImagePath, imageBytes)
        }

    }


    def createLabeledPageImages(stableId: String @@ DocumentID)(implicit docStore: DocumentZoningApi): Seq[(Int @@ PageNum, Image)] = {


        val colorScheme = Map(
            4 -> Seq(135, 206, 250),
            5 -> Seq(173, 255, 47),
            6 -> Seq(255, 215, 0),
            7 -> Seq(240, 128, 128))
        val acceptedLabels = Seq(4, 5, 6, 7)

        val pageImages = for {
            docId <- docStore.getDocument(stableId).toList
            documentLabels = docStore.getZoneLabelsForDocument(docId).filter(labelIdElement => acceptedLabels.contains(labelIdElement.unwrap))
            pageId <- docStore.getPages(docId) if documentLabels.nonEmpty
        } yield {
            val pageDef = docStore.getPageDef(pageId).get
            val pageGeometry = docStore.getPageGeometry(pageId)
            val LTBounds.Ints(left, top, width, height) = pageGeometry

            println(s"createLabeledPageImages: ${stableId}: page ${pageDef.pagenum}")

            val imageGeometry = LTBounds.Ints(left, top, width, height)
            val labelsAndDrawables = for {
                labelId <- documentLabels
                label = docStore.getLabel(labelId)
                zone <- docStore.getPageZones(pageId, label)
            } yield {

                val embossings = for {
                    region <- zone.regions
                } yield {
                    val r = colorScheme(labelId.unwrap).head
                    val g = colorScheme(labelId.unwrap)(1)
                    val b = colorScheme(labelId.unwrap)(2)
                    ltBoundsToDrawablesFilled(region.bbox, pageGeometry, imageGeometry, Color.apply(red = r, green = g, blue = b))

                }

                (zone.label, embossings.flatten)
            }

            if (labelsAndDrawables.nonEmpty) {
                println(s"   writing page ${pageDef.pagenum}: ")
                // Init page image:
                var labelOverlay = new SC.Canvas(Image.filled(imageGeometry.width.asInt(), imageGeometry.height.asInt(), Color.Transparent))
                labelsAndDrawables.foreach { case (label, drawables) =>
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
