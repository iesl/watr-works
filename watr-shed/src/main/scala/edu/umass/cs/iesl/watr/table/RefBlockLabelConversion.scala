package edu.umass.cs.iesl.watr
package table

import scalaz.{@@ => _, _} // , Scalaz._

import TypeTags._
import corpora._
import utils.DoOrDieHandlers._
import _root_.io.circe, circe._
import circe.syntax._
import textgrid._
import textgraph._
import annots._
import geometry._

import LabeledSequenceTreeTransforms._
import com.github.tototoshi.csv._
import ammonite.{ops => fs} // , fs._

object RefBlockLabelConversion {

  import watrmarks._
  import apps._
  import apps.ProcessPipelineSteps._
  import cats.effect._
  import segment.ReferenceBlockConversion._

  def filterInputHasLabel(label: Option[Label])(implicit corpusAccessApi: CorpusAccessApi): fs2.Pipe[IO, MarkedInput, MarkedInput] = {
    _.map {
      case r@ Right(p@ Processable.CorpusFile(corpusEntry)) if label.isDefined =>
        val annotApi = corpusAccessApi.annotApi
        val docStore = corpusAccessApi.docStore
        val stableId = DocumentID(corpusEntry.entryDescriptor)
        val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")
        val docLabels = annotApi.listDocumentAnnotations(docId).map { id =>
          annotApi.getAnnotationRecord(id).label
        }

        if (docLabels.exists(_ == label.get)) {
          r
        } else Left(p)

      case x => x
    }
  }



  def convertReferenceBlockLabelsToReferenceLabels()(implicit corpusAccessApi: CorpusAccessApi): fs2.Pipe[IO, MarkedOutput, MarkedOutput] = _.map {
    markedOutput => markedOutput match {
      case Right(Processable.ExtractedFile(segmentation, Processable.CorpusFile(corpusEntry))) =>
        val annotApi = corpusAccessApi.annotApi
        val docStore = corpusAccessApi.docStore
        val stableId = DocumentID(corpusEntry.entryDescriptor)

        println(s"Converting ${stableId} reference labels...")


        val docId = docStore.getDocument(stableId).orDie(s"no document for ${stableId}")

        val docAnnots = annotApi.listDocumentAnnotations(docId)
          .map { id =>
            val rec = annotApi.getAnnotationRecord(id)
            (rec.location, rec.label)
          }
          .collect{ case (z: AnnotatedLocation.Zone, l: watrmarks.Label) => (z, l) }

        if (docAnnots.isEmpty) {
          println(s"No annotation in ${stableId}")
        } else {
          val zonesByPage = docAnnots.groupBy { case (zone, label) =>
            val region = zone.regions.headOption.orDie(s"no regions found in zone ${zone}")
            region.page
          }

          zonesByPage.foreach { case (stablePage, pageZonesAndLabels) =>
            val refBlockZones = pageZonesAndLabels.filter(_._2 == ReferenceBlock).map(_._1)

            if (refBlockZones.nonEmpty) {
              val pageNum = stablePage.pageNum
              println(s"Found ReferenceBlock on page ${pageNum}")
              val pageSegmenter = segmentation.pageSegmenters.find(_.pageNum == pageNum).headOption.orDie(s"no segmenter found for page ${pageNum}")
              val maybeRefs = pageSegmenter.convertReferenceBlocks(pageZonesAndLabels)
              maybeRefs.foreach { refBounds =>
                refBounds.foreach{ case (bbox, textGrid) =>

                  val zone = AnnotatedLocation.Zone(List(
                    PageRegion(stablePage, bbox)
                  ))
                  val annotId = annotApi.createAnnotation(Reference, zone)
                  annotApi.setCorpusPath(annotId, CorpusPath("References.AutoLabeled"))
                  val gridAsJson = textGrid.toJson().noSpaces
                  val body = AnnotationBody.TextGrid(gridAsJson)
                  annotApi.updateBody(annotId, body)
                }
              }
            }
          }
        }


      case _ =>
    }
    markedOutput
  }

  def convertAllRefLabels(n: Int=Int.MaxValue, skip: Int=0, regexFilter: Option[String], labelFilter: Option[String])(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val conf = TextWorksConfig.Config(IOConfig(
      inputMode = Some(InputMode.CorpusFile(corpusAccessApi.corpus.corpusRoot.toNIO)),
      outputPath= None,
      overwrite = true
    ))

    val processStream = createInputStream[IO](conf.ioConfig)
      .drop(skip.toLong).take(n.toLong)
      .through(initMarkedInput())
      .through(filterInputMatchRegex(regexFilter))
      .through(filterInputHasLabel(labelFilter.map(s => Label(s))))
      .through(runSegmentation(conf))
      .through(convertReferenceBlockLabelsToReferenceLabels())
      .through(cleanTraceLogArtifacts(conf))
      .through(writeTraceLogs(conf))

    processStream.compile.drain
      .unsafeRunSync()

  }

}
