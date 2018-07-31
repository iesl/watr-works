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

  val ReferenceBlock          = Label.auto
  val PartialRefBegin         = Label.auto
  val PartialRefEnd           = Label.auto
  val Reference               = Label.auto
  val LeftHangingIndentColumn = Label.auto
  val LeftJustifiedColumn     = Label.auto
  val AlphabeticRefMarker     = Label.auto
  val NumericRefMarker        = Label.auto


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

          zonesByPage.map { case (stablePage, pageZonesAndLabels) =>

            val labelsOnPage = pageZonesAndLabels.map(_._2)

            val refBlockZones = pageZonesAndLabels.filter(_._2 == ReferenceBlock).map(_._1)

            // Query for LeftHangingIndentColumn blocks
            //   if nonEmpty: Build reference bounds pairwise between hanging indents (+ last wrt RefBlock bottom)

            // Build pairwise line-space histogram, try to draw reference separator lines base on those
            //   if histogram has enough variation to make 2 clean bins (inter/intra reference), build ref-bounds between separator lines

            // Query for labeled refs, see if they match the inferred versions

            // Query for partials, and delete any inferred refs that they cover

          }
        }


      case _ =>
    }
    markedOutput
  }

  def convertAllRefLabels(n: Int=Int.MaxValue, skip: Int=0, regexFilter: Option[String])(implicit corpusAccessApi: CorpusAccessApi): Unit = {
    val conf = TextWorksConfig.Config(IOConfig(
      inputMode = Some(InputMode.CorpusFile(corpusAccessApi.corpus.corpusRoot.toNIO)),
      outputPath= None,
      overwrite = true
    ))

    val processStream = createInputStream[IO](conf.ioConfig)
      .drop(skip.toLong).take(n.toLong)
      .through(initMarkedInput())
      .through(filterInputMatchRegex(regexFilter))
      .through(runSegmentation(conf))
      .through(convertReferenceBlockLabelsToReferenceLabels())
      .through(cleanTraceLogArtifacts(conf))
      .through(writeTraceLogs(conf))

    processStream.compile.drain
      .unsafeRunSync()

  }




}
